static int cblc_parse_function(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function *function;
    t_cblc_data_item *return_item;
    t_cblc_function_return_kind return_kind;
    char return_type_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;
    size_t parameter_scope_base;
    size_t saved_scope_id;
    size_t function_scope_id;

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
    function->is_template = 0;
    function->is_template_instantiation = 0;
    function->template_parameter_name[0] = '\0';
    function->template_source_name[0] = '\0';
    function->template_argument_name[0] = '\0';
    function->local_destructor_targets = NULL;
    function->local_destructor_count = 0;
    function->local_destructor_capacity = 0;
    function->source_name[0] = '\0';
    function->cobol_name[0] = '\0';
    ft_strlcpy(function->source_name, identifier, sizeof(function->source_name));
    cblc_identifier_to_cobol(identifier, function->cobol_name, sizeof(function->cobol_name));
    saved_scope_id = unit->active_scope_id;
    if (cblc_register_scope(unit, saved_scope_id, function->source_name,
            &function_scope_id) != FT_SUCCESS)
        return (FT_FAILURE);
    function->scope_id = function_scope_id;
    unit->active_scope_id = function_scope_id;
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
    if (!g_cblc_active_template_parameter
        && unit->entry_function_index == static_cast<size_t>(-1))
        unit->entry_function_index = index;
    if (!g_cblc_active_template_parameter
        && std::strncmp(identifier, "main", sizeof(identifier)) == 0)
        unit->entry_function_index = index;
    if (!g_cblc_active_template_parameter && unit->entry_function_index == index)
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
    if (cblc_translation_unit_ensure_function_capacity(unit, unit->function_count + 2)
        != FT_SUCCESS)
        return (FT_FAILURE);
    function = &unit->functions[index];
    if (cblc_parse_statement_block(cursor, unit, function, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    unit->active_scope_id = saved_scope_id;
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

static int cblc_translation_unit_ensure_file_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_file_declaration *files;

    if (!unit)
        return (FT_FAILURE);
    if (desired_capacity <= unit->file_capacity)
        return (FT_SUCCESS);
    files = static_cast<t_cblc_file_declaration *>(cma_calloc(desired_capacity,
            sizeof(*files)));
    if (!files)
        return (FT_FAILURE);
    if (unit->files)
    {
        std::memcpy(files, unit->files, unit->file_count * sizeof(*files));
        cma_free(unit->files);
    }
    unit->files = files;
    unit->file_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static t_cblc_file_declaration *cblc_find_file(t_cblc_translation_unit *unit,
    const char *source_name)
{
    size_t index;

    if (!unit || !source_name)
        return (NULL);
    index = 0;
    while (index < unit->file_count)
    {
        if (std::strncmp(unit->files[index].source_name, source_name,
                sizeof(unit->files[index].source_name)) == 0)
            return (&unit->files[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_parse_file_declaration(const char **cursor, t_cblc_translation_unit *unit)
{
    char first[TRANSPILE_IDENTIFIER_MAX];
    char second[TRANSPILE_IDENTIFIER_MAX];
    char path[TRANSPILE_FILE_PATH_MAX];
    const char *lookahead;
    t_cblc_file_declaration *file;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    lookahead = *cursor;
    if (!cblc_match_keyword(&lookahead, "file"))
        return (FT_FAILURE);
    *cursor = lookahead;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, first, sizeof(first)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    second[0] = '\0';
    lookahead = *cursor;
    if (cblc_parse_identifier(&lookahead, second, sizeof(second)) == FT_SUCCESS)
    {
        *cursor = lookahead;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != '"' || cblc_parse_string_literal(cursor, path, sizeof(path)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_find_file(unit, second[0] ? second : first))
        return (FT_FAILURE);
    if (unit->file_count >= unit->file_capacity
        && cblc_translation_unit_ensure_file_capacity(unit,
            unit->file_capacity == 0 ? 4 : unit->file_capacity * 2) != FT_SUCCESS)
        return (FT_FAILURE);
    file = &unit->files[unit->file_count++];
    std::memset(file, 0, sizeof(*file));
    if (second[0])
    {
        ft_strlcpy(file->source_name, second, sizeof(file->source_name));
        if (std::strncmp(first, "input", sizeof(first)) == 0)
            file->role = TRANSPILE_FILE_ROLE_INPUT;
        else if (std::strncmp(first, "output", sizeof(first)) == 0)
            file->role = TRANSPILE_FILE_ROLE_OUTPUT;
        else if (std::strncmp(first, "data", sizeof(first)) == 0)
            file->role = TRANSPILE_FILE_ROLE_DATA;
        else
            return (FT_FAILURE);
    }
    else
    {
        ft_strlcpy(file->source_name, first, sizeof(file->source_name));
        file->role = TRANSPILE_FILE_ROLE_DATA;
    }
    cblc_identifier_to_cobol(file->source_name, file->cobol_name,
        sizeof(file->cobol_name));
    ft_strlcpy(file->path, path, sizeof(file->path));
    return (FT_SUCCESS);
}

void cblc_translation_unit_init(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    unit->source_text = NULL;
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->scopes = static_cast<t_cblc_scope *>(cma_calloc(1, sizeof(t_cblc_scope)));
    unit->scope_count = unit->scopes ? 1 : 0;
    unit->scope_capacity = unit->scopes ? 1 : 0;
    if (unit->scopes)
    {
        unit->scopes[0].id = 0;
        unit->scopes[0].parent_id = static_cast<size_t>(-1);
        unit->scopes[0].depth = 0;
        unit->scopes[0].owner_function_name[0] = '\0';
    }
    unit->struct_types = NULL;
    unit->struct_type_count = 0;
    unit->struct_type_capacity = 0;
    unit->type_refs = NULL;
    unit->type_ref_count = 0;
    unit->type_ref_capacity = 0;
    unit->template_definitions = NULL;
    unit->template_definition_count = 0;
    unit->template_definition_capacity = 0;
    unit->template_instantiations = NULL;
    unit->template_instantiation_count = 0;
    unit->template_instantiation_capacity = 0;
    unit->function_instantiations = NULL;
    unit->function_instantiation_count = 0;
    unit->function_instantiation_capacity = 0;
    unit->parse_error_code = 0;
    unit->parse_error_message[0] = '\0';
    unit->template_error_name[0] = '\0';
    unit->template_error_arguments[0] = '\0';
    unit->template_error_definition_offset = static_cast<size_t>(-1);
    unit->template_error_use_offset = static_cast<size_t>(-1);
    unit->template_max_instantiations = 1024;
    unit->template_max_instantiation_depth = 32;
    unit->template_instantiation_depth = 0;
    unit->imports = NULL;
    unit->import_count = 0;
    unit->import_capacity = 0;
    unit->copy_includes = NULL;
    unit->copy_include_count = 0;
    unit->copy_include_capacity = 0;
    unit->files = NULL;
    unit->file_count = 0;
    unit->file_capacity = 0;
    unit->functions = NULL;
    unit->function_count = 0;
    unit->function_capacity = 0;
    unit->entry_function_index = static_cast<size_t>(-1);
    unit->helper_literal_counter = 0;
    unit->helper_status_index = -1;
    unit->program_name[0] = '\0';
    unit->active_scope_id = 0;
    cblc_register_builtin_string_type(unit);
}

void cblc_translation_unit_dispose(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    if (unit->data_items)
        cma_free(unit->data_items);
    if (unit->scopes)
        cma_free(unit->scopes);
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
    if (unit->type_refs)
        cma_free(unit->type_refs);
    if (unit->template_definitions)
        cma_free(unit->template_definitions);
    if (unit->template_instantiations)
        cma_free(unit->template_instantiations);
    if (unit->function_instantiations)
        cma_free(unit->function_instantiations);
    if (unit->imports)
        cma_free(unit->imports);
    if (unit->copy_includes)
        cma_free(unit->copy_includes);
    if (unit->files)
        cma_free(unit->files);
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
    unit->source_text = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->scopes = NULL;
    unit->scope_count = 0;
    unit->scope_capacity = 0;
    unit->struct_types = NULL;
    unit->struct_type_count = 0;
    unit->struct_type_capacity = 0;
    unit->type_refs = NULL;
    unit->type_ref_count = 0;
    unit->type_ref_capacity = 0;
    unit->template_definitions = NULL;
    unit->template_definition_count = 0;
    unit->template_definition_capacity = 0;
    unit->template_instantiations = NULL;
    unit->template_instantiation_count = 0;
    unit->template_instantiation_capacity = 0;
    unit->function_instantiations = NULL;
    unit->function_instantiation_count = 0;
    unit->function_instantiation_capacity = 0;
    unit->parse_error_code = 0;
    unit->parse_error_message[0] = '\0';
    unit->template_error_name[0] = '\0';
    unit->template_error_arguments[0] = '\0';
    unit->template_error_definition_offset = static_cast<size_t>(-1);
    unit->template_error_use_offset = static_cast<size_t>(-1);
    unit->template_max_instantiations = 1024;
    unit->template_max_instantiation_depth = 32;
    unit->template_instantiation_depth = 0;
    unit->imports = NULL;
    unit->import_count = 0;
    unit->import_capacity = 0;
    unit->copy_includes = NULL;
    unit->copy_include_count = 0;
    unit->copy_include_capacity = 0;
    unit->files = NULL;
    unit->file_count = 0;
    unit->file_capacity = 0;
    unit->functions = NULL;
    unit->function_count = 0;
    unit->function_capacity = 0;
    unit->entry_function_index = static_cast<size_t>(-1);
    unit->helper_literal_counter = 0;
    unit->helper_status_index = -1;
    unit->program_name[0] = '\0';
    unit->active_scope_id = 0;
}

int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit)
{
    const char *cursor;

    if (!text || !unit)
        return (FT_FAILURE);
    unit->source_text = text;
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
        if (cblc_match_keyword(&cursor, "file"))
        {
            cursor -= std::strlen("file");
            if (cblc_parse_file_declaration(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "template"))
        {
            cursor -= std::strlen("template");
            if (cblc_parse_template_struct_definition(&cursor, unit) != FT_SUCCESS
                && cblc_parse_template_function_definition(&cursor, unit) != FT_SUCCESS)
            {
                cblc_record_template_parse_error(unit,
                    TRANSPILE_ERROR_TEMPLATE_INVALID_DECLARATION,
                    "invalid template declaration");
                return (FT_FAILURE);
            }
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
