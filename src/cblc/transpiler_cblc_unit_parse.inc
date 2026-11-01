static void cblc_resolve_local_function_calls(t_cblc_translation_unit *unit)
{
    size_t function_index;

    if (!unit)
        return ;
    function_index = 0;
    while (function_index < unit->function_count)
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < unit->functions[function_index].statement_count)
        {
            t_cblc_statement *statement;

            statement = &unit->functions[function_index].statements[statement_index];
            if ((statement->type == CBLC_STATEMENT_CALL
                    || statement->type == CBLC_STATEMENT_CALL_ASSIGN)
                && cblc_find_function_in_unit(unit, statement->call_identifier))
                statement->call_is_external = 0;
            statement_index += 1;
        }
        function_index += 1;
    }
}

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
    if (cblc_register_parameter_destructors(unit, function) != FT_SUCCESS)
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
    if (!function->saw_return
        && cblc_append_parameter_destructor_statements(unit, function)
            != FT_SUCCESS)
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
    char third[TRANSPILE_IDENTIFIER_MAX];
    char path[TRANSPILE_FILE_PATH_MAX];
    const char *lookahead;
    t_cblc_file_declaration *file;
    t_transpiler_file_organization organization;
    int first_is_organization;

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
    third[0] = '\0';
    organization = TRANSPILE_FILE_ORGANIZATION_LINE_SEQUENTIAL;
    first_is_organization = 0;
    if (std::strncmp(first, "indexed", sizeof(first)) == 0)
    {
        organization = TRANSPILE_FILE_ORGANIZATION_INDEXED;
        first_is_organization = 1;
    }
    else if (std::strncmp(first, "relative", sizeof(first)) == 0)
    {
        organization = TRANSPILE_FILE_ORGANIZATION_RELATIVE;
        first_is_organization = 1;
    }
    else if (std::strncmp(first, "sequential", sizeof(first)) == 0)
    {
        organization = TRANSPILE_FILE_ORGANIZATION_SEQUENTIAL;
        first_is_organization = 1;
    }
    lookahead = *cursor;
    if (cblc_parse_identifier(&lookahead, second, sizeof(second)) == FT_SUCCESS)
    {
        *cursor = lookahead;
        cblc_skip_whitespace(cursor);
    }
    if (!first_is_organization && second[0])
    {
        if (std::strncmp(second, "indexed", sizeof(second)) == 0)
        {
            organization = TRANSPILE_FILE_ORGANIZATION_INDEXED;
            lookahead = *cursor;
            if (cblc_parse_identifier(&lookahead, third, sizeof(third)) != FT_SUCCESS)
                return (FT_FAILURE);
            *cursor = lookahead;
            cblc_skip_whitespace(cursor);
        }
        else if (std::strncmp(second, "relative", sizeof(second)) == 0)
        {
            organization = TRANSPILE_FILE_ORGANIZATION_RELATIVE;
            lookahead = *cursor;
            if (cblc_parse_identifier(&lookahead, third, sizeof(third)) != FT_SUCCESS)
                return (FT_FAILURE);
            *cursor = lookahead;
            cblc_skip_whitespace(cursor);
        }
    }
    if (**cursor != '"' || cblc_parse_string_literal(cursor, path, sizeof(path)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_find_file(unit, third[0] ? third : (second[0] ? second : first)))
        return (FT_FAILURE);
    if (unit->file_count >= unit->file_capacity
        && cblc_translation_unit_ensure_file_capacity(unit,
            unit->file_capacity == 0 ? 4 : unit->file_capacity * 2) != FT_SUCCESS)
        return (FT_FAILURE);
    file = &unit->files[unit->file_count++];
    std::memset(file, 0, sizeof(*file));
    file->organization = organization;
    if (first_is_organization)
    {
        ft_strlcpy(file->source_name, second, sizeof(file->source_name));
        file->role = TRANSPILE_FILE_ROLE_DATA;
    }
    else if (third[0])
    {
        if (std::strncmp(first, "input", sizeof(first)) != 0
            && std::strncmp(first, "output", sizeof(first)) != 0
            && std::strncmp(first, "data", sizeof(first)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(file->source_name, third, sizeof(file->source_name));
        file->role = std::strncmp(first, "input", sizeof(first)) == 0
            ? TRANSPILE_FILE_ROLE_INPUT : (std::strncmp(first, "output", sizeof(first)) == 0
                ? TRANSPILE_FILE_ROLE_OUTPUT : TRANSPILE_FILE_ROLE_DATA);
    }
    else if (second[0])
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
    unit->source_file_id = 0;
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
    unit->template_max_instantiations =
        TRANSPILE_DEFAULT_MAX_TEMPLATE_INSTANTIATIONS;
    unit->template_max_instantiation_depth = CBLC_TEMPLATE_DEFAULT_MAX_DEPTH;
    unit->max_lifetime_edge_transfers =
        TRANSPILE_DEFAULT_MAX_LIFETIME_EDGE_TRANSFERS;
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
    unit->is_native_standard_library = 0;
    unit->program_name[0] = '\0';
    unit->active_scope_id = 0;
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
    unit->source_file_id = 0;
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
    unit->template_max_instantiations =
        TRANSPILE_DEFAULT_MAX_TEMPLATE_INSTANTIATIONS;
    unit->template_max_instantiation_depth = CBLC_TEMPLATE_DEFAULT_MAX_DEPTH;
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

static size_t cblc_source_file_id(const char *text)
{
    std::uint32_t hash;

    if (!text)
        return (0);
    hash = 2166136261u;
    while (*text != '\0')
    {
        hash ^= static_cast<unsigned char>(*text);
        hash *= 16777619u;
        text += 1;
    }
    if (hash == 0)
        hash = 1;
    return (static_cast<size_t>(hash));
}

static void cblc_scan_string_class_references(const char *text,
    int *out_uses_string, int *out_declares_string_class,
    int *out_uses_bad_alloc, int *out_declares_bad_alloc,
    int *out_uses_new, int *out_uses_overflow_error,
    int *out_uses_range_error)
{
    const char *cursor;
    int previous_was_class;

    if (!out_uses_string || !out_declares_string_class || !out_uses_bad_alloc
        || !out_declares_bad_alloc || !out_uses_new
        || !out_uses_overflow_error || !out_uses_range_error)
        return ;
    *out_uses_string = 0;
    *out_declares_string_class = 0;
    *out_uses_bad_alloc = 0;
    *out_declares_bad_alloc = 0;
    *out_uses_new = 0;
    *out_uses_overflow_error = 0;
    *out_uses_range_error = 0;
    if (!text)
        return ;
    cursor = text;
    previous_was_class = 0;
    while (*cursor != '\0')
    {
        if (std::isspace(static_cast<unsigned char>(*cursor)))
        {
            cursor += 1;
            continue ;
        }
        if (cursor[0] == '/' && cursor[1] == '/')
        {
            cursor += 2;
            while (*cursor != '\0' && *cursor != '\n')
                cursor += 1;
            continue ;
        }
        if (cursor[0] == '/' && cursor[1] == '*')
        {
            cursor += 2;
            while (*cursor != '\0' && !(cursor[0] == '*' && cursor[1] == '/'))
                cursor += 1;
            if (*cursor != '\0')
                cursor += 2;
            continue ;
        }
        if (*cursor == '"' || *cursor == '\'')
        {
            char quote;

            quote = *cursor++;
            previous_was_class = 0;
            while (*cursor != '\0' && *cursor != quote)
            {
                if (*cursor == '\\' && cursor[1] != '\0')
                    cursor += 1;
                cursor += 1;
            }
            if (*cursor != '\0')
                cursor += 1;
            continue ;
        }
        if (std::isalpha(static_cast<unsigned char>(*cursor)) || *cursor == '_')
        {
            const char *identifier_start;
            size_t identifier_length;
            int is_string;
            int is_class;
            int is_bad_alloc;
            int is_new;
            int is_overflow_error;
            int is_range_error;

            identifier_start = cursor;
            while (std::isalnum(static_cast<unsigned char>(*cursor)) || *cursor == '_')
                cursor += 1;
            identifier_length = static_cast<size_t>(cursor - identifier_start);
            is_string = identifier_length == std::strlen("string")
                && std::strncmp(identifier_start, "string", identifier_length) == 0;
            is_class = identifier_length == std::strlen("class")
                && std::strncmp(identifier_start, "class", identifier_length) == 0;
            is_bad_alloc = identifier_length == std::strlen("bad_alloc")
                && std::strncmp(identifier_start, "bad_alloc", identifier_length) == 0;
            is_new = identifier_length == std::strlen("new")
                && std::strncmp(identifier_start, "new", identifier_length) == 0;
            is_overflow_error = identifier_length == std::strlen("overflow_error")
                && std::strncmp(identifier_start, "overflow_error", identifier_length) == 0;
            is_range_error = identifier_length == std::strlen("range_error")
                && std::strncmp(identifier_start, "range_error", identifier_length) == 0;
            if (is_string)
            {
                *out_uses_string = 1;
                if (previous_was_class)
                    *out_declares_string_class = 1;
            }
            if (is_bad_alloc)
            {
                *out_uses_bad_alloc = 1;
                if (previous_was_class)
                    *out_declares_bad_alloc = 1;
            }
            if (is_new)
                *out_uses_new = 1;
            if (is_overflow_error)
                *out_uses_overflow_error = 1;
            if (is_range_error)
                *out_uses_range_error = 1;
            previous_was_class = is_class;
            continue ;
        }
        previous_was_class = 0;
        cursor += 1;
    }
}

static void cblc_mark_standard_library_types(t_cblc_translation_unit *unit,
    size_t first_type_index)
{
    if (!unit || first_type_index > unit->struct_type_count)
        return ;
    while (first_type_index < unit->struct_type_count)
    {
        unit->struct_types[first_type_index].is_standard_library = 1;
        first_type_index += 1;
    }
}

static int cblc_parse_translation_unit_impl(const char *text,
    t_cblc_translation_unit *unit)
{
    const char *cursor;
    const char *error_source;
    size_t error_source_length;
    size_t first_standard_error_type;
    size_t first_standard_string_type;
    int source_uses_string;
    int source_declares_string_class;
    int source_uses_bad_alloc;
    int source_declares_bad_alloc;
    int source_uses_new;
    int source_uses_overflow_error;
    int source_uses_range_error;
    int source_has_error_prefix;
    int skip_error_source_prefix;
    size_t parse_error_line;
    size_t parse_error_column;
    const char *diagnostic_cursor;

    if (!text || !unit)
        return (FT_FAILURE);
    cblc_scan_string_class_references(text, &source_uses_string,
        &source_declares_string_class, &source_uses_bad_alloc,
        &source_declares_bad_alloc, &source_uses_new,
        &source_uses_overflow_error, &source_uses_range_error);
    error_source = transpiler_standard_library_get_native_source("CBLC-ERRORS");
    if (!error_source)
        return (FT_FAILURE);
    error_source_length = std::strlen(error_source);
    first_standard_error_type = unit->struct_type_count;
    first_standard_string_type = unit->struct_type_count;
    source_has_error_prefix = std::strncmp(text, error_source,
        error_source_length) == 0;
    skip_error_source_prefix = 0;
    if (((source_uses_overflow_error
            && !cblc_find_struct_type(unit, "overflow_error"))
            || (source_uses_range_error
                && !cblc_find_struct_type(unit, "range_error"))
            || (source_uses_new && !cblc_find_struct_type(unit, "bad_alloc"))
            || (source_uses_bad_alloc && !source_declares_bad_alloc
                && !cblc_find_struct_type(unit, "bad_alloc")))
        && !source_has_error_prefix)
    {
        first_standard_error_type = unit->struct_type_count;
        if (cblc_parse_translation_unit(error_source, unit) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_mark_standard_library_types(unit, first_standard_error_type);
    }
    if (!cblc_find_struct_type(unit, "string")
        && source_uses_string && !source_declares_string_class)
    {
        const char *string_class_source;
        if (!cblc_find_struct_type(unit, "length_error"))
        {
            first_standard_error_type = unit->struct_type_count;
            if (cblc_parse_translation_unit(error_source, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_mark_standard_library_types(unit, first_standard_error_type);
        }
        string_class_source = transpiler_standard_library_get_native_source("CBLC-STRING");
        if (!string_class_source)
            return (FT_FAILURE);
        first_standard_string_type = unit->struct_type_count;
        if (cblc_parse_translation_unit(string_class_source, unit) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_mark_standard_library_types(unit, first_standard_string_type);
    }
    if (source_has_error_prefix && cblc_find_struct_type(unit, "length_error"))
        skip_error_source_prefix = 1;
    unit->source_text = text;
    unit->source_file_id = cblc_source_file_id(text);
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = NULL;
    cursor = skip_error_source_prefix ? text + error_source_length : text;
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
                if (unit->parse_error_message[0] == '\0')
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
            size_t const_instance_start;
            size_t const_instance_index;

            cursor -= std::strlen("const");
            if (!cblc_match_keyword(&cursor, "const"))
                return (FT_FAILURE);
            cblc_skip_whitespace(&cursor);
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
            const_instance_start = unit->data_count;
            if (cblc_parse_struct_instance_declaration(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            const_instance_index = const_instance_start;
            while (const_instance_index < unit->data_count)
            {
                unit->data_items[const_instance_index].is_const = 1;
                const_instance_index += 1;
            }
            continue ;
        }
        if (cblc_match_keyword(&cursor, "void"))
        {
            cursor -= std::strlen("void");
            if (cblc_parse_pointer_declaration(&cursor, unit, 0) != FT_SUCCESS)
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
        if (unit->parse_error_message[0] == '\0')
        {
            parse_error_line = 1;
            parse_error_column = 1;
            diagnostic_cursor = text;
            while (diagnostic_cursor < cursor && *diagnostic_cursor != '\0')
            {
                if (*diagnostic_cursor == '\n')
                {
                    parse_error_line += 1;
                    parse_error_column = 1;
                }
                else
                    parse_error_column += 1;
                diagnostic_cursor += 1;
            }
            std::snprintf(unit->parse_error_message,
                sizeof(unit->parse_error_message),
                "unable to parse CBL-C declaration at %zu:%zu near '%.80s'",
                parse_error_line, parse_error_column, cursor);
        }
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
    cblc_resolve_local_function_calls(unit);
    if (cblc_propagate_local_exception_summaries(unit) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit)
{
    size_t allocation_failures_before;
    int status;

    allocation_failures_before = cma_allocation_failure_count();
    status = cblc_parse_translation_unit_impl(text, unit);
    if (status != FT_SUCCESS && unit
        && cma_allocation_failure_count() != allocation_failures_before)
    {
        if (unit->parse_error_code == 0)
            unit->parse_error_code = TRANSPILE_ERROR_ALLOCATION_FAILURE;
        if (unit->parse_error_message[0] == '\0')
            std::snprintf(unit->parse_error_message,
                sizeof(unit->parse_error_message),
                "allocation failed while parsing CBL-C translation unit");
    }
    return (status);
}
