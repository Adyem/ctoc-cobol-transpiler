static int cblc_context_module_imports_module(const t_transpiler_context *context,
    const char *requesting_module, const char *provider_module)
{
    const t_transpiler_module *modules;
    const t_transpiler_module *requester;
    const t_transpiler_module *provider;
    size_t module_count;
    size_t index;

    if (!context || !requesting_module || !provider_module)
        return (0);
    if (std::strncmp(requesting_module, provider_module, TRANSPILE_MODULE_NAME_MAX) == 0)
        return (1);
    modules = transpiler_context_get_modules(context, &module_count);
    requester = NULL;
    provider = NULL;
    index = 0;
    while (index < module_count)
    {
        if (std::strncmp(modules[index].name, requesting_module, TRANSPILE_MODULE_NAME_MAX) == 0)
            requester = &modules[index];
        if (std::strncmp(modules[index].name, provider_module, TRANSPILE_MODULE_NAME_MAX) == 0)
            provider = &modules[index];
        index += 1;
    }
    if (!requester || !provider)
        return (0);
    index = 0;
    while (index < requester->import_count)
    {
        if (requester->imports[index].resolved_index != static_cast<size_t>(-1)
            && requester->imports[index].resolved_index < module_count
            && std::strncmp(modules[requester->imports[index].resolved_index].name,
                provider_module, TRANSPILE_MODULE_NAME_MAX) == 0)
            return (1);
        if (std::strncmp(requester->imports[index].path, provider->name,
                TRANSPILE_FILE_PATH_MAX) == 0
            || std::strncmp(requester->imports[index].path, provider->path,
                TRANSPILE_FILE_PATH_MAX) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

int cblc_import_translation_unit_type_stubs(t_transpiler_context *context, const char *module_name,
    t_cblc_translation_unit *unit)
{
    const t_transpiler_type_signature *types;
    size_t type_count;
    size_t index;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    types = transpiler_context_get_types(context, &type_count);
    index = 0;
    while (index < type_count)
    {
        if (std::strncmp(types[index].module, module_name, TRANSPILE_MODULE_NAME_MAX) != 0
            && types[index].visibility == TRANSPILE_SYMBOL_PUBLIC
            && cblc_context_module_imports_module(context, module_name, types[index].module))
        {
            if (cblc_register_imported_type_stub(unit, &types[index]) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int cblc_resolve_translation_unit_calls(t_transpiler_context *context, const char *module_name,
    t_cblc_translation_unit *unit)
{
    const t_transpiler_function_signature *functions;
    size_t function_count;
    size_t function_index;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    functions = transpiler_context_get_functions(context, &function_count);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        t_cblc_function *function;
        size_t statement_index;

        function = &unit->functions[function_index];
        statement_index = 0;
        while (statement_index < function->statement_count)
        {
            t_cblc_statement *statement;

            statement = &function->statements[statement_index];
            if (statement->type == CBLC_STATEMENT_CALL
                || statement->type == CBLC_STATEMENT_CALL_ASSIGN)
            {
                if (std::strncmp(statement->call_identifier, "std::malloc",
                        sizeof(statement->call_identifier)) == 0
                    || std::strncmp(statement->call_identifier, "std::realloc",
                        sizeof(statement->call_identifier)) == 0
                    || std::strncmp(statement->call_identifier, "std::free",
                        sizeof(statement->call_identifier)) == 0)
                {
                    statement->call_is_external = 0;
                    statement_index += 1;
                    continue ;
                }
                if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0)
                {
                    statement->call_is_external = 1;
                    statement_index += 1;
                    continue ;
                }
                const t_cblc_function *local_target;

                local_target = cblc_find_function_in_unit(unit, statement->call_identifier);
                if (local_target)
                {
                    size_t argument_index;

                    if (local_target->parameter_count != statement->call_argument_count)
                    {
                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        if (std::snprintf(message, sizeof(message),
                                "function '%s' expects %zu arguments but call provides %zu",
                                local_target->source_name, local_target->parameter_count,
                                statement->call_argument_count) >= 0)
                            transpiler_diagnostics_push(&context->diagnostics,
                                TRANSPILE_SEVERITY_ERROR,
                                TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH, message);
                        transpiler_context_record_error(context,
                            TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH);
                        return (FT_FAILURE);
                    }
                    argument_index = 0;
                    while (argument_index < statement->call_argument_count)
                    {
                        char argument[TRANSPILE_STATEMENT_TEXT_MAX];

                        if (cblc_extract_call_argument(statement, argument_index,
                                argument, sizeof(argument)) != FT_SUCCESS
                            || !cblc_argument_matches_parameter(unit, argument,
                                &local_target->parameters[argument_index]))
                        {
                            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            if (std::snprintf(message, sizeof(message),
                                    "function '%s' argument %zu has incompatible type",
                                    local_target->source_name, argument_index + 1) >= 0)
                                transpiler_diagnostics_push(&context->diagnostics,
                                    TRANSPILE_SEVERITY_ERROR,
                                    TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message);
                            transpiler_context_record_error(context,
                                TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH);
                            return (FT_FAILURE);
                        }
                        argument_index += 1;
                    }
                    statement->call_is_external = 0;
                }
                else
                {
                    const t_transpiler_function_signature *signature;
                    size_t index;

                    signature = NULL;
                    index = 0;
                    while (index < function_count)
                    {
                        if (std::strncmp(functions[index].name, statement->call_identifier,
                                TRANSPILE_FUNCTION_NAME_MAX) == 0)
                        {
                            signature = &functions[index];
                            break ;
                        }
                        index += 1;
                    }
                    if (!signature)
                    {
                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        if (std::snprintf(message, sizeof(message),
                                "function '%s' is not declared in module '%s' or its imports",
                                statement->call_identifier, module_name) >= 0)
                            transpiler_diagnostics_push(&context->diagnostics,
                                TRANSPILE_SEVERITY_ERROR, TRANSPILE_ERROR_FUNCTION_UNRESOLVED, message);
                        transpiler_context_record_error(context, TRANSPILE_ERROR_FUNCTION_UNRESOLVED);
                        return (FT_FAILURE);
                    }
                    if (!transpiler_context_resolve_function_access(context, module_name,
                            signature->module, signature->name))
                        return (FT_FAILURE);
                    if (signature->parameter_count != statement->call_argument_count)
                    {
                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        if (std::snprintf(message, sizeof(message),
                                "function '%s' expects %zu arguments but call provides %zu",
                                signature->name, signature->parameter_count,
                                statement->call_argument_count) >= 0)
                            transpiler_diagnostics_push(&context->diagnostics,
                                TRANSPILE_SEVERITY_ERROR,
                                TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH, message);
                        transpiler_context_record_error(context,
                            TRANSPILE_ERROR_FUNCTION_ARGUMENT_COUNT_MISMATCH);
                        return (FT_FAILURE);
                    }
                    index = 0;
                    while (index < statement->call_argument_count)
                    {
                        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
                        t_cblc_parameter parameter;

                        std::memset(&parameter, 0, sizeof(parameter));
                        parameter.kind = signature->parameter_kinds[index];
                        if (parameter.kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT
                            && parameter.kind != TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER
                            && (cblc_extract_call_argument(statement, index, argument,
                                    sizeof(argument)) != FT_SUCCESS
                                || !cblc_argument_matches_parameter(unit, argument, &parameter)))
                        {
                            char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            if (std::snprintf(message, sizeof(message),
                                    "function '%s' argument %zu has incompatible type",
                                    signature->name, index + 1) >= 0)
                                transpiler_diagnostics_push(&context->diagnostics,
                                    TRANSPILE_SEVERITY_ERROR,
                                    TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH, message);
                            transpiler_context_record_error(context,
                                TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH);
                            return (FT_FAILURE);
                        }
                        index += 1;
                    }
                    if (statement->type == CBLC_STATEMENT_CALL_ASSIGN
                        && signature->return_mode != TRANSPILE_FUNCTION_RETURN_VALUE)
                    {
                        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        if (std::snprintf(message, sizeof(message),
                                "module '%s' cannot assign the result of void external function '%s'",
                                module_name, signature->name) >= 0)
                            transpiler_diagnostics_push(&context->diagnostics,
                                TRANSPILE_SEVERITY_ERROR,
                                TRANSPILE_ERROR_FUNCTION_EXTERNAL_RETURN_UNSUPPORTED, message);
                        transpiler_context_record_error(context,
                            TRANSPILE_ERROR_FUNCTION_EXTERNAL_RETURN_UNSUPPORTED);
                        return (FT_FAILURE);
                    }
                    statement->call_is_external = 1;
                }
            }
            statement_index += 1;
        }
        function_index += 1;
    }
    return (FT_SUCCESS);
}

static t_transpiler_symbol_visibility cblc_export_member_visibility(
    t_cblc_member_visibility visibility)
{
    if (visibility == CBLC_MEMBER_VISIBILITY_PUBLIC)
        return (TRANSPILE_SYMBOL_PUBLIC);
    return (TRANSPILE_SYMBOL_PRIVATE);
}

static void cblc_export_type_signature_clear(t_transpiler_type_signature *signature)
{
    if (!signature)
        return ;
    if (signature->fields)
        cma_free(signature->fields);
    if (signature->methods)
        cma_free(signature->methods);
    if (signature->constructors)
        cma_free(signature->constructors);
    std::memset(signature, 0, sizeof(*signature));
    return ;
}

static int cblc_export_type_fields(t_transpiler_type_signature *signature,
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!signature || !type)
        return (FT_FAILURE);
    if (type->field_count == 0)
        return (FT_SUCCESS);
    signature->fields = static_cast<t_transpiler_type_field_signature *>(cma_calloc(
        type->field_count, sizeof(*signature->fields)));
    if (!signature->fields)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        ft_strlcpy(signature->fields[index].name, type->fields[index].source_name,
            sizeof(signature->fields[index].name));
        ft_strlcpy(signature->fields[index].declared_type_name,
            type->fields[index].declared_type_name,
            sizeof(signature->fields[index].declared_type_name));
        ft_strlcpy(signature->fields[index].struct_type_name,
            type->fields[index].struct_type_name,
            sizeof(signature->fields[index].struct_type_name));
        signature->fields[index].length = type->fields[index].length;
        signature->fields[index].array_count = type->fields[index].array_count;
        signature->fields[index].kind = static_cast<int>(type->fields[index].kind);
        signature->fields[index].is_const = type->fields[index].is_const;
        signature->fields[index].visibility =
            cblc_export_member_visibility(type->fields[index].visibility);
        index += 1;
    }
    signature->field_count = type->field_count;
    return (FT_SUCCESS);
}

static int cblc_export_type_methods(t_transpiler_type_signature *signature,
    const t_cblc_struct_type *type)
{
    size_t index;
    size_t parameter_index;

    if (!signature || !type)
        return (FT_FAILURE);
    if (type->method_count == 0)
        return (FT_SUCCESS);
    signature->methods = static_cast<t_transpiler_type_method_signature *>(cma_calloc(
        type->method_count, sizeof(*signature->methods)));
    if (!signature->methods)
        return (FT_FAILURE);
    index = 0;
    while (index < type->method_count)
    {
        ft_strlcpy(signature->methods[index].name, type->methods[index].source_name,
            sizeof(signature->methods[index].name));
        signature->methods[index].return_kind = static_cast<int>(type->methods[index].return_kind);
        ft_strlcpy(signature->methods[index].return_type_name,
            type->methods[index].return_type_name,
            sizeof(signature->methods[index].return_type_name));
        signature->methods[index].visibility =
            cblc_export_member_visibility(type->methods[index].visibility);
        parameter_index = 0;
        while (parameter_index < type->methods[index].parameter_count
            && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
        {
            signature->methods[index].parameter_kinds[parameter_index] =
                type->methods[index].parameters[parameter_index].kind;
            ft_strlcpy(signature->methods[index].parameter_source_names[parameter_index],
                type->methods[index].parameters[parameter_index].source_name,
                sizeof(signature->methods[index].parameter_source_names[parameter_index]));
            ft_strlcpy(signature->methods[index].parameter_actual_source_names[parameter_index],
                type->methods[index].parameters[parameter_index].actual_source_name,
                sizeof(signature->methods[index].parameter_actual_source_names[parameter_index]));
            ft_strlcpy(signature->methods[index].parameter_cobol_names[parameter_index],
                type->methods[index].parameters[parameter_index].cobol_name,
                sizeof(signature->methods[index].parameter_cobol_names[parameter_index]));
            ft_strlcpy(signature->methods[index].parameter_type_names[parameter_index],
                type->methods[index].parameters[parameter_index].type_name,
                sizeof(signature->methods[index].parameter_type_names[parameter_index]));
            parameter_index += 1;
        }
        signature->methods[index].parameter_count = type->methods[index].parameter_count;
        signature->methods[index].has_definition = type->methods[index].has_definition;
        if (type->methods[index].statement_count > 0 && type->methods[index].statements)
        {
            signature->methods[index].statements = static_cast<t_cblc_statement *>(cma_calloc(
                type->methods[index].statement_count,
                sizeof(*signature->methods[index].statements)));
            if (!signature->methods[index].statements)
                return (FT_FAILURE);
            std::memcpy(signature->methods[index].statements,
                type->methods[index].statements,
                type->methods[index].statement_count
                    * sizeof(*signature->methods[index].statements));
            signature->methods[index].statement_count = type->methods[index].statement_count;
        }
        index += 1;
    }
    signature->method_count = type->method_count;
    return (FT_SUCCESS);
}

static int cblc_export_type_constructors(t_transpiler_type_signature *signature,
    const t_cblc_struct_type *type)
{
    size_t index;
    size_t parameter_index;

    if (!signature || !type)
        return (FT_FAILURE);
    if (type->constructor_count == 0)
        return (FT_SUCCESS);
    signature->constructors = static_cast<t_transpiler_type_constructor_signature *>(cma_calloc(
        type->constructor_count, sizeof(*signature->constructors)));
    if (!signature->constructors)
        return (FT_FAILURE);
    index = 0;
    while (index < type->constructor_count)
    {
        parameter_index = 0;
        while (parameter_index < type->constructors[index].parameter_count
            && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
        {
            signature->constructors[index].parameter_kinds[parameter_index] =
                type->constructors[index].parameters[parameter_index].kind;
            ft_strlcpy(signature->constructors[index].parameter_source_names[parameter_index],
                type->constructors[index].parameters[parameter_index].source_name,
                sizeof(signature->constructors[index].parameter_source_names[parameter_index]));
            ft_strlcpy(signature->constructors[index].parameter_actual_source_names[parameter_index],
                type->constructors[index].parameters[parameter_index].actual_source_name,
                sizeof(signature->constructors[index].parameter_actual_source_names[parameter_index]));
            ft_strlcpy(signature->constructors[index].parameter_cobol_names[parameter_index],
                type->constructors[index].parameters[parameter_index].cobol_name,
                sizeof(signature->constructors[index].parameter_cobol_names[parameter_index]));
            ft_strlcpy(signature->constructors[index].parameter_type_names[parameter_index],
                type->constructors[index].parameters[parameter_index].type_name,
                sizeof(signature->constructors[index].parameter_type_names[parameter_index]));
            parameter_index += 1;
        }
        signature->constructors[index].parameter_count =
            type->constructors[index].parameter_count;
        signature->constructors[index].has_definition =
            type->constructors[index].has_definition;
        if (type->constructors[index].statement_count > 0
            && type->constructors[index].statements)
        {
            signature->constructors[index].statements =
                static_cast<t_cblc_statement *>(cma_calloc(
                    type->constructors[index].statement_count,
                    sizeof(*signature->constructors[index].statements)));
            if (!signature->constructors[index].statements)
                return (FT_FAILURE);
            std::memcpy(signature->constructors[index].statements,
                type->constructors[index].statements,
                type->constructors[index].statement_count
                    * sizeof(*signature->constructors[index].statements));
            signature->constructors[index].statement_count =
                type->constructors[index].statement_count;
        }
        index += 1;
    }
    signature->constructor_count = type->constructor_count;
    return (FT_SUCCESS);
}

static int cblc_export_type_signature_fill(t_transpiler_type_signature *signature,
    const t_cblc_struct_type *type)
{
    if (!signature || !type)
        return (FT_FAILURE);
    if (cblc_export_type_fields(signature, type) != FT_SUCCESS)
    {
        cblc_export_type_signature_clear(signature);
        return (FT_FAILURE);
    }
    if (cblc_export_type_methods(signature, type) != FT_SUCCESS)
    {
        cblc_export_type_signature_clear(signature);
        return (FT_FAILURE);
    }
    if (cblc_export_type_constructors(signature, type) != FT_SUCCESS)
    {
        cblc_export_type_signature_clear(signature);
        return (FT_FAILURE);
    }
    if (type->destructor_statement_count > 0 && type->destructor_statements)
    {
        signature->destructor_statements = static_cast<t_cblc_statement *>(cma_calloc(
            type->destructor_statement_count, sizeof(*signature->destructor_statements)));
        if (!signature->destructor_statements)
        {
            cblc_export_type_signature_clear(signature);
            return (FT_FAILURE);
        }
        std::memcpy(signature->destructor_statements, type->destructor_statements,
            type->destructor_statement_count * sizeof(*signature->destructor_statements));
        signature->destructor_statement_count = type->destructor_statement_count;
    }
    signature->has_destructor_definition = type->has_destructor_definition;
    return (FT_SUCCESS);
}

static int cblc_type_has_exportable_definitions(const t_cblc_struct_type *type)
{
    size_t index;

    if (!type)
        return (0);
    index = 0;
    while (index < type->method_count)
    {
        if (type->methods[index].has_definition)
            return (1);
        index += 1;
    }
    index = 0;
    while (index < type->constructor_count)
    {
        if (type->constructors[index].has_definition)
            return (1);
        index += 1;
    }
    if (type->has_destructor_definition)
        return (1);
    return (0);
}

int cblc_register_translation_unit_exports(t_transpiler_context *context, const char *module_name,
    const t_cblc_translation_unit *unit)
{
    size_t entry_index;
    size_t index;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    if (module_name[0] == '\0')
        return (FT_FAILURE);
    index = 0;
    while (index < unit->struct_type_count)
    {
        t_transpiler_type_signature signature;

        if (!unit->struct_types[index].is_builtin
            && (!unit->struct_types[index].is_imported
                || cblc_type_has_exportable_definitions(&unit->struct_types[index])))
        {
            std::memset(&signature, 0, sizeof(signature));
            ft_strlcpy(signature.name, unit->struct_types[index].source_name,
                sizeof(signature.name));
            signature.kind = TRANSPILE_TYPE_STRUCT;
            if (unit->struct_types[index].is_class)
                signature.kind = TRANSPILE_TYPE_CLASS;
            signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
            signature.has_default_constructor = unit->struct_types[index].has_default_constructor;
            signature.has_destructor = unit->struct_types[index].has_destructor;
            if (cblc_export_type_signature_fill(&signature, &unit->struct_types[index])
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (transpiler_context_register_type_signature(context, module_name,
                    &signature) != FT_SUCCESS)
            {
                cblc_export_type_signature_clear(&signature);
                return (FT_FAILURE);
            }
            cblc_export_type_signature_clear(&signature);
        }
        index += 1;
    }
    if (unit->function_count == 0)
        return (FT_SUCCESS);
    entry_index = static_cast<size_t>(-1);
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strncmp(unit->functions[index].source_name, "main", TRANSPILE_IDENTIFIER_MAX) == 0)
        {
            entry_index = index;
            break ;
        }
        index += 1;
    }
    if (entry_index != static_cast<size_t>(-1))
    {
        t_transpiler_function_return_mode return_mode;

        return_mode = TRANSPILE_FUNCTION_RETURN_VOID;
        if (unit->functions[entry_index].return_kind != CBLC_FUNCTION_RETURN_VOID)
            return_mode = TRANSPILE_FUNCTION_RETURN_VALUE;
        if (transpiler_context_register_entrypoint(context, module_name,
                unit->functions[entry_index].source_name, return_mode, NULL, NULL)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    index = 0;
    while (index < unit->function_count)
    {
        if (index != entry_index)
        {
            t_transpiler_function_return_mode return_mode;
            t_transpiler_function_parameter_kind parameter_kinds[TRANSPILE_FUNCTION_PARAMETER_MAX];
            size_t parameter_index;

            return_mode = TRANSPILE_FUNCTION_RETURN_VOID;
            if (unit->functions[index].return_kind != CBLC_FUNCTION_RETURN_VOID)
                return_mode = TRANSPILE_FUNCTION_RETURN_VALUE;
            parameter_index = 0;
            while (parameter_index < unit->functions[index].parameter_count)
            {
                parameter_kinds[parameter_index] = unit->functions[index].parameters[parameter_index].kind;
                parameter_index += 1;
            }
            if (transpiler_context_register_function_signature(context, module_name,
                    unit->functions[index].source_name, return_mode,
                    TRANSPILE_SYMBOL_PUBLIC, parameter_kinds,
                    unit->functions[index].parameter_count) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}
