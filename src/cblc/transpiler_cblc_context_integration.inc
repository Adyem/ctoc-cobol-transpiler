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

int cblc_register_translation_unit_exports(t_transpiler_context *context, const char *module_name,
    const t_cblc_translation_unit *unit)
{
    size_t entry_index;
    size_t index;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    if (module_name[0] == '\0')
        return (FT_FAILURE);
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
