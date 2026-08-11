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

static int cblc_validate_exception_abi(const t_transpiler_context *context,
    const char *requesting_module, const char *provider_module, const char *symbol_name,
    size_t abi_version, int required)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context || !requesting_module || !provider_module || !symbol_name)
        return (FT_FAILURE);
    if (abi_version == CBLC_EXCEPTION_ABI_VERSION
        || (!required && abi_version == 0))
        return (FT_SUCCESS);
    if (std::snprintf(message, sizeof(message),
            "exception ABI mismatch for '%s' imported by module '%s' from module '%s' (version %zu, expected %d)",
            symbol_name, requesting_module, provider_module, abi_version,
            CBLC_EXCEPTION_ABI_VERSION) < 0)
        return (FT_FAILURE);
    transpiler_diagnostics_push(&const_cast<t_transpiler_context *>(context)->diagnostics,
        TRANSPILE_SEVERITY_ERROR, TRANSPILE_ERROR_EXCEPTION_ABI_MISMATCH, message);
    transpiler_context_record_error(const_cast<t_transpiler_context *>(context),
        TRANSPILE_ERROR_EXCEPTION_ABI_MISMATCH);
    return (FT_FAILURE);
}

static int cblc_validate_exception_policy(const t_transpiler_context *context,
    const char *requesting_module, const char *provider_module, const char *symbol_name,
    unsigned long long policy_fingerprint, int required)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context || !requesting_module || !provider_module || !symbol_name)
        return (FT_FAILURE);
    if (policy_fingerprint == CBLC_EXCEPTION_RUNTIME_POLICY_FINGERPRINT
        || (!required && policy_fingerprint == 0))
        return (FT_SUCCESS);
    if (std::snprintf(message, sizeof(message),
            "exception runtime policy mismatch for '%s' imported by module '%s' from module '%s' (fingerprint %llu, expected %llu)",
            symbol_name, requesting_module, provider_module,
            policy_fingerprint, CBLC_EXCEPTION_RUNTIME_POLICY_FINGERPRINT) < 0)
        return (FT_FAILURE);
    transpiler_diagnostics_push(&const_cast<t_transpiler_context *>(context)->diagnostics,
        TRANSPILE_SEVERITY_ERROR, TRANSPILE_ERROR_EXCEPTION_POLICY_MISMATCH, message);
    transpiler_context_record_error(const_cast<t_transpiler_context *>(context),
        TRANSPILE_ERROR_EXCEPTION_POLICY_MISMATCH);
    return (FT_FAILURE);
}

static int cblc_validate_exception_payload_size(const t_transpiler_context *context,
    const char *requesting_module, const char *provider_module,
    const char *type_name, size_t payload_size)
{
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context || !requesting_module || !provider_module || !type_name)
        return (FT_FAILURE);
    if (payload_size <= CBLC_EXCEPTION_PAYLOAD_MAX)
        return (FT_SUCCESS);
    if (std::snprintf(message, sizeof(message),
            "exception payload metadata for '%s' imported by module '%s' from module '%s' requires size %zu, exceeding the configured limit %d",
            type_name, requesting_module, provider_module, payload_size,
            CBLC_EXCEPTION_PAYLOAD_MAX) < 0)
        return (FT_FAILURE);
    transpiler_diagnostics_push(&const_cast<t_transpiler_context *>(context)->diagnostics,
        TRANSPILE_SEVERITY_ERROR, TRANSPILE_ERROR_EXCEPTION_PAYLOAD_OVERFLOW, message);
    transpiler_context_record_error(const_cast<t_transpiler_context *>(context),
        TRANSPILE_ERROR_EXCEPTION_PAYLOAD_OVERFLOW);
    return (FT_FAILURE);
}

int cblc_import_translation_unit_type_stubs(t_transpiler_context *context, const char *module_name,
    t_cblc_translation_unit *unit)
{
    const t_transpiler_data_signature *data_signatures;
    const t_transpiler_type_signature *types;
    size_t data_count;
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
            if (!types[index].is_template
                && (types[index].exception_abi_version != 0
                || types[index].exception_policy_fingerprint != 0
                || types[index].exception_payload_size != 0))
            {
                if (cblc_validate_exception_abi(context, module_name, types[index].module,
                        types[index].name, types[index].exception_abi_version, 1) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_validate_exception_policy(context, module_name, types[index].module,
                        types[index].name, types[index].exception_policy_fingerprint, 1)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_validate_exception_payload_size(context, module_name,
                        types[index].module, types[index].name,
                        types[index].exception_payload_size) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cblc_register_imported_type_stub(unit, &types[index]) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    data_signatures = transpiler_context_get_data_signatures(context, &data_count);
    index = 0;
    while (index < data_count)
    {
        if (std::strncmp(data_signatures[index].module, module_name, TRANSPILE_MODULE_NAME_MAX) != 0
            && data_signatures[index].visibility == TRANSPILE_SYMBOL_PUBLIC
            && cblc_context_module_imports_module(context, module_name,
                data_signatures[index].module))
        {
            if (cblc_register_imported_data_stub(unit, &data_signatures[index]) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int cblc_import_translation_unit_function_stubs(t_transpiler_context *context,
    const char *module_name, t_cblc_translation_unit *unit)
{
    const t_transpiler_function_signature *functions;
    size_t function_count;
    size_t index;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    functions = transpiler_context_get_functions(context, &function_count);
    index = 0;
    while (index < function_count)
    {
        const t_transpiler_function_signature *signature;
        const t_cblc_function *existing;
        t_cblc_function *function;
        size_t parameter_index;

        signature = &functions[index];
        if (!signature->is_template
            || std::strncmp(signature->module, module_name, TRANSPILE_MODULE_NAME_MAX) == 0
            || !cblc_context_module_imports_module(context, module_name, signature->module))
        {
            index += 1;
            continue ;
        }
        if (signature->may_throw
            && cblc_validate_exception_abi(context, module_name, signature->module,
                signature->name, signature->exception_abi_version, 1) != FT_SUCCESS)
            return (FT_FAILURE);
        if (signature->may_throw
            && cblc_validate_exception_policy(context, module_name, signature->module,
                signature->name, signature->exception_policy_fingerprint, 1) != FT_SUCCESS)
            return (FT_FAILURE);
        existing = cblc_find_function_in_unit(unit, signature->name);
        if (existing)
        {
            index += 1;
            continue ;
        }
        if (unit->function_count >= unit->function_capacity
            && cblc_translation_unit_ensure_function_capacity(unit,
                unit->function_capacity == 0 ? 2 : unit->function_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
        function = &unit->functions[unit->function_count];
        std::memset(function, 0, sizeof(*function));
        ft_strlcpy(function->source_name, signature->name, sizeof(function->source_name));
        cblc_identifier_to_cobol(function->source_name, function->cobol_name,
            sizeof(function->cobol_name));
        function->return_kind = static_cast<t_cblc_function_return_kind>(signature->return_kind);
        ft_strlcpy(function->return_type_name, signature->return_type_name,
            sizeof(function->return_type_name));
        ft_strlcpy(function->return_cobol_name, signature->return_cobol_name,
            sizeof(function->return_cobol_name));
        ft_strlcpy(function->return_source_name, signature->return_source_name,
            sizeof(function->return_source_name));
        function->saw_return = signature->saw_return;
        function->may_throw = signature->may_throw;
        function->exception_type_count = signature->exception_type_count;
        std::memcpy(function->exception_type_ids, signature->exception_type_ids,
            sizeof(function->exception_type_ids));
        function->exception_types_unknown = signature->exception_types_unknown;
        function->parameter_count = signature->parameter_count;
        function->is_template = 1;
        function->template_parameter_count = signature->template_parameter_count;
        size_t template_parameter_index = 0;
        while (template_parameter_index < function->template_parameter_count
            && template_parameter_index < CBLC_TEMPLATE_PARAMETER_MAX)
        {
            ft_strlcpy(function->template_parameter_names[template_parameter_index],
                signature->template_parameter_names[template_parameter_index],
                sizeof(function->template_parameter_names[template_parameter_index]));
            template_parameter_index += 1;
        }
        ft_strlcpy(function->template_parameter_name, signature->template_parameter_name,
            sizeof(function->template_parameter_name));
        parameter_index = 0;
        while (parameter_index < signature->parameter_count
            && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
        {
            function->parameters[parameter_index].kind = signature->parameter_kinds[parameter_index];
            function->parameters[parameter_index].array_count =
                signature->parameter_array_counts[parameter_index];
            function->parameters[parameter_index].length =
                signature->parameter_lengths[parameter_index];
            ft_strlcpy(function->parameters[parameter_index].source_name,
                signature->parameter_source_names[parameter_index],
                sizeof(function->parameters[parameter_index].source_name));
            ft_strlcpy(function->parameters[parameter_index].actual_source_name,
                signature->parameter_actual_source_names[parameter_index],
                sizeof(function->parameters[parameter_index].actual_source_name));
            ft_strlcpy(function->parameters[parameter_index].cobol_name,
                signature->parameter_cobol_names[parameter_index],
                sizeof(function->parameters[parameter_index].cobol_name));
            ft_strlcpy(function->parameters[parameter_index].type_name,
                signature->parameter_type_names[parameter_index],
                sizeof(function->parameters[parameter_index].type_name));
            parameter_index += 1;
        }
        if (signature->statement_count > 0 && signature->statements)
        {
            function->statements = static_cast<t_cblc_statement *>(cma_calloc(
                signature->statement_count, sizeof(*function->statements)));
            if (!function->statements)
                return (FT_FAILURE);
            std::memcpy(function->statements, signature->statements,
                signature->statement_count * sizeof(*function->statements));
            function->statement_count = signature->statement_count;
            function->statement_capacity = signature->statement_count;
        }
        unit->function_count += 1;
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_merge_member_exception_summary(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cblc_function *function)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *type;
    t_cblc_function summary;

    if (!unit || !statement || !function)
        return ;
    std::memset(&summary, 0, sizeof(summary));
    if (statement->type == CBLC_STATEMENT_METHOD_CALL
        || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
    {
        const t_cblc_method *method;
        const char *receiver_name;

        receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
            ? statement->target : statement->source;
        receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
        type = receiver ? cblc_find_receiver_type(unit, receiver) : NULL;
        method = type ? cblc_find_method_on_type(type, statement->call_identifier) : NULL;
        if (!method)
            return ;
        summary.exception_type_count = method->exception_type_count;
        std::memcpy(summary.exception_type_ids, method->exception_type_ids,
            sizeof(summary.exception_type_ids));
        summary.exception_types_unknown = method->exception_types_unknown;
        if (method->statements)
            cblc_collect_statement_exception_summary(method->statements,
                method->statement_count, &summary);
        cblc_function_merge_exception_summary(function, summary.exception_type_count,
            summary.exception_type_ids, summary.exception_types_unknown);
    }
    else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT)
    {
        const t_cblc_constructor *constructor;

        receiver = cblc_find_data_item_by_cobol(unit, statement->target);
        if (!receiver || receiver->kind != CBLC_DATA_KIND_STRUCT)
            return ;
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        constructor = type ? cblc_find_constructor_for_arguments(unit, type,
            statement->call_arguments, statement->call_argument_count) : NULL;
        if (!constructor)
            return ;
        summary.exception_type_count = constructor->exception_type_count;
        std::memcpy(summary.exception_type_ids, constructor->exception_type_ids,
            sizeof(summary.exception_type_ids));
        summary.exception_types_unknown = constructor->exception_types_unknown;
        if (constructor->statements)
            cblc_collect_statement_exception_summary(constructor->statements,
                constructor->statement_count, &summary);
        cblc_function_merge_exception_summary(function, summary.exception_type_count,
            summary.exception_type_ids, summary.exception_types_unknown);
    }
}

static int cblc_propagate_local_exception_summaries(t_cblc_translation_unit *unit)
{
    size_t pass;
    int changed;

    if (!unit)
        return (FT_FAILURE);
    pass = 0;
    do
    {
        changed = 0;
        size_t function_index = 0;
        while (function_index < unit->function_count)
        {
            t_cblc_function *function;
            size_t statement_index;

            function = &unit->functions[function_index];
            statement_index = 0;
            while (statement_index < function->statement_count)
            {
                t_cblc_statement *statement;
                const t_cblc_function *target;
                size_t old_count;
                int old_unknown;

                statement = &function->statements[statement_index];
                old_count = function->exception_type_count;
                old_unknown = function->exception_types_unknown;
                cblc_merge_member_exception_summary(unit, statement, function);
                if (old_count != function->exception_type_count
                    || old_unknown != function->exception_types_unknown)
                    changed = 1;
                if ((statement->type != CBLC_STATEMENT_CALL
                        && statement->type != CBLC_STATEMENT_CALL_ASSIGN)
                    || statement->call_is_external)
                {
                    statement_index += 1;
                    continue ;
                }
                target = cblc_find_function_in_unit(unit, statement->call_identifier);
                if (!target)
                {
                    statement_index += 1;
                    continue ;
                }
                old_count = function->exception_type_count;
                old_unknown = function->exception_types_unknown;
                cblc_function_merge_exception_summary(function,
                    target->exception_type_count, target->exception_type_ids,
                    target->exception_types_unknown);
                if (old_count != function->exception_type_count
                    || old_unknown != function->exception_types_unknown)
                    changed = 1;
                statement_index += 1;
            }
            function_index += 1;
        }
        pass += 1;
    } while (changed && pass <= unit->function_count + CBLC_EXCEPTION_THROW_SET_MAX);
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
                        sizeof(statement->call_identifier)) == 0
                    || std::strncmp(statement->call_identifier, "CBLC-NEW",
                        std::strlen("CBLC-NEW")) == 0
                    || std::strncmp(statement->call_identifier, "CBLC-DELETE",
                        std::strlen("CBLC-DELETE")) == 0
                    || std::strncmp(statement->call_identifier, "std::odd",
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
                if (transpiler_standard_library_lookup(statement->call_identifier))
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
                    statement->exception_may_throw = local_target->may_throw
                        || cblc_function_may_throw(unit, local_target, 0);
                    cblc_function_merge_exception_summary(function,
                        local_target->exception_type_count,
                        local_target->exception_type_ids,
                        local_target->exception_types_unknown);
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
                    if (signature->may_throw
                        && cblc_validate_exception_abi(context, module_name,
                            signature->module, signature->name,
                            signature->exception_abi_version, 1) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (signature->may_throw
                        && cblc_validate_exception_policy(context, module_name,
                            signature->module, signature->name,
                            signature->exception_policy_fingerprint, 1) != FT_SUCCESS)
                        return (FT_FAILURE);
                    statement->exception_may_throw = signature->may_throw;
                    cblc_function_merge_exception_summary(function,
                        signature->exception_type_count,
                        signature->exception_type_ids,
                        signature->exception_types_unknown
                        || (signature->may_throw && signature->exception_type_count == 0));
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
                        parameter.array_count = signature->parameter_array_counts[index];
                        parameter.length = signature->parameter_lengths[index];
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
    if (cblc_propagate_local_exception_summaries(unit) != FT_SUCCESS)
        return (FT_FAILURE);
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
        signature->fields[index].is_borrowed = type->fields[index].is_borrowed;
        signature->fields[index].is_template_parameter =
            type->fields[index].is_template_parameter;
        signature->fields[index].template_parameter_pointer_depth =
            type->fields[index].template_parameter_pointer_depth;
        ft_strlcpy(signature->fields[index].template_parameter_name,
            type->fields[index].template_parameter_name,
            sizeof(signature->fields[index].template_parameter_name));
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
        t_cblc_function summary;

        std::memset(&summary, 0, sizeof(summary));
        cblc_collect_statement_exception_summary(type->methods[index].statements,
            type->methods[index].statement_count, &summary);
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
            signature->methods[index].parameter_array_counts[parameter_index] =
                type->methods[index].parameters[parameter_index].array_count;
            signature->methods[index].parameter_lengths[parameter_index] =
                type->methods[index].parameters[parameter_index].length;
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
        signature->methods[index].exception_type_count = summary.exception_type_count;
        std::memcpy(signature->methods[index].exception_type_ids,
            summary.exception_type_ids, sizeof(summary.exception_type_ids));
        signature->methods[index].exception_types_unknown = summary.exception_types_unknown;
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
        t_cblc_function summary;

        std::memset(&summary, 0, sizeof(summary));
        cblc_collect_statement_exception_summary(type->constructors[index].statements,
            type->constructors[index].statement_count, &summary);
        parameter_index = 0;
        while (parameter_index < type->constructors[index].parameter_count
            && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
        {
            signature->constructors[index].parameter_kinds[parameter_index] =
                type->constructors[index].parameters[parameter_index].kind;
            signature->constructors[index].parameter_array_counts[parameter_index] =
                type->constructors[index].parameters[parameter_index].array_count;
            signature->constructors[index].parameter_lengths[parameter_index] =
                type->constructors[index].parameters[parameter_index].length;
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
        signature->constructors[index].exception_type_count = summary.exception_type_count;
        std::memcpy(signature->constructors[index].exception_type_ids,
            summary.exception_type_ids, sizeof(summary.exception_type_ids));
        signature->constructors[index].exception_types_unknown = summary.exception_types_unknown;
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
    t_cblc_translation_unit *unit)
{
    size_t entry_index;
    size_t index;
    t_transpiler_data_signature data_signature;

    if (!context || !module_name || !unit)
        return (FT_FAILURE);
    if (module_name[0] == '\0')
        return (FT_FAILURE);
    if (cblc_propagate_local_exception_summaries(unit) != FT_SUCCESS)
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
            signature.has_base_type = unit->struct_types[index].has_base_type;
            signature.base_is_public = unit->struct_types[index].base_is_public;
            signature.exception_abi_version = CBLC_EXCEPTION_ABI_VERSION;
            signature.exception_policy_fingerprint =
                CBLC_EXCEPTION_RUNTIME_POLICY_FINGERPRINT;
            signature.exception_type_id = cblc_exception_type_id(
                unit->struct_types[index].source_name);
            signature.exception_payload_size = cblc_exception_payload_size(
                unit, &unit->struct_types[index], 0);
            ft_strlcpy(signature.base_type_name, unit->struct_types[index].base_type_name,
                sizeof(signature.base_type_name));
            signature.kind = TRANSPILE_TYPE_STRUCT;
            if (unit->struct_types[index].is_class)
                signature.kind = TRANSPILE_TYPE_CLASS;
            signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
            signature.has_default_constructor = unit->struct_types[index].has_default_constructor;
            signature.has_destructor = unit->struct_types[index].has_destructor;
            signature.is_template = unit->struct_types[index].is_template;
            signature.template_parameter_count =
                unit->struct_types[index].template_parameter_count;
            size_t template_parameter_index = 0;
            while (template_parameter_index < signature.template_parameter_count
                && template_parameter_index < CBLC_TEMPLATE_PARAMETER_MAX)
            {
                ft_strlcpy(signature.template_parameter_names[template_parameter_index],
                    unit->struct_types[index].template_parameter_names[template_parameter_index],
                    sizeof(signature.template_parameter_names[template_parameter_index]));
                template_parameter_index += 1;
            }
            ft_strlcpy(signature.template_parameter_name,
                unit->struct_types[index].template_parameter_name,
                sizeof(signature.template_parameter_name));
            signature.layout_fingerprint = cblc_struct_layout_fingerprint(
                unit, &unit->struct_types[index], 0);
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
    index = 0;
    while (index < unit->data_count)
    {
        const t_cblc_data_item *item;

        item = &unit->data_items[index];
        if (item->is_alias || item->is_function_local || item->is_imported
            || item->owner_function_name[0] != '\0'
            || std::strchr(item->source_name, '.')
            || std::strstr(item->source_name, "->"))
        {
            index += 1;
            continue ;
        }
        std::memset(&data_signature, 0, sizeof(data_signature));
        ft_strlcpy(data_signature.name, item->source_name, sizeof(data_signature.name));
        ft_strlcpy(data_signature.declared_type_name, item->declared_type_name,
            sizeof(data_signature.declared_type_name));
        ft_strlcpy(data_signature.struct_type_name, item->struct_type_name,
            sizeof(data_signature.struct_type_name));
        data_signature.length = item->length;
        data_signature.array_count = item->array_count;
        data_signature.kind = item->kind;
        data_signature.is_const = item->is_const;
        data_signature.has_initializer = item->has_initializer;
        data_signature.initializer_length = item->initializer_length;
        ft_strlcpy(data_signature.initializer_text, item->initializer_text,
            sizeof(data_signature.initializer_text));
        ft_strlcpy(data_signature.constructor_arguments, item->constructor_arguments,
            sizeof(data_signature.constructor_arguments));
        data_signature.constructor_argument_count = item->constructor_argument_count;
        data_signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
        if (transpiler_context_register_data_signature(context, module_name, &data_signature) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    if (unit->function_count == 0)
        return (FT_SUCCESS);
    entry_index = static_cast<size_t>(-1);
    index = 0;
    while (index < unit->function_count)
    {
        if (!unit->functions[index].is_template
            && std::strncmp(unit->functions[index].source_name, "main", TRANSPILE_IDENTIFIER_MAX) == 0)
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
            t_transpiler_function_signature signature;
            size_t parameter_index;

            std::memset(&signature, 0, sizeof(signature));
            ft_strlcpy(signature.name, unit->functions[index].source_name,
                sizeof(signature.name));
            signature.return_mode = TRANSPILE_FUNCTION_RETURN_VOID;
            if (unit->functions[index].return_kind != CBLC_FUNCTION_RETURN_VOID)
                signature.return_mode = TRANSPILE_FUNCTION_RETURN_VALUE;
            signature.return_kind = unit->functions[index].return_kind;
            ft_strlcpy(signature.return_type_name, unit->functions[index].return_type_name,
                sizeof(signature.return_type_name));
            ft_strlcpy(signature.return_cobol_name, unit->functions[index].return_cobol_name,
                sizeof(signature.return_cobol_name));
            ft_strlcpy(signature.return_source_name, unit->functions[index].return_source_name,
                sizeof(signature.return_source_name));
            signature.saw_return = unit->functions[index].saw_return;
            signature.may_throw = cblc_function_may_throw(unit,
                &unit->functions[index], 0);
            signature.exception_abi_version = CBLC_EXCEPTION_ABI_VERSION;
            signature.exception_policy_fingerprint =
                CBLC_EXCEPTION_RUNTIME_POLICY_FINGERPRINT;
            signature.exception_type_count = unit->functions[index].exception_type_count;
            std::memcpy(signature.exception_type_ids,
                unit->functions[index].exception_type_ids,
                sizeof(signature.exception_type_ids));
            signature.exception_types_unknown = unit->functions[index].exception_types_unknown
                || (signature.may_throw && signature.exception_type_count == 0);
            size_t summary_statement_index = 0;
            while (summary_statement_index < unit->functions[index].statement_count)
            {
                const t_cblc_statement *summary_statement;

                summary_statement = &unit->functions[index].statements[summary_statement_index];
                if (summary_statement->exception_is_rethrow
                    || summary_statement->type == CBLC_STATEMENT_FILE_OPEN
                    || summary_statement->type == CBLC_STATEMENT_FILE_CLOSE
                    || summary_statement->type == CBLC_STATEMENT_FILE_READ
                    || summary_statement->type == CBLC_STATEMENT_FILE_WRITE
                    || ((summary_statement->type == CBLC_STATEMENT_CALL
                        || summary_statement->type == CBLC_STATEMENT_CALL_ASSIGN)
                        && summary_statement->exception_may_throw
                        && summary_statement->call_is_external))
                {
                    signature.exception_types_unknown = 1;
                    break ;
                }
                summary_statement_index += 1;
            }
            signature.visibility = TRANSPILE_SYMBOL_PUBLIC;
            signature.is_template = unit->functions[index].is_template;
            signature.template_parameter_count =
                unit->functions[index].template_parameter_count;
            size_t template_parameter_index = 0;
            while (template_parameter_index < signature.template_parameter_count
                && template_parameter_index < CBLC_TEMPLATE_PARAMETER_MAX)
            {
                ft_strlcpy(signature.template_parameter_names[template_parameter_index],
                    unit->functions[index].template_parameter_names[template_parameter_index],
                    sizeof(signature.template_parameter_names[template_parameter_index]));
                template_parameter_index += 1;
            }
            ft_strlcpy(signature.template_parameter_name,
                unit->functions[index].template_parameter_name,
                sizeof(signature.template_parameter_name));
            signature.statements = unit->functions[index].statements;
            signature.statement_count = unit->functions[index].statement_count;
            signature.parameter_count = unit->functions[index].parameter_count;
            parameter_index = 0;
            while (parameter_index < unit->functions[index].parameter_count)
            {
                signature.parameter_kinds[parameter_index] =
                    unit->functions[index].parameters[parameter_index].kind;
                signature.parameter_array_counts[parameter_index] =
                    unit->functions[index].parameters[parameter_index].array_count;
                signature.parameter_lengths[parameter_index] =
                    unit->functions[index].parameters[parameter_index].length;
                ft_strlcpy(signature.parameter_source_names[parameter_index],
                    unit->functions[index].parameters[parameter_index].source_name,
                    sizeof(signature.parameter_source_names[parameter_index]));
                ft_strlcpy(signature.parameter_actual_source_names[parameter_index],
                    unit->functions[index].parameters[parameter_index].actual_source_name,
                    sizeof(signature.parameter_actual_source_names[parameter_index]));
                ft_strlcpy(signature.parameter_cobol_names[parameter_index],
                    unit->functions[index].parameters[parameter_index].cobol_name,
                    sizeof(signature.parameter_cobol_names[parameter_index]));
                ft_strlcpy(signature.parameter_type_names[parameter_index],
                    unit->functions[index].parameters[parameter_index].type_name,
                    sizeof(signature.parameter_type_names[parameter_index]));
                parameter_index += 1;
            }
            if (transpiler_context_register_function_signature_record(context, module_name,
                    &signature) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}
