static size_t cblc_native_result_digits(const t_cblc_translation_unit *unit,
    const char *cobol_name);

static void cblc_set_backend_ir_diagnostic(int *out_diagnostic_code,
    char *out_diagnostic_message, size_t diagnostic_message_size,
    const t_cblc_function *function,
    const cblc::backend::typed_function *typed_function,
    const cblc::backend::verification_diagnostic *diagnostic,
    const char *phase)
{
    const char *function_name;
    const char *reason;
    std::uint32_t source_line = 0;
    std::uint32_t source_column = 0;

    if (out_diagnostic_code)
        *out_diagnostic_code = TRANSPILE_ERROR_BACKEND_IR_VERIFICATION;
    if (!out_diagnostic_message || diagnostic_message_size == 0)
        return ;
    function_name = function && function->source_name[0] != '\0'
        ? function->source_name : "<unknown>";
    reason = diagnostic && !diagnostic->message.empty()
        ? diagnostic->message.c_str() : "unspecified verifier failure";
    if (typed_function && diagnostic && diagnostic->operation != 0)
    {
        for (const cblc::backend::typed_operation &operation
                : typed_function->operations)
        {
            if (operation.id != diagnostic->operation)
                continue ;
            for (const cblc::backend::source_origin &origin : operation.provenance)
            {
                if (origin.line != 0 && origin.column != 0)
                {
                    source_line = origin.line;
                    source_column = origin.column;
                    break ;
                }
            }
            break ;
        }
    }
    if (source_line != 0)
        std::snprintf(out_diagnostic_message, diagnostic_message_size,
            "typed %s verification failed in function '%s' at line %u, column %u: %s",
            phase, function_name, source_line, source_column, reason);
    else
        std::snprintf(out_diagnostic_message, diagnostic_message_size,
            "typed %s verification failed in function '%s': %s",
            phase, function_name, reason);
}

static int cblc_method_parameter_storage_uses_method(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_method *requested_method)
{
    const t_cblc_struct_type *receiver_type;
    size_t type_index;

    if (!unit || !receiver || !requested_method)
        return (0);
    receiver_type = cblc_find_struct_type(unit, receiver->struct_type_name);
    if (!receiver_type)
        return (0);
    for (type_index = 0; type_index < unit->struct_type_count; ++type_index)
    {
        const t_cblc_struct_type *owner_type = &unit->struct_types[type_index];
        size_t owner_method_index;

        owner_method_index = 0;
        while (owner_method_index < owner_type->method_count)
        {
            const t_cblc_method *owner_method =
                &owner_type->methods[owner_method_index];
            size_t parameter_index;
            int owns_receiver_storage = 0;

            parameter_index = 0;
            while (parameter_index < owner_method->parameter_count)
            {
                const t_cblc_parameter *parameter =
                    &owner_method->parameters[parameter_index];

                if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
                    && std::strcmp(parameter->cobol_name,
                        receiver->cobol_name) == 0
                    && std::strcmp(parameter->type_name,
                        receiver->struct_type_name) == 0
                    && (parameter->actual_source_name[0] == '\0'
                        || std::strcmp(parameter->actual_source_name,
                            receiver->source_name) == 0))
                {
                    owns_receiver_storage = 1;
                    break ;
                }
                parameter_index += 1;
            }
            if (owns_receiver_storage)
            {
                size_t statement_index;

                statement_index = 0;
                while (statement_index < owner_method->statement_count)
                {
                    const t_cblc_statement *statement =
                        &owner_method->statements[statement_index];
                    const t_cblc_method *resolved_method;

                    if (statement->type != CBLC_STATEMENT_METHOD_CALL
                        && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                    {
                        statement_index += 1;
                        continue ;
                    }
                    resolved_method = cblc_find_method_on_type_for_arguments(unit,
                        receiver_type, statement->call_identifier,
                        statement->call_arguments,
                        statement->call_argument_count);
                    if (resolved_method == requested_method)
                        return (1);
                    statement_index += 1;
                }
            }
            owner_method_index += 1;
        }
    }
    return (0);
}

static int cblc_statement_calls_method(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const t_cblc_statement *statement,
    const t_cblc_method *requested_method);

static int cblc_statement_constructs_receiver(
    const t_cblc_translation_unit *unit,
    const t_cblc_data_item *receiver, const t_cblc_struct_type *type,
    const t_cblc_statement *statement)
{
    const char *target;

    if (!receiver || !statement
        || (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            && !statement->is_move_construct
            && !statement->is_relocate_construct))
        return (0);
    target = statement->target;
    const auto matches_storage = [&](const char *storage_name)
    {
        size_t storage_length;
        const char *closing_bracket;

        if (!storage_name || storage_name[0] == '\0')
            return (0);
        if (std::strcmp(target, storage_name) == 0)
            return (1);
        storage_length = std::strlen(storage_name);
        if (std::strncmp(target, storage_name, storage_length) != 0
            || target[storage_length] != '[')
            return (0);
        closing_bracket = std::strrchr(target + storage_length, ']');
        return (closing_bracket && closing_bracket[1] == '\0' ? 1 : 0);
    };

    if (matches_storage(receiver->cobol_name)
        || matches_storage(receiver->source_name)
        || matches_storage(receiver->local_source_name))
        return (1);
    /* Nested template lifecycle receivers are synthesized from a pointer
     * field and may have a hashed COBOL name. Resolve an indexed construction
     * through the same canonical pointer-name builder used by code emission,
     * rather than comparing a truncated textual prefix. */
    if (unit && type && statement->target[0] != '\0')
    {
        const char *open_bracket = std::strchr(statement->target, '[');
        const char *close_bracket = open_bracket
            ? std::strchr(open_bracket, ']') : NULL;

        if (open_bracket && close_bracket && close_bracket[1] == '\0')
        {
            char pointer_name[TRANSPILE_IDENTIFIER_MAX];
            char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
            const size_t pointer_name_length = static_cast<size_t>(
                open_bracket - statement->target);

            if (pointer_name_length > 0
                && pointer_name_length < sizeof(pointer_name))
            {
                std::memcpy(pointer_name, statement->target,
                    pointer_name_length);
                pointer_name[pointer_name_length] = '\0';
                if (cblc_build_struct_pointer_based_name(unit, pointer_name,
                        type->source_name, based_name, sizeof(based_name))
                        == FT_SUCCESS
                    && std::strcmp(based_name, receiver->cobol_name) == 0)
                    return (1);
            }
        }
    }
    return (0);
}

static int cblc_collect_used_constructors_for_receiver(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_struct_type *type,
    std::unordered_set<const t_cblc_constructor *> *used_constructors)
{
    int observed_receiver_construction;
    int receiver_is_parameter_storage;
    int receiver_is_based_alias;

    if (!unit || !receiver || !type || !used_constructors)
        return (FT_FAILURE);
    used_constructors->clear();
    observed_receiver_construction = 0;
    receiver_is_parameter_storage =
        cblc_find_method_parameter_storage_by_cobol(unit,
            receiver->cobol_name) != NULL;
    receiver_is_based_alias = std::strncmp(receiver->source_name,
        "CBLC-SPTR-", sizeof("CBLC-SPTR-") - 1) == 0;
    const auto inspect_statement = [&](const t_cblc_statement *statement)
    {
        const t_cblc_constructor *selected_constructor;
        int constructs_receiver;

        constructs_receiver = cblc_statement_constructs_receiver(unit,
            receiver, type, statement);
        if (!constructs_receiver)
            return ;
        observed_receiver_construction = 1;
        if (statement->is_relocate_construct)
        {
            const t_cblc_constructor *move_constructor =
                cblc_find_move_constructor(type);

            selected_constructor = move_constructor
                    && move_constructor->has_definition
                    && !cblc_constructor_may_throw(unit, type,
                        move_constructor, 0)
                ? move_constructor : cblc_find_copy_constructor(type);
        }
        else if (statement->is_move_construct)
            selected_constructor = cblc_find_move_constructor(type);
        else
            selected_constructor = cblc_find_constructor_for_arguments(unit, type,
                statement->call_arguments, statement->call_argument_count);
        if (selected_constructor)
            used_constructors->insert(selected_constructor);
    };
    for (size_t function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *function = &unit->functions[function_index];

        for (size_t statement_index = 0;
            statement_index < function->statement_count; ++statement_index)
            inspect_statement(&function->statements[statement_index]);
    }
    for (size_t type_index = 0; type_index < unit->struct_type_count;
        ++type_index)
    {
        const t_cblc_struct_type *owner_type = &unit->struct_types[type_index];

        for (size_t method_index = 0; method_index < owner_type->method_count;
            ++method_index)
        {
            const t_cblc_method *method = &owner_type->methods[method_index];

            for (size_t statement_index = 0;
                statement_index < method->statement_count; ++statement_index)
                inspect_statement(&method->statements[statement_index]);
        }
        for (size_t constructor_index = 0;
            constructor_index < owner_type->constructor_count; ++constructor_index)
        {
            const t_cblc_constructor *owner_constructor =
                &owner_type->constructors[constructor_index];

            for (size_t statement_index = 0;
                statement_index < owner_constructor->statement_count;
                ++statement_index)
                inspect_statement(&owner_constructor->statements[statement_index]);
        }
    }
    if (receiver->constructor_argument_count > 0)
    {
        const t_cblc_constructor *selected_constructor;

        observed_receiver_construction = 1;
        selected_constructor = cblc_find_constructor_for_arguments(unit, type,
                receiver->constructor_arguments,
                receiver->constructor_argument_count);
        if (selected_constructor)
            used_constructors->insert(selected_constructor);
    }
    /* Some compiler-generated object lifetimes have no source constructor
     * statement to inspect. Keep the conservative all-constructors behavior
     * for owned storage, but do not treat borrowed by-value parameter storage
     * or based aliases of template instances as if every overload were used.
     * Their concrete object lifetime belongs to another owner; only
     * constructors selected by reachable source operations need paragraphs. */
    if (receiver_is_based_alias)
    {
        /* Generic methods can default-construct an element through a based
         * storage alias without a source-level construction targeting that
         * alias. Keep its concrete default-constructor paragraph reachable. */
        const t_cblc_constructor *default_constructor =
            cblc_find_constructor_for_arguments(unit, type, NULL, 0);

        if (default_constructor && default_constructor->has_definition
            && default_constructor->statement_count > 0)
            used_constructors->insert(default_constructor);
    }
    if (!observed_receiver_construction && !receiver_is_parameter_storage
        && !(receiver_is_based_alias && type->is_template_instantiation))
        for (size_t constructor_index = 0;
            constructor_index < type->constructor_count; ++constructor_index)
            used_constructors->insert(&type->constructors[constructor_index]);
    return (FT_SUCCESS);
}

static int cblc_method_is_called_for_receiver_in_functions(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_struct_type *type, const t_cblc_method *requested_method)
{
    if (!unit || !receiver || !type || !requested_method)
        return (0);
    /* CBLC-SPTR-* names are compiler-created BASED aliases. Source statements
     * can never call a method through those aliases; their reachable methods
     * are discovered from the concrete constructor/method bodies below. */
    if (std::strncmp(receiver->source_name, "CBLC-SPTR-",
            sizeof("CBLC-SPTR-") - 1) == 0)
        return (0);
    for (size_t function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *function = &unit->functions[function_index];

        for (size_t statement_index = 0;
            statement_index < function->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement = &function->statements[statement_index];
            const char *receiver_name;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            if (std::strcmp(statement->call_identifier,
                    requested_method->source_name) != 0)
                continue ;
            receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
                ? statement->target : statement->source;
            if ((std::strcmp(receiver_name, receiver->cobol_name) == 0
                    || std::strcmp(receiver_name, receiver->source_name) == 0
                    || (receiver->local_source_name[0] != '\0'
                        && std::strcmp(receiver_name,
                            receiver->local_source_name) == 0))
                && cblc_statement_calls_method(unit, type, statement,
                    requested_method))
                return (1);
        }
    }
    return (0);
}

static void cblc_collect_type_method_references(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_struct_type *type,
    std::unordered_set<const t_cblc_method *> *references,
    const t_cblc_constructor *extra_constructor = NULL)
{
    std::vector<const t_cblc_method *> pending;
    std::unordered_set<const t_cblc_constructor *> used_constructors;

    if (!unit || !receiver || !type || !references)
        return ;
    if (cblc_collect_used_constructors_for_receiver(unit, receiver, type,
            &used_constructors) != FT_SUCCESS)
        return ;
    for (size_t method_index = 0; method_index < type->method_count;
        ++method_index)
    {
        const t_cblc_method *method = &type->methods[method_index];

        if (cblc_method_is_called_for_receiver_in_functions(unit, receiver,
                type, method)
            && references->insert(method).second)
            pending.push_back(method);
    }
    for (size_t constructor_index = 0;
        constructor_index < type->constructor_count; ++constructor_index)
    {
        const t_cblc_constructor *constructor =
            &type->constructors[constructor_index];

        if (used_constructors.find(constructor) == used_constructors.end())
            continue ;

        for (size_t statement_index = 0;
            statement_index < constructor->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement =
                &constructor->statements[statement_index];
            const t_cblc_method *called_method;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            called_method = cblc_find_method_on_type_for_arguments(unit, type,
                statement->call_identifier, statement->call_arguments,
                statement->call_argument_count);
            if (!called_method)
                called_method = cblc_find_unique_method_on_type_for_arity(type,
                    statement->call_identifier,
                    statement->call_argument_count);
            if (called_method && references->insert(called_method).second)
                pending.push_back(called_method);
        }
    }
    if (extra_constructor)
    {
        for (size_t statement_index = 0;
            statement_index < extra_constructor->statement_count;
            ++statement_index)
        {
            const t_cblc_statement *statement =
                &extra_constructor->statements[statement_index];
            const t_cblc_method *called_method;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            called_method = cblc_find_method_on_type_for_arguments(unit, type,
                statement->call_identifier, statement->call_arguments,
                statement->call_argument_count);
            if (!called_method)
                called_method = cblc_find_unique_method_on_type_for_arity(type,
                    statement->call_identifier,
                    statement->call_argument_count);
            if (called_method && references->insert(called_method).second)
                pending.push_back(called_method);
        }
    }
    for (size_t statement_index = 0;
        statement_index < type->destructor_statement_count; ++statement_index)
    {
        const t_cblc_statement *statement =
            &type->destructor_statements[statement_index];
        const t_cblc_method *called_method;

        if (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
            continue ;
        called_method = cblc_find_method_on_type_for_arguments(unit, type,
            statement->call_identifier, statement->call_arguments,
            statement->call_argument_count);
        if (!called_method)
            called_method = cblc_find_unique_method_on_type_for_arity(type,
                statement->call_identifier,
                statement->call_argument_count);
        if (called_method && references->insert(called_method).second)
            pending.push_back(called_method);
    }
    while (!pending.empty())
    {
        const t_cblc_method *method = pending.back();

        pending.pop_back();
        for (size_t statement_index = 0;
            statement_index < method->statement_count; ++statement_index)
        {
            const t_cblc_statement *statement = &method->statements[statement_index];
            const t_cblc_method *called_method;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            called_method = cblc_find_method_on_type_for_arguments(unit, type,
                statement->call_identifier, statement->call_arguments,
                statement->call_argument_count);
            if (!called_method)
                called_method = cblc_find_unique_method_on_type_for_arity(type,
                    statement->call_identifier,
                    statement->call_argument_count);
            if (called_method && references->insert(called_method).second)
                pending.push_back(called_method);
        }
    }
}

static int cblc_statement_calls_method(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const t_cblc_statement *statement,
    const t_cblc_method *requested_method)
{
    const t_cblc_method *resolved_method;

    if (!unit || !type || !statement || !requested_method
        || (statement->type != CBLC_STATEMENT_METHOD_CALL
            && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN))
        return (0);
    resolved_method = cblc_find_method_on_type_for_arguments(unit, type,
        statement->call_identifier, statement->call_arguments,
        statement->call_argument_count);
    return (resolved_method == requested_method);
}

static int cblc_method_is_referenced_for_receiver(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_struct_type *type, const t_cblc_method *requested_method)
{
    size_t function_index;
    size_t method_index;

    if (!unit || !receiver || !type || !requested_method)
        return (0);
    for (function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *function = &unit->functions[function_index];
        size_t statement_index;

        for (statement_index = 0; statement_index < function->statement_count;
            ++statement_index)
        {
            const t_cblc_statement *statement =
                &function->statements[statement_index];
            const char *receiver_name;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
                ? statement->target : statement->source;
            if ((std::strcmp(receiver_name, receiver->cobol_name) == 0
                    || std::strcmp(receiver_name, receiver->source_name) == 0
                    || (receiver->local_source_name[0] != '\0'
                        && std::strcmp(receiver_name,
                            receiver->local_source_name) == 0))
                && cblc_statement_calls_method(unit, type, statement,
                    requested_method))
                return (1);
        }
    }
    for (method_index = 0; method_index < type->method_count; ++method_index)
    {
        const t_cblc_method *owner_method = &type->methods[method_index];
        size_t statement_index;

        for (statement_index = 0; statement_index < owner_method->statement_count;
            ++statement_index)
            if (cblc_statement_calls_method(unit, type,
                    &owner_method->statements[statement_index], requested_method))
                return (1);
    }
    for (method_index = 0; method_index < type->constructor_count; ++method_index)
    {
        const t_cblc_constructor *constructor = &type->constructors[method_index];
        size_t statement_index;

        for (statement_index = 0; statement_index < constructor->statement_count;
            ++statement_index)
            if (cblc_statement_calls_method(unit, type,
                    &constructor->statements[statement_index], requested_method))
                return (1);
    }
    for (method_index = 0; method_index < type->destructor_statement_count;
        ++method_index)
        if (cblc_statement_calls_method(unit, type,
                &type->destructor_statements[method_index], requested_method))
            return (1);
    return (0);
}

static int cblc_emit_reusable_method_paragraphs(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;
    std::unordered_set<std::string> emitted_method_paragraphs;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *data_item;
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        size_t method_index;

        data_item = &unit->data_items[data_index];
        receiver = data_item;
        if (data_item->is_alias || data_item->array_count > 0
            || data_item->kind != CBLC_DATA_KIND_STRUCT
            || data_item->is_template_item
            || (!data_item->is_function_local
                && data_item->owner_function_name[0] != '\0'
                && !cblc_find_method_parameter_storage_by_cobol(unit,
                    data_item->cobol_name)))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        std::unordered_set<const t_cblc_method *> type_method_references;

        cblc_collect_type_method_references(unit, receiver, type,
            &type_method_references);
        method_index = 0;
        while (method_index < type->method_count)
        {
            const t_cblc_method *method;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];

            method = &type->methods[method_index];
            if (!cblc_method_is_reusable(method)
                || (!cblc_method_is_called_for_receiver_in_functions(unit,
                        receiver, type, method)
                    && type_method_references.find(method)
                        == type_method_references.end())
                || ((!data_item->is_function_local
                        && data_item->owner_function_name[0] != '\0')
                    && !cblc_method_parameter_storage_uses_method(unit,
                        data_item, method)))
            {
                method_index += 1;
                continue ;
            }
            if (cblc_method_paragraph_name(unit, receiver, method, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (builder->emitted_method_paragraphs
                && builder->emitted_method_paragraphs->find(paragraph_name)
                    != builder->emitted_method_paragraphs->end())
            {
                emitted_method_paragraphs.insert(paragraph_name);
                method_index += 1;
                continue ;
            }
            if (builder->known_paragraph_definitions
                ? builder->known_paragraph_definitions->find(paragraph_name)
                    != builder->known_paragraph_definitions->end()
                : ((builder->data
                        && cblc_cobol_text_has_paragraph_definition(builder->data,
                            paragraph_name))
                    || (builder->deferred_procedure_text
                        && cblc_cobol_text_has_paragraph_definition(
                            builder->deferred_procedure_text, paragraph_name))))
            {
                emitted_method_paragraphs.insert(paragraph_name);
                if (builder->emitted_method_paragraphs)
                    builder->emitted_method_paragraphs->insert(paragraph_name);
                method_index += 1;
                continue ;
            }
            if (!emitted_method_paragraphs.insert(paragraph_name).second)
            {
                method_index += 1;
                continue ;
            }
            {
                char result_name[TRANSPILE_IDENTIFIER_MAX];
                char return_name[TRANSPILE_IDENTIFIER_MAX];
                const char *assign_target;

                assign_target = NULL;
                if (cblc_method_return_paragraph_name(unit, receiver, method,
                        return_name, sizeof(return_name)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (method->return_kind != CBLC_FUNCTION_RETURN_VOID)
                {
                    if (cblc_method_result_name(receiver, method, result_name,
                            sizeof(result_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    assign_target = result_name;
                }
                if (cblc_method_may_throw(unit, type, method, 0))
                {
                    if (cblc_emit_method_exception_body(unit, receiver, method,
                            assign_target ? assign_target : "", builder, 0) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed to emit exception-aware method '%s::%s'",
                            receiver->struct_type_name, method->source_name);
                        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                        return (FT_FAILURE);
                    }
                }
                else
                {
                    if (builder->emitted_method_paragraphs)
                        builder->emitted_method_paragraphs->insert(paragraph_name);
                    if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_emit_method_body(unit, receiver, method, assign_target,
                            builder) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed to emit method body '%s::%s'",
                            receiver->struct_type_name, method->source_name);
                        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                        return (FT_FAILURE);
                    }

                    if (cobol_text_builder_append_line(builder, "           EXIT.")
                            != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "       %s.",
                            return_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           EXIT.") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            method_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_unit_uses_exceptions(const t_cblc_translation_unit *unit)
{
    size_t function_index;
    size_t type_index;

    if (!unit)
        return (0);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < unit->functions[function_index].statement_count)
        {
            t_cblc_statement_type type;

            type = unit->functions[function_index].statements[statement_index].type;
            if (type == CBLC_STATEMENT_TRY_BEGIN
                || type == CBLC_STATEMENT_CATCH_BEGIN
                || type == CBLC_STATEMENT_THROW)
                return (1);
            if ((type == CBLC_STATEMENT_CALL || type == CBLC_STATEMENT_CALL_ASSIGN)
                && cblc_statement_call_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index]))
                return (1);
            if (cblc_statement_method_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index])
                || cblc_statement_constructor_may_throw(unit,
                    &unit->functions[function_index].statements[statement_index]))
                return (1);
            statement_index += 1;
        }
        function_index += 1;
    }
    type_index = 0;
    while (type_index < unit->struct_type_count)
    {
        size_t constructor_index;
        size_t method_index;

        constructor_index = 0;
        while (constructor_index < unit->struct_types[type_index].constructor_count)
        {
            const t_cblc_constructor *constructor;
            size_t statement_index;

            constructor = &unit->struct_types[type_index].constructors[constructor_index];
            statement_index = 0;
            while (statement_index < constructor->statement_count)
            {
                const t_cblc_statement *statement;

                statement = &constructor->statements[statement_index];
                if (statement->type == CBLC_STATEMENT_TRY_BEGIN
                    || statement->type == CBLC_STATEMENT_CATCH_BEGIN
                    || statement->type == CBLC_STATEMENT_THROW
                    || statement->exception_may_throw
                    || cblc_statement_method_may_throw(unit, statement)
                    || cblc_statement_constructor_may_throw(unit, statement))
                    return (1);
                statement_index += 1;
            }
            constructor_index += 1;
        }
        method_index = 0;
        while (method_index < unit->struct_types[type_index].method_count)
        {
            if (cblc_method_has_exception_control(
                    &unit->struct_types[type_index].methods[method_index]))
                return (1);
            method_index += 1;
        }
        if (unit->struct_types[type_index].destructor_statement_count > 0)
        {
            size_t statement_index;

            statement_index = 0;
            while (statement_index < unit->struct_types[type_index].destructor_statement_count)
            {
                t_cblc_statement_type type;

                type = unit->struct_types[type_index].destructor_statements[statement_index].type;
                if (type == CBLC_STATEMENT_THROW
                    || unit->struct_types[type_index].destructor_statements[statement_index].exception_may_throw)
                    return (1);
                statement_index += 1;
            }
        }
        type_index += 1;
    }
    return (0);
}

static int cblc_unit_uses_cleanup_guard(const t_cblc_translation_unit *unit)
{
    size_t function_index;

    if (!unit)
        return (0);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        size_t statement_index;

        statement_index = 0;
        while (statement_index < unit->functions[function_index].statement_count)
        {
            if (unit->functions[function_index].statements[statement_index]
                    .exception_cleanup_count > 0)
                return (1);
            statement_index += 1;
        }
        function_index += 1;
    }
    return (0);
}

static int cblc_struct_pointer_layout_seen(
    std::unordered_set<std::string> *layout_keys,
    const t_cblc_data_item *item)
{
    std::string key;

    if (!layout_keys || !item)
        return (0);
    key.assign(item->cobol_name);
    key.push_back('\x1f');
    key.append(item->struct_type_name);
    if (layout_keys->find(key) != layout_keys->end())
        return (1);
    layout_keys->insert(key);
    return (0);
}

static int cblc_emit_referenced_void_methods_for_receiver(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_struct_type *type, t_cobol_text_builder *builder,
    const t_cblc_constructor *extra_constructor = NULL)
{
    size_t method_index;
    std::unordered_set<const t_cblc_method *> reachable_methods;

    if (!unit || !receiver || !type || !builder)
        return (FT_FAILURE);
    if (type->is_template_instantiation || extra_constructor)
        cblc_collect_type_method_references(unit, receiver, type,
            &reachable_methods, extra_constructor);
    for (method_index = 0; method_index < type->method_count; ++method_index)
    {
        const t_cblc_method *method = &type->methods[method_index];
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        char return_name[TRANSPILE_IDENTIFIER_MAX];
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        if (!cblc_method_is_reusable(method)
            || method->return_kind != CBLC_FUNCTION_RETURN_VOID
            || ((type->is_template_instantiation || extra_constructor)
                ? (!cblc_method_is_called_for_receiver_in_functions(unit,
                        receiver, type, method)
                    && reachable_methods.find(method) == reachable_methods.end())
                : !cblc_method_is_referenced_for_receiver(unit, receiver, type,
                    method)))
            continue ;
        if (cblc_method_paragraph_name(unit, receiver, method, paragraph_name,
                sizeof(paragraph_name)) != FT_SUCCESS
            || cblc_method_return_paragraph_name(unit, receiver, method,
                return_name, sizeof(return_name)) != FT_SUCCESS
            || paragraph_name[0] == '\0')
            return (FT_FAILURE);
        if (builder->emitted_method_paragraphs
            && builder->emitted_method_paragraphs->find(paragraph_name)
                != builder->emitted_method_paragraphs->end())
            continue ;
        if (builder->known_paragraph_definitions
            ? builder->known_paragraph_definitions->find(paragraph_name)
                != builder->known_paragraph_definitions->end()
            : ((builder->data
                    && cblc_cobol_text_has_paragraph_definition(builder->data,
                        paragraph_name))
                || (builder->deferred_procedure_text
                    && cblc_cobol_text_has_paragraph_definition(
                        builder->deferred_procedure_text, paragraph_name))))
        {
            if (builder->emitted_method_paragraphs)
                builder->emitted_method_paragraphs->insert(paragraph_name);
            continue ;
        }
        if (cblc_method_may_throw(unit, type, method, 0))
        {
            if (cblc_emit_method_exception_body(unit, receiver, method, "",
                    builder, 0) != FT_SUCCESS)
            {
                std::snprintf(diagnostic, sizeof(diagnostic),
                    "failed to emit referenced method '%s::%s' for based receiver '%s'",
                    type->source_name, method->source_name,
                    receiver->cobol_name);
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                return (FT_FAILURE);
            }
        }
        else if (std::snprintf(diagnostic, sizeof(diagnostic), "       %s.",
                    paragraph_name) < 0
            || (builder->emitted_method_paragraphs
                && !builder->emitted_method_paragraphs->insert(paragraph_name).second)
            || cobol_text_builder_append_line(builder, diagnostic) != FT_SUCCESS
            || cblc_emit_method_body(unit, receiver, method, NULL, builder)
                != FT_SUCCESS
            || cobol_text_builder_append_line(builder,
                "           EXIT.") != FT_SUCCESS
            || std::snprintf(diagnostic, sizeof(diagnostic), "       %s.",
                return_name) < 0
            || cobol_text_builder_append_line(builder, diagnostic) != FT_SUCCESS
            || cobol_text_builder_append_line(builder,
                "           EXIT.") != FT_SUCCESS
            || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        {
            char failure[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(failure, sizeof(failure),
                "failed to emit referenced method '%s::%s' for based receiver '%s'",
                type->source_name, method->source_name,
                receiver->cobol_name);
            cobol_text_builder_set_diagnostic(builder, 0, failure);
            return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int cblc_emit_nested_pointer_lifecycle_paragraphs(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *prefix, size_t depth, t_cobol_text_builder *builder)
{
    size_t field_index;
    std::unordered_set<const t_cblc_constructor *> used_constructors;
    if (!unit || !type || !prefix || !builder
        || depth > CBLC_TEMPLATE_DEFAULT_MAX_DEPTH)
        return (FT_FAILURE);
    field_index = 0;
    while (field_index < type->field_count)
    {
        const t_cblc_struct_field *field;
        const t_cblc_struct_type *field_type;
        char field_path[TRANSPILE_STATEMENT_TEXT_MAX];

        field = &type->fields[field_index];
        if (std::snprintf(field_path, sizeof(field_path), "%s-%s", prefix,
                field->cobol_name) < 0
            || std::strlen(field_path) >= sizeof(field_path))
            return (FT_FAILURE);
        if (field->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
            t_cblc_data_item based_receiver;
            const t_cblc_struct_type *pointee_type;
            size_t constructor_index;

            pointee_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!pointee_type
                || cblc_build_struct_pointer_based_name(unit, field_path,
                    field->struct_type_name, based_name, sizeof(based_name))
                    != FT_SUCCESS)
                return (FT_FAILURE);
            std::memset(&based_receiver, 0, sizeof(based_receiver));
            based_receiver.kind = CBLC_DATA_KIND_STRUCT;
            ft_strlcpy(based_receiver.source_name, based_name,
                sizeof(based_receiver.source_name));
            ft_strlcpy(based_receiver.cobol_name, based_name,
                sizeof(based_receiver.cobol_name));
            ft_strlcpy(based_receiver.struct_type_name,
                field->struct_type_name, sizeof(based_receiver.struct_type_name));
            if (cblc_collect_used_constructors_for_receiver(unit,
                    &based_receiver, pointee_type, &used_constructors)
                != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_emit_referenced_void_methods_for_receiver(unit,
                    &based_receiver, pointee_type, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            constructor_index = 0;
            while (constructor_index < pointee_type->constructor_count)
            {
                const t_cblc_constructor *constructor;
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char return_name[TRANSPILE_IDENTIFIER_MAX];
                char line[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t statement_index;
                int control_flow_depth;

                constructor = &pointee_type->constructors[constructor_index];
                if (!constructor->has_definition || constructor->statement_count == 0)
                {
                    constructor_index += 1;
                    continue ;
                }
                /* A nested based receiver can have several constructor
                 * overloads even though its reachable callers select only
                 * one. Avoid generating dead overload bodies when that use
                 * is explicit in the AST. The helper deliberately falls back
                 * to "used" when it cannot observe a construction, preserving
                 * all lifecycle paragraphs for compiler-generated paths. */
                if (used_constructors.find(constructor)
                    == used_constructors.end())
                {
                    constructor_index += 1;
                    continue ;
                }
                if (cblc_constructor_paragraph_name(&based_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS
                    || cblc_constructor_return_paragraph_name(unit,
                        &based_receiver, constructor_index, return_name,
                        sizeof(return_name)) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "failed to name nested pointer-backed constructor %zu for type '%s' at '%s'",
                        constructor_index, pointee_type->source_name,
                        based_receiver.cobol_name);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
                if (builder->known_paragraph_definitions
                    && builder->known_paragraph_definitions->find(paragraph_name)
                        != builder->known_paragraph_definitions->end())
                {
                    constructor_index += 1;
                    continue ;
                }
                if (cblc_constructor_has_exception_control(constructor))
                {
                    if (cblc_emit_constructor_exception_body(unit,
                            &based_receiver, constructor, constructor_index,
                            builder) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed nested pointer-backed constructor %zu for type '%s' at '%s'",
                            constructor_index, pointee_type->source_name,
                            based_receiver.cobol_name);
                        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                        return (FT_FAILURE);
                    }
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "       %s.",
                            paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    control_flow_depth = 0;
                    statement_index = 0;
                    while (statement_index < constructor->statement_count)
                    {
                        if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                                &constructor->statements[statement_index],
                                based_receiver.cobol_name, builder,
                                &control_flow_depth, &based_receiver) != FT_SUCCESS)
                        {
                            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            std::snprintf(diagnostic, sizeof(diagnostic),
                                "failed nested pointer-backed constructor statement %zu (kind %d, call '%s', target '%s') for type '%s' at '%s'",
                                statement_index,
                                static_cast<int>(constructor->statements[statement_index].type),
                                constructor->statements[statement_index].call_identifier,
                                constructor->statements[statement_index].target,
                                pointee_type->source_name, based_receiver.cobol_name);
                            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                            return (FT_FAILURE);
                        }
                        statement_index += 1;
                    }
                    if (control_flow_depth != 0
                        || std::snprintf(line, sizeof(line), "       %s.",
                            return_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           EXIT.") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                    return (FT_FAILURE);
                constructor_index += 1;
            }
            if (cblc_emit_nested_pointer_lifecycle_paragraphs(unit, pointee_type,
                    field_path, depth + 1, builder) != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "failed to recurse into nested pointer lifecycle for field '%s' type '%s'",
                    field_path, pointee_type->source_name);
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                return (FT_FAILURE);
            }
        }
        else if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            field_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!field_type
                || cblc_emit_nested_pointer_lifecycle_paragraphs(unit, field_type,
                    field_path, depth + 1, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        field_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_method_result_is_used_on_data_item(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *owner,
    const t_cblc_method *method)
{
    const t_cblc_struct_type *owner_type;
    size_t function_index;

    if (!unit || !owner || !method)
        return (0);
    owner_type = cblc_find_struct_type(unit, owner->struct_type_name);
    if (owner_type && cblc_method_is_referenced_for_receiver(unit, owner,
            owner_type, method))
        return (1);
    for (function_index = 0; function_index < unit->function_count;
        ++function_index)
    {
        const t_cblc_function *function = &unit->functions[function_index];
        size_t statement_index;

        for (statement_index = 0; statement_index < function->statement_count;
            ++statement_index)
        {
            const t_cblc_statement *statement =
                &function->statements[statement_index];
            const char *receiver_name;

            if (statement->type != CBLC_STATEMENT_METHOD_CALL
                && statement->type != CBLC_STATEMENT_METHOD_CALL_ASSIGN)
                continue ;
            if (std::strcmp(statement->call_identifier, method->source_name) != 0)
                continue ;
            receiver_name = statement->type == CBLC_STATEMENT_METHOD_CALL
                ? statement->target : statement->source;
            if (std::strcmp(receiver_name, owner->cobol_name) == 0
                || std::strcmp(receiver_name, owner->source_name) == 0
                || (owner->local_source_name[0] != '\0'
                    && std::strcmp(receiver_name, owner->local_source_name) == 0))
                return (1);
        }
    }
    return (0);
}

static int cblc_emit_method_result_lifecycle_paragraphs(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    std::unordered_set<std::string> emitted_results;
    size_t data_index;

    if (!unit || !builder)
        return (FT_FAILURE);
    for (data_index = 0; data_index < unit->data_count; ++data_index)
    {
        const t_cblc_data_item *owner = &unit->data_items[data_index];
        const t_cblc_struct_type *owner_type;
        std::unordered_set<const t_cblc_method *> reachable_methods;
        size_t method_index;

        if (owner->is_alias || owner->kind != CBLC_DATA_KIND_STRUCT
            || owner->array_count > 0
            || (!owner->is_function_local
                && owner->owner_function_name[0] != '\0'))
            continue ;
        owner_type = cblc_find_struct_type(unit, owner->struct_type_name);
        if (!owner_type || owner_type->is_builtin)
            continue ;
        if (owner_type->is_template_instantiation)
            cblc_collect_type_method_references(unit, owner, owner_type,
                &reachable_methods);
        for (method_index = 0; method_index < owner_type->method_count;
            ++method_index)
        {
            const t_cblc_method *method = &owner_type->methods[method_index];
            const t_cblc_struct_type *result_type;
            t_cblc_data_item result_receiver;
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            size_t constructor_index;
            int result_is_reachable;

            result_is_reachable = cblc_method_result_is_used_on_data_item(unit,
                owner, method)
                || (owner_type->is_template_instantiation
                    && reachable_methods.find(method) != reachable_methods.end());
            if (!cblc_method_is_reusable(method)
                || method->return_kind != CBLC_FUNCTION_RETURN_STRUCT
                || !result_is_reachable
                || cblc_method_result_name(owner, method, result_name,
                    sizeof(result_name)) != FT_SUCCESS
                || !emitted_results.insert(result_name).second)
                continue ;
            result_type = cblc_find_struct_type(unit, method->return_type_name);
            if (!result_type || result_type->is_builtin)
                continue ;
            std::memset(&result_receiver, 0, sizeof(result_receiver));
            result_receiver.kind = CBLC_DATA_KIND_STRUCT;
            ft_strlcpy(result_receiver.source_name, result_name,
                sizeof(result_receiver.source_name));
            ft_strlcpy(result_receiver.cobol_name, result_name,
                sizeof(result_receiver.cobol_name));
            ft_strlcpy(result_receiver.struct_type_name, result_type->source_name,
                sizeof(result_receiver.struct_type_name));
            constructor_index = 0;
            while (constructor_index < result_type->constructor_count)
            {
                const t_cblc_constructor *constructor =
                    &result_type->constructors[constructor_index];
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char return_name[TRANSPILE_IDENTIFIER_MAX];
                char line[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t statement_index;
                int control_flow_depth;

                if (!constructor->has_definition
                    || constructor->statement_count == 0)
                {
                    constructor_index += 1;
                    continue ;
                }
                if (cblc_constructor_paragraph_name(&result_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS
                    || cblc_constructor_return_paragraph_name(unit,
                        &result_receiver, constructor_index, return_name,
                        sizeof(return_name)) != FT_SUCCESS)
                {
                    cobol_text_builder_set_diagnostic(builder, 0,
                        "failed to name constructor lifecycle paragraphs for a method result");
                    return (FT_FAILURE);
                }
                if (cblc_constructor_has_exception_control(constructor))
                {
                    if (cblc_emit_constructor_exception_body(unit,
                            &result_receiver, constructor, constructor_index,
                            builder) != FT_SUCCESS)
                    {
                        cobol_text_builder_set_diagnostic(builder, 0,
                            "failed to emit exception-aware constructor for a method result");
                        return (FT_FAILURE);
                    }
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "       %s.",
                            paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                    control_flow_depth = 0;
                    for (statement_index = 0;
                        statement_index < constructor->statement_count;
                        ++statement_index)
                        if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                                &constructor->statements[statement_index],
                                result_receiver.cobol_name, builder,
                                &control_flow_depth, &result_receiver)
                            != FT_SUCCESS)
                        {
                            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            std::snprintf(diagnostic, sizeof(diagnostic),
                                "failed to emit constructor statement %zu for method result type '%s'",
                                statement_index, result_type->source_name);
                            cobol_text_builder_set_diagnostic(builder, 0,
                                diagnostic);
                            return (FT_FAILURE);
                        }
                    if (control_flow_depth != 0
                        || std::snprintf(line, sizeof(line), "       %s.",
                            return_name) < 0
                        || cobol_text_builder_append_line(builder, line)
                            != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           EXIT.") != FT_SUCCESS)
                    {
                        cobol_text_builder_set_diagnostic(builder, 0,
                            "failed to close constructor paragraph for a method result");
                        return (FT_FAILURE);
                    }
                }
                if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                    return (FT_FAILURE);
                constructor_index += 1;
            }
            if (result_type->destructor_statement_count > 0)
            {
                char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                char line[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t statement_index;
                int control_flow_depth;

                if (cblc_destructor_paragraph_name(&result_receiver,
                        paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line), "       %s.",
                        paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line)
                        != FT_SUCCESS)
                    return (FT_FAILURE);
                control_flow_depth = 0;
                for (statement_index = 0;
                    statement_index < result_type->destructor_statement_count;
                    ++statement_index)
                    if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                            &result_type->destructor_statements[statement_index],
                            result_receiver.cobol_name, builder,
                            &control_flow_depth, &result_receiver) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed to emit destructor statement %zu for method result type '%s'",
                            statement_index, result_type->source_name);
                        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                        return (FT_FAILURE);
                    }
                if (control_flow_depth != 0
                    || cobol_text_builder_append_line(builder,
                        "           EXIT.") != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cblc_emit_nested_pointer_lifecycle_paragraphs(unit, result_type,
                    result_receiver.cobol_name, 0, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    return (FT_SUCCESS);
}

static int cblc_lifecycle_storage_owner_is_referenced(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *item,
    std::unordered_map<std::string, int> *reachability_cache)
{
    size_t index;

    if (!unit || !item || item->owner_function_name[0] == '\0')
        return (1);
    if (reachability_cache)
    {
        const auto cached = reachability_cache->find(item->owner_function_name);

        if (cached != reachability_cache->end())
            return (cached->second);
    }
    const auto cache_result = [&](int referenced)
    {
        if (reachability_cache)
            reachability_cache->emplace(item->owner_function_name, referenced);
        return referenced;
    };
    for (index = 0; index < unit->function_count; ++index)
        if (std::strcmp(unit->functions[index].source_name,
                item->owner_function_name) == 0)
            return (cache_result(1));
    for (index = 0; index < unit->struct_type_count; ++index)
    {
        const t_cblc_struct_type *owner_type = &unit->struct_types[index];
        size_t method_index;

        method_index = 0;
        while (method_index < owner_type->method_count)
        {
            const t_cblc_method *method = &owner_type->methods[method_index];

            if (std::strcmp(method->scope_source_name,
                    item->owner_function_name) == 0)
            {
                size_t receiver_index;
                t_cblc_data_item synthetic_receiver;

                receiver_index = 0;
                while (receiver_index < unit->data_count)
                {
                    const t_cblc_data_item *receiver =
                        &unit->data_items[receiver_index];

                    if ((receiver->kind == CBLC_DATA_KIND_STRUCT
                            || receiver->kind == CBLC_DATA_KIND_STRUCT_POINTER)
                        && std::strcmp(receiver->struct_type_name,
                            owner_type->source_name) == 0)
                    {
                        if (owner_type->is_template_instantiation)
                        {
                            std::unordered_set<const t_cblc_method *> reachable_methods;

                            cblc_collect_type_method_references(unit, receiver,
                                owner_type, &reachable_methods);
                            if (reachable_methods.find(method)
                                != reachable_methods.end())
                                return (cache_result(1));
                        }
                        else if (cblc_method_is_referenced_for_receiver(unit,
                                receiver, owner_type, method))
                            return (cache_result(1));
                    }
                    receiver_index += 1;
                }
                std::memset(&synthetic_receiver, 0, sizeof(synthetic_receiver));
                synthetic_receiver.kind = CBLC_DATA_KIND_STRUCT;
                ft_strlcpy(synthetic_receiver.source_name,
                    owner_type->source_name,
                    sizeof(synthetic_receiver.source_name));
                ft_strlcpy(synthetic_receiver.cobol_name,
                    owner_type->cobol_name,
                    sizeof(synthetic_receiver.cobol_name));
                ft_strlcpy(synthetic_receiver.struct_type_name,
                    owner_type->source_name,
                    sizeof(synthetic_receiver.struct_type_name));
                if (owner_type->is_template_instantiation)
                {
                    std::unordered_set<const t_cblc_method *> reachable_methods;

                    cblc_collect_type_method_references(unit,
                        &synthetic_receiver, owner_type, &reachable_methods);
                    if (reachable_methods.find(method)
                        != reachable_methods.end())
                        return (cache_result(1));
                }
                else if (cblc_method_is_referenced_for_receiver(unit,
                        &synthetic_receiver, owner_type, method))
                    return (cache_result(1));
                /* Copy assignment can be selected by compiler-generated
                 * lifecycle dispatch without a source-level method-call edge.
                 * Preserve its storage, but do not retain every method in a
                 * concrete specialization. */
                if (owner_type->is_template_instantiation
                    && method->is_copy_assignment)
                    return (cache_result(1));
                return (cache_result(0));
            }
            method_index += 1;
        }
        for (size_t constructor_index = 0;
            constructor_index < owner_type->constructor_count;
            ++constructor_index)
            if (std::strcmp(owner_type->constructors[constructor_index].scope_source_name,
                    item->owner_function_name) == 0)
                return (cache_result(1));
    }
    /* Preserve storage for unknown owners until their ownership is modeled. */
    return (1);
}

static int cblc_emit_reusable_lifecycle_paragraphs(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;
    std::unordered_set<std::string> emitted_pointer_layouts;
    std::unordered_map<std::string, int> lifecycle_owner_reachability;
    const int trace_lifecycle = std::getenv("CTOC_TRACE_COBOL_EMIT") != NULL;
    std::chrono::steady_clock::time_point lifecycle_phase_started;

    if (!unit || !builder)
        return (FT_FAILURE);
    lifecycle_phase_started = std::chrono::steady_clock::now();
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        std::unordered_set<const t_cblc_constructor *> used_constructors;
        size_t constructor_index;

        receiver = &unit->data_items[data_index];
        if (receiver->is_alias || receiver->is_template_item
            || receiver->kind != CBLC_DATA_KIND_STRUCT
            || !cblc_lifecycle_storage_owner_is_referenced(unit, receiver,
                &lifecycle_owner_reachability)
            || (!receiver->is_function_local && receiver->owner_function_name[0] != '\0'
                && !cblc_find_method_parameter_storage_by_cobol(unit,
                    receiver->cobol_name)))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        if (cblc_collect_used_constructors_for_receiver(unit, receiver, type,
                &used_constructors) != FT_SUCCESS)
            return (FT_FAILURE);
        constructor_index = 0;
        while (constructor_index < type->constructor_count)
        {
            const t_cblc_constructor *constructor;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t statement_index;
            int control_flow_depth;

            constructor = &type->constructors[constructor_index];
            t_cobol_text_builder_lifecycle_scope lifecycle_scope(builder, NULL,
                constructor);
            if (!constructor->has_definition || constructor->statement_count == 0
                || used_constructors.find(constructor)
                    == used_constructors.end())
            {
                constructor_index += 1;
                continue ;
            }
            if (cblc_constructor_paragraph_name(receiver, constructor_index,
                    paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS
                || cblc_constructor_return_paragraph_name(unit, receiver,
                    constructor_index, return_paragraph_name,
                    sizeof(return_paragraph_name)) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            if (builder->known_paragraph_definitions
                && builder->known_paragraph_definitions->find(paragraph_name)
                    != builder->known_paragraph_definitions->end())
            {
                constructor_index += 1;
                continue ;
            }
            if (cblc_constructor_has_exception_control(constructor))
            {
                if (cblc_emit_constructor_exception_body(unit, receiver, constructor,
                        constructor_index, builder) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "failed to emit constructor %zu for type '%s' at '%s'",
                        constructor_index, type->source_name,
                        receiver->cobol_name);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                control_flow_depth = 0;
                statement_index = 0;
                while (statement_index < constructor->statement_count)
                {
                    if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                            &constructor->statements[statement_index], receiver->cobol_name,
                            builder, &control_flow_depth, receiver) != FT_SUCCESS)
                    {
                        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                        std::snprintf(diagnostic, sizeof(diagnostic),
                            "failed to emit constructor statement %zu for type '%s' at '%s'",
                            statement_index, type->source_name,
                            receiver->cobol_name);
                        cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                        return (FT_FAILURE);
                    }
                    statement_index += 1;
                }
                if (control_flow_depth != 0)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "       %s.",
                        return_paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                    || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
            constructor_index += 1;
        }
        if (type->destructor_statement_count > 0)
        {
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            char line[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t statement_index;
            int control_flow_depth;

            if (cblc_destructor_paragraph_name(receiver, paragraph_name,
                    sizeof(paragraph_name)) != FT_SUCCESS)
            {
                return (FT_FAILURE);
            }
            if (builder->known_paragraph_definitions
                && builder->known_paragraph_definitions->find(paragraph_name)
                    != builder->known_paragraph_definitions->end())
            {
                data_index += 1;
                continue ;
            }
            if (std::snprintf(line, sizeof(line), "       %s.", paragraph_name) < 0
                || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            control_flow_depth = 0;
            statement_index = 0;
            while (statement_index < type->destructor_statement_count)
            {
                if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                        &type->destructor_statements[statement_index], receiver->cobol_name,
                        builder, &control_flow_depth, receiver) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
                statement_index += 1;
            }
            if (control_flow_depth != 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
                || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        data_index += 1;
    }
    if (trace_lifecycle)
    {
        std::fprintf(stderr,
            "COBOL lifecycle phase value receivers: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - lifecycle_phase_started).count(), builder->length);
        lifecycle_phase_started = std::chrono::steady_clock::now();
    }
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *pointer_item;
        const t_cblc_struct_type *type;
        t_cblc_data_item based_receiver;
        std::unordered_set<const t_cblc_constructor *> used_constructors;
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t constructor_index;

        pointer_item = &unit->data_items[data_index];
        if (pointer_item->is_alias
            || !cblc_lifecycle_storage_owner_is_referenced(unit, pointer_item,
                &lifecycle_owner_reachability)
            || std::strchr(pointer_item->cobol_name, '[')
            || pointer_item->kind != CBLC_DATA_KIND_STRUCT_POINTER
            || cblc_struct_pointer_layout_seen(&emitted_pointer_layouts,
                pointer_item)
            || cblc_build_struct_pointer_based_name(unit, pointer_item->cobol_name,
                pointer_item->struct_type_name, based_name, sizeof(based_name))
                != FT_SUCCESS)
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, pointer_item->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        based_receiver = *pointer_item;
        based_receiver.kind = CBLC_DATA_KIND_STRUCT;
        based_receiver.is_alias = 0;
        ft_strlcpy(based_receiver.declared_type_name,
            based_receiver.struct_type_name,
            sizeof(based_receiver.declared_type_name));
        ft_strlcpy(based_receiver.source_name, based_name,
            sizeof(based_receiver.source_name));
        ft_strlcpy(based_receiver.cobol_name, based_name,
            sizeof(based_receiver.cobol_name));
        if (cblc_collect_used_constructors_for_receiver(unit,
                &based_receiver, type, &used_constructors) != FT_SUCCESS)
            return (FT_FAILURE);
        constructor_index = 0;
        while (constructor_index < type->constructor_count)
        {
            const t_cblc_constructor *constructor;
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];

            constructor = &type->constructors[constructor_index];
            if (constructor->has_definition && constructor->statement_count > 0
                && used_constructors.find(constructor)
                    != used_constructors.end())
            {
                if (cblc_constructor_paragraph_name(&based_receiver,
                        constructor_index, paragraph_name,
                        sizeof(paragraph_name)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (builder->known_paragraph_definitions
                    && builder->known_paragraph_definitions->find(paragraph_name)
                        != builder->known_paragraph_definitions->end())
                {
                    constructor_index += 1;
                    continue ;
                }
                if (cblc_constructor_has_exception_control(constructor))
                {
                    if (cblc_emit_constructor_exception_body(unit, &based_receiver,
                            constructor, constructor_index, builder) != FT_SUCCESS)
                    {
                        if (builder->diagnostic_message[0] == '\0')
                        {
                            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                            std::snprintf(diagnostic, sizeof(diagnostic),
                                "failed to emit pointer-backed constructor %zu for type '%s' at '%s'",
                                constructor_index, type->source_name,
                                based_receiver.cobol_name);
                            cobol_text_builder_set_diagnostic(builder, 0,
                                diagnostic);
                        }
                        return (FT_FAILURE);
                    }
                }
                else
                {
                    char return_paragraph_name[TRANSPILE_IDENTIFIER_MAX];
                    char line[TRANSPILE_STATEMENT_TEXT_MAX];
                    size_t statement_index;
                    int control_flow_depth;

                    if (cblc_constructor_paragraph_name(&based_receiver,
                            constructor_index, paragraph_name,
                            sizeof(paragraph_name)) != FT_SUCCESS
                        || cblc_constructor_return_paragraph_name(unit,
                            &based_receiver, constructor_index,
                            return_paragraph_name,
                            sizeof(return_paragraph_name)) != FT_SUCCESS
                        || std::snprintf(line, sizeof(line), "       %s.",
                            paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                        return (FT_FAILURE);
                    control_flow_depth = 0;
                    statement_index = 0;
                    while (statement_index < constructor->statement_count)
                    {
                        if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                                &constructor->statements[statement_index],
                                based_receiver.cobol_name, builder,
                                &control_flow_depth, &based_receiver) != FT_SUCCESS)
                            return (FT_FAILURE);
                        statement_index += 1;
                    }
                    if (control_flow_depth != 0
                        || std::snprintf(line, sizeof(line), "       %s.",
                            return_paragraph_name) < 0
                        || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
                        || cobol_text_builder_append_line(builder,
                            "           EXIT.") != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            constructor_index += 1;
        }
        if (cblc_emit_nested_pointer_lifecycle_paragraphs(unit, type,
                pointer_item->cobol_name, 0, builder) != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed to emit nested pointer lifecycle for type '%s' at '%s'",
                type->source_name, pointer_item->cobol_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            return (FT_FAILURE);
        }
        data_index += 1;
    }
    if (trace_lifecycle)
    {
        std::fprintf(stderr,
            "COBOL lifecycle phase pointer receivers: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - lifecycle_phase_started).count(), builder->length);
        lifecycle_phase_started = std::chrono::steady_clock::now();
    }
    if (cblc_emit_method_result_lifecycle_paragraphs(unit, builder) != FT_SUCCESS)
    {
        cobol_text_builder_set_diagnostic(builder, 0,
            "failed to emit method-result lifecycle paragraphs");
        return (FT_FAILURE);
    }
    if (trace_lifecycle)
        std::fprintf(stderr,
            "COBOL lifecycle phase method results: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - lifecycle_phase_started).count(), builder->length);
    return (FT_SUCCESS);
}

static int cblc_emit_deferred_lifecycle_constructor_paragraphs(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    size_t request_index;
    size_t scanned_data_length;
    size_t scanned_deferred_length;
    std::unordered_map<std::string,
        t_cblc_lifecycle_constructor_request> candidate_constructors;
    std::unordered_set<std::string> requested_constructors;

    if (!unit || !builder)
        return (FT_FAILURE);
    request_index = 0;
    scanned_data_length = 0;
    scanned_deferred_length = 0;
    for (size_t data_index = 0; data_index < unit->data_count; ++data_index)
    {
        const t_cblc_data_item *item = &unit->data_items[data_index];
        t_cblc_data_item receiver;
        char based_name[TRANSPILE_STATEMENT_TEXT_MAX];
        const t_cblc_struct_type *type;

        if (item->kind == CBLC_DATA_KIND_STRUCT_POINTER)
        {
            if (cblc_build_struct_pointer_based_name(unit, item->cobol_name,
                    item->struct_type_name, based_name, sizeof(based_name))
                != FT_SUCCESS)
                continue ;
            receiver = *item;
            receiver.kind = CBLC_DATA_KIND_STRUCT;
            receiver.is_alias = 0;
            ft_strlcpy(receiver.declared_type_name, receiver.struct_type_name,
                sizeof(receiver.declared_type_name));
            ft_strlcpy(receiver.source_name, based_name,
                sizeof(receiver.source_name));
            ft_strlcpy(receiver.cobol_name, based_name,
                sizeof(receiver.cobol_name));
        }
        else if (item->kind == CBLC_DATA_KIND_STRUCT)
            receiver = *item;
        else
            continue ;
        type = cblc_find_struct_type(unit, receiver.struct_type_name);
        if (!type || type->is_builtin)
            continue ;
        for (size_t constructor_index = 0;
            constructor_index < type->constructor_count; ++constructor_index)
        {
            const t_cblc_constructor *constructor =
                &type->constructors[constructor_index];
            char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
            t_cblc_lifecycle_constructor_request request;

            if (!constructor->has_definition || constructor->statement_count == 0
                || cblc_constructor_paragraph_name(&receiver, constructor_index,
                    paragraph_name, sizeof(paragraph_name)) != FT_SUCCESS)
                continue ;
            request.receiver = receiver;
            request.constructor_index = constructor_index;
            candidate_constructors.emplace(paragraph_name, request);
        }
    }
    for (size_t index = 0;
        index < builder->lifecycle_constructor_requests.size(); ++index)
    {
        const t_cblc_lifecycle_constructor_request *request =
            &builder->lifecycle_constructor_requests[index];
        const t_cblc_struct_type *type = cblc_find_struct_type(unit,
            request->receiver.struct_type_name);
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];

        if (type && request->constructor_index < type->constructor_count
            && cblc_constructor_paragraph_name(&request->receiver,
                request->constructor_index, paragraph_name,
                sizeof(paragraph_name)) == FT_SUCCESS)
            requested_constructors.insert(paragraph_name);
    }
    const auto scan_references = [&](const char *text, size_t length,
        size_t *scan_offset) -> int
    {
        size_t cursor;

        if (!text || !scan_offset || *scan_offset > length)
            return (FT_SUCCESS);
        cursor = *scan_offset;
        while (cursor + sizeof("PERFORM") - 1 <= length)
        {
            size_t token_start;
            size_t token_length;
            std::string paragraph_name;
            auto candidate = candidate_constructors.end();

            if (std::memcmp(text + cursor, "PERFORM",
                    sizeof("PERFORM") - 1) != 0
                || (cursor > 0
                    && (std::isalnum(static_cast<unsigned char>(text[cursor - 1]))
                        || text[cursor - 1] == '-'))
                || (cursor + sizeof("PERFORM") - 1 < length
                    && !std::isspace(static_cast<unsigned char>(
                        text[cursor + sizeof("PERFORM") - 1]))))
            {
                cursor += 1;
                continue ;
            }
            cursor += sizeof("PERFORM") - 1;
            while (cursor < length
                && std::isspace(static_cast<unsigned char>(text[cursor])))
                cursor += 1;
            token_start = cursor;
            while (cursor < length
                && !std::isspace(static_cast<unsigned char>(text[cursor]))
                && text[cursor] != '.')
                cursor += 1;
            token_length = cursor - token_start;
            if (token_length == 0)
            {
                cursor += 1;
                continue ;
            }
            paragraph_name.assign(text + token_start, token_length);
            candidate = candidate_constructors.find(paragraph_name);
            if (candidate != candidate_constructors.end()
                && (!builder->known_paragraph_definitions
                    || builder->known_paragraph_definitions->find(paragraph_name)
                        == builder->known_paragraph_definitions->end())
                && requested_constructors.insert(paragraph_name).second)
                builder->lifecycle_constructor_requests.push_back(
                    candidate->second);
        }
        *scan_offset = length > 32 ? length - 32 : 0;
        return (FT_SUCCESS);
    };
    while (1)
    {
        if (scan_references(builder->data, builder->length,
                &scanned_data_length) != FT_SUCCESS
            || scan_references(builder->deferred_procedure_text,
                builder->deferred_procedure_length,
                &scanned_deferred_length) != FT_SUCCESS)
            return (FT_FAILURE);
        if (request_index >= builder->lifecycle_constructor_requests.size())
            break ;
        const t_cblc_lifecycle_constructor_request request =
            builder->lifecycle_constructor_requests[request_index];
        const t_cblc_struct_type *type = cblc_find_struct_type(unit,
            request.receiver.struct_type_name);
        const t_cblc_constructor *constructor;
        char paragraph_name[TRANSPILE_IDENTIFIER_MAX];
        char return_name[TRANSPILE_IDENTIFIER_MAX];

        if (!type || request.constructor_index >= type->constructor_count)
            return (FT_FAILURE);
        constructor = &type->constructors[request.constructor_index];
        if (!constructor->has_definition || constructor->statement_count == 0
            || cblc_constructor_paragraph_name(&request.receiver,
                request.constructor_index, paragraph_name,
                sizeof(paragraph_name)) != FT_SUCCESS
            || cblc_constructor_return_paragraph_name(unit, &request.receiver,
                request.constructor_index, return_name,
                sizeof(return_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (builder->known_paragraph_definitions
            && builder->known_paragraph_definitions->find(paragraph_name)
                != builder->known_paragraph_definitions->end())
        {
            request_index += 1;
            continue ;
        }
        if (cblc_emit_referenced_void_methods_for_receiver(unit,
                &request.receiver, type, builder, constructor) != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            std::snprintf(diagnostic, sizeof(diagnostic),
                "failed to emit methods referenced by deferred constructor '%s::%zu' for '%s'",
                type->source_name, request.constructor_index,
                request.receiver.cobol_name);
            cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
            return (FT_FAILURE);
        }
        {
            t_cobol_text_builder_lifecycle_scope lifecycle_scope(builder, NULL,
                constructor);

            if (cblc_constructor_has_exception_control(constructor))
            {
                if (cblc_emit_constructor_exception_body(unit,
                        &request.receiver, constructor,
                        request.constructor_index, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                char line[TRANSPILE_STATEMENT_TEXT_MAX];
                size_t statement_index;
                int control_flow_depth;

                if (std::snprintf(line, sizeof(line), "       %s.",
                        paragraph_name) < 0
                    || cobol_text_builder_append_line(builder, line)
                        != FT_SUCCESS)
                    return (FT_FAILURE);
                control_flow_depth = 0;
                statement_index = 0;
                while (statement_index < constructor->statement_count)
                {
                    if (cblc_emit_substituted_statement_with_flow(unit, NULL,
                            &constructor->statements[statement_index],
                            request.receiver.cobol_name, builder,
                            &control_flow_depth, &request.receiver)
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    statement_index += 1;
                }
                if (control_flow_depth != 0
                    || std::snprintf(line, sizeof(line), "       %s.",
                        return_name) < 0
                    || cobol_text_builder_append_line(builder, line)
                        != FT_SUCCESS
                    || cobol_text_builder_append_line(builder,
                        "           EXIT.") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        request_index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_reusable_method_results(const t_cblc_translation_unit *unit,
    t_cobol_text_builder *builder)
{
    size_t data_index;
    std::unordered_set<std::string> emitted_result_names;

    if (!unit || !builder)
        return (FT_FAILURE);
    data_index = 0;
    while (data_index < unit->data_count)
    {
        const t_cblc_data_item *data_item;
        const t_cblc_data_item *receiver;
        const t_cblc_struct_type *type;
        size_t method_index;

        data_item = &unit->data_items[data_index];
        receiver = data_item;
        if (data_item->is_alias
            || data_item->kind != CBLC_DATA_KIND_STRUCT
            || data_item->is_template_item
            || (!data_item->is_function_local
                && data_item->owner_function_name[0] != '\0'
                && !cblc_find_method_parameter_storage_by_cobol(unit,
                    data_item->cobol_name)))
        {
            data_index += 1;
            continue ;
        }
        type = cblc_find_struct_type(unit, receiver->struct_type_name);
        if (!type || type->is_builtin)
        {
            data_index += 1;
            continue ;
        }
        method_index = 0;
        while (method_index < type->method_count)
        {
            const t_cblc_method *method;
            char result_name[TRANSPILE_IDENTIFIER_MAX];
            char line[256];

            method = &type->methods[method_index];
            if (!cblc_method_is_reusable(method)
                || method->return_kind == CBLC_FUNCTION_RETURN_VOID
                || ((!data_item->is_function_local
                        && data_item->owner_function_name[0] != '\0')
                    && !cblc_method_parameter_storage_uses_method(unit,
                        data_item, method)))
            {
                method_index += 1;
                continue ;
            }
            if (cblc_method_result_name(receiver, method, result_name,
                    sizeof(result_name)) != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "failed to name result storage for method '%s::%s'",
                    receiver->struct_type_name, method->source_name);
                cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                return (FT_FAILURE);
            }
            if (!emitted_result_names.insert(result_name).second)
            {
                method_index += 1;
                continue ;
            }
            if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s PIC S9(9) VALUE 0.", result_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_CHAR)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s PIC X VALUE SPACE.", result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s USAGE COMP-2 VALUE 0.", result_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_VOID_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_CHAR_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_INT_POINTER
                || method->return_kind == CBLC_FUNCTION_RETURN_STRUCT_POINTER)
            {
                if (std::snprintf(line, sizeof(line),
                        "       01 %s USAGE POINTER VALUE NULL.", result_name) < 0
                    || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                {
                    return (FT_FAILURE);
                }
            }
            else
            {
                t_cblc_data_item result_item;

                std::memset(&result_item, 0, sizeof(result_item));
                result_item.kind = CBLC_DATA_KIND_STRUCT;
                ft_strlcpy(result_item.struct_type_name, method->return_type_name,
                    sizeof(result_item.struct_type_name));
                ft_strlcpy(result_item.cobol_name, result_name,
                    sizeof(result_item.cobol_name));
                if (cblc_emit_struct_instance(unit, &result_item, builder) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "failed to emit result object for method '%s::%s' as '%s'",
                        receiver->struct_type_name, method->source_name,
                        method->return_type_name);
                    cobol_text_builder_set_diagnostic(builder, 0, diagnostic);
                    return (FT_FAILURE);
                }
            }
            method_index += 1;
        }
        data_index += 1;
    }
    return (FT_SUCCESS);
}

static size_t cblc_native_result_digits(const t_cblc_translation_unit *unit,
    const char *cobol_name)
{
    if (!unit || !unit->is_native_standard_library || !cobol_name)
        return (9);
    if (std::strcmp(cobol_name, "F-RESULT") != 0
        && std::strcmp(cobol_name, "F-OPERAND") != 0
        && std::strcmp(cobol_name, "CBLC-RETURN-F") != 0)
        return (9);
    if (std::strcmp(unit->program_name, "CBLC-ATOL") == 0)
        return (18);
    if (std::strcmp(unit->program_name, "CBLC-ATOLL") == 0)
        return (36);
    if (std::strcmp(unit->program_name, "CBLC-ABS") == 0)
        return (18);
    return (9);
}

static const char *cblc_native_numeric_storage_format(
    const t_cblc_translation_unit *unit, const char *cobol_name)
{
    if (unit && unit->is_native_standard_library
        && cblc_native_result_digits(unit, cobol_name) <= 18)
        return (" COMP-5");
    return ("");
}

static int cblc_native_parameter_is_by_value(const t_cblc_translation_unit *unit,
    size_t parameter_index)
{
    const char *program_name;

    if (!unit || !unit->is_native_standard_library)
        return (0);
    program_name = unit->program_name;
    if (std::strcmp(program_name, "CBLC-STRLEN") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-STRNLEN") == 0)
        return (parameter_index == 1 || parameter_index == 2);
    if (std::strcmp(program_name, "CBLC-STRTOD") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-TOUPPER") == 0
        || std::strcmp(program_name, "CBLC-TOLOWER") == 0)
        return (parameter_index == 1);
    if (std::strcmp(program_name, "CBLC-STRCMP") == 0
        || std::strcmp(program_name, "CBLC-STRCPY") == 0)
        return (parameter_index == 1 || parameter_index == 3);
    if (std::strcmp(program_name, "CBLC-STRNCPY") == 0)
        return (parameter_index == 1 || parameter_index == 3 || parameter_index == 4);
    if (std::strcmp(program_name, "CBLC-STRCAT") == 0)
        return (parameter_index == 1 || parameter_index == 3 || parameter_index == 5);
    return (0);
}

static int cblc_native_parameter_is_status(const t_cblc_function *entry_function,
    size_t parameter_index)
{
    if (!entry_function || parameter_index >= entry_function->parameter_count)
        return (0);
    return (std::strstr(entry_function->parameters[parameter_index].cobol_name,
            "STATUS") != NULL);
}

static int cblc_native_parameter_uses_comp5(const t_cblc_translation_unit *unit,
    const t_cblc_function *entry_function, size_t parameter_index)
{
    const char *program_name;
    const char *parameter_name;

    if (!unit || !entry_function || parameter_index >= entry_function->parameter_count
        || entry_function->parameters[parameter_index].kind
            != TRANSPILE_FUNCTION_PARAMETER_INT)
        return (0);
    if (cblc_native_parameter_is_status(entry_function, parameter_index))
        return (0);
    program_name = unit->program_name;
    parameter_name = entry_function->parameters[parameter_index].cobol_name;
    if (std::strcmp(program_name, "CBLC-ABS") == 0)
        return (1);
    if (std::strstr(parameter_name, "RESULT") != NULL)
        return (0);
    if (std::strstr(parameter_name, "LENGTH") != NULL
        || std::strstr(parameter_name, "LIMIT") != NULL
        || std::strstr(parameter_name, "REQUEST") != NULL
        || std::strstr(parameter_name, "SERIAL") != NULL
        || std::strstr(parameter_name, "SCALE") != NULL
        || std::strstr(parameter_name, "DURATION") != NULL
        || std::strstr(parameter_name, "COMPARISON") != NULL)
        return (1);
    return (0);
}

static int cblc_emit_global_failure_unwind(
    const t_cblc_translation_unit *unit, t_cobol_text_builder *builder)
{
    const t_cblc_function *entry;
    std::vector<const t_cblc_statement *> drops;
    std::unordered_set<std::string> emitted;
    char line[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!unit || !builder)
        return (FT_FAILURE);
    /* Class-only/native-library units have no root function. Their exceptions
     * propagate through the caller-provided context; they do not own a
     * process-level startup or uncaught-exception boundary. */
    if (unit->entry_function_index >= unit->function_count)
        return (FT_SUCCESS);
    entry = &unit->functions[unit->entry_function_index];
    if (std::strcmp(entry->source_name, "main") != 0)
        return (FT_SUCCESS);
    for (size_t index = 0; index < entry->statement_count; ++index)
    {
        const t_cblc_statement *statement = &entry->statements[index];

        if (statement->type != CBLC_STATEMENT_DESTRUCT
            || statement->lifecycle_completion_flag[0] == '\0'
            || statement->lifecycle_cleanup_paragraph[0] == '\0'
            || !emitted.insert(statement->lifecycle_cleanup_paragraph).second)
            continue;
        drops.push_back(statement);
        if (std::snprintf(line, sizeof(line), "       %s.",
                statement->lifecycle_cleanup_paragraph) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line),
                "           IF %s NOT = 'Y'",
                statement->lifecycle_completion_flag) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line),
                "               GO TO %s-END",
                statement->lifecycle_cleanup_paragraph) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || cobol_text_builder_append_line(builder,
                "           END-IF.") != FT_SUCCESS
            || cblc_emit_lifecycle_statement(unit, statement, builder, entry,
                index, NULL) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "           MOVE 'N' TO %s.",
                statement->lifecycle_completion_flag) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || std::snprintf(line, sizeof(line), "       %s-END.",
                statement->lifecycle_cleanup_paragraph) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS
            || cobol_text_builder_append_line(builder, "           EXIT.") != FT_SUCCESS
            || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (!cblc_unit_uses_exceptions(unit))
        return (FT_SUCCESS);
    if (cobol_text_builder_append_line(builder,
            "       CBLC-ROOT-INIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-ACTIVE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-MATCHED") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-DYNAMIC-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-PAYLOAD-SIZE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-PAYLOAD-OWNER") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-FILE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-LINE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SOURCE-COLUMN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-RAISING") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 'N' TO CBLC-EX-CLEANING") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-TYPE-ID") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-LINE") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-SECONDARY-SOURCE-COLUMN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-INT-PAYLOAD") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE 0 TO CBLC-EX-MESSAGE-LEN") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           MOVE SPACES TO CBLC-EX-MESSAGE-BUF") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           EXIT.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder,
            "       CBLC-ROOT-UNCAUGHT.") != FT_SUCCESS)
        return (FT_FAILURE);
    /* Entry statements already carry reverse declaration-order destruction. */
    for (size_t drop_index = 0; drop_index < drops.size(); ++drop_index)
    {
        const t_cblc_statement *statement = drops[drop_index];

        if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                statement->lifecycle_cleanup_paragraph) < 0
            || cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cobol_text_builder_append_line(builder,
            "           DISPLAY \"Unhandled CBL-C exception\"") != FT_SUCCESS
        || cobol_text_builder_append_line(builder,
            "           PERFORM CBLC-EX-CLEAR-PAYLOAD") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "           STOP RUN.") != FT_SUCCESS
        || cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int cblc_generate_cobol_with_diagnostic(
    const t_cblc_translation_unit *unit, char **out_text,
    int *out_diagnostic_code, char *out_diagnostic_message,
    size_t diagnostic_message_size)
{
    t_cobol_text_builder builder;
    std::unordered_set<std::string> emitted_pointer_layouts;
    std::unordered_set<std::string> emitted_method_paragraphs;
    std::unordered_set<std::string> known_paragraph_definitions;
    char line[256];
    size_t index;
    size_t entry_index;
    const t_cblc_function *entry_function;
    const t_cblc_function *unresolved_function;
    const t_cblc_statement *unresolved_statement;
    const char *generation_phase;
    const int trace_cobol_emission =
        std::getenv("CTOC_TRACE_COBOL_EMIT") != NULL;
    std::chrono::steady_clock::time_point emission_phase_started;
    int entry_uses_exception_context;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    if (out_diagnostic_code)
        *out_diagnostic_code = 0;
    if (out_diagnostic_message && diagnostic_message_size > 0)
        out_diagnostic_message[0] = '\0';
    {
        cblc::backend::verification_diagnostic global_lifetime_diagnostic;

        if (!cblc::backend::verify_global_object_lifetimes(unit,
                &global_lifetime_diagnostic))
        {
            if (out_diagnostic_code)
                *out_diagnostic_code = TRANSPILE_ERROR_BACKEND_IR_VERIFICATION;
            if (out_diagnostic_message && diagnostic_message_size > 0)
                std::snprintf(out_diagnostic_message, diagnostic_message_size,
                    "global lifecycle verification failed: %s",
                    global_lifetime_diagnostic.message.c_str());
            return (FT_FAILURE);
        }
    }
    generation_phase = "initialization";
    /*
     * Shadow-lower supported free functions and class methods before emission.
     * The legacy emitter remains authoritative during migration; only an
     * explicitly unsupported legacy construct may bypass this verifier.
     */
    index = 0;
    while (index < unit->function_count)
    {
        cblc::backend::typed_function typed_function;
        cblc::backend::control_flow_graph typed_cfg;
        cblc::backend::lifecycle_cleanup_plan cleanup_plan;
        cblc::backend::verification_diagnostic ir_diagnostic;

        if (!cblc::backend::lower_scalar_function(unit, index, &typed_function,
                &ir_diagnostic))
        {
            if (ir_diagnostic.code != cblc::backend::verification_error::unsupported_legacy_statement
                && ir_diagnostic.code != cblc::backend::verification_error::unsupported_legacy_type)
            {
                cblc_set_backend_ir_diagnostic(out_diagnostic_code,
                    out_diagnostic_message, diagnostic_message_size,
                    &unit->functions[index], &typed_function, &ir_diagnostic,
                    "IR");
                return (FT_FAILURE);
            }
        }
        else if (!cblc::backend::build_structured_control_flow_graph_with_cleanup(
                typed_function, &typed_cfg, &cleanup_plan, &ir_diagnostic,
                cblc_lifetime_policy_for_unit(unit)))
        {
            cblc_set_backend_ir_diagnostic(out_diagnostic_code,
                out_diagnostic_message, diagnostic_message_size,
                &unit->functions[index], &typed_function, &ir_diagnostic,
                "CFG/lifecycle");
            return (FT_FAILURE);
        }
        index += 1;
    }
    {
        size_t type_index;

        type_index = 0;
        while (type_index < unit->struct_type_count)
        {
            size_t method_index;

            method_index = 0;
            while (method_index < unit->struct_types[type_index].method_count)
            {
                cblc::backend::typed_function typed_function;
                cblc::backend::control_flow_graph typed_cfg;
                cblc::backend::lifecycle_cleanup_plan cleanup_plan;
                cblc::backend::verification_diagnostic ir_diagnostic;

                if (!cblc::backend::lower_scalar_method(unit, type_index,
                        method_index, &typed_function, &ir_diagnostic))
                {
                    if (ir_diagnostic.code != cblc::backend::verification_error::unsupported_legacy_statement
                        && ir_diagnostic.code != cblc::backend::verification_error::unsupported_legacy_type)
                    {
                        if (out_diagnostic_code)
                            *out_diagnostic_code = TRANSPILE_ERROR_BACKEND_IR_VERIFICATION;
                        if (out_diagnostic_message && diagnostic_message_size > 0)
                            std::snprintf(out_diagnostic_message,
                                diagnostic_message_size,
                                "typed IR verification failed in method '%s::%s': %s",
                                unit->struct_types[type_index].source_name,
                                unit->struct_types[type_index].methods[method_index].source_name,
                                ir_diagnostic.message.c_str());
                        return (FT_FAILURE);
                    }
                }
                else if (!cblc::backend::build_structured_control_flow_graph_with_cleanup(
                        typed_function, &typed_cfg, &cleanup_plan, &ir_diagnostic,
                        cblc_lifetime_policy_for_unit(unit)))
                {
                    if (out_diagnostic_code)
                        *out_diagnostic_code = TRANSPILE_ERROR_BACKEND_IR_VERIFICATION;
                    if (out_diagnostic_message && diagnostic_message_size > 0)
                        std::snprintf(out_diagnostic_message,
                            diagnostic_message_size,
                            "typed CFG/lifecycle verification failed in method '%s::%s': %s",
                            unit->struct_types[type_index].source_name,
                            unit->struct_types[type_index].methods[method_index].source_name,
                            ir_diagnostic.message.c_str());
                    return (FT_FAILURE);
                }
                method_index += 1;
            }
            type_index += 1;
        }
    }
    unresolved_function = NULL;
    unresolved_statement = NULL;
    if (cblc_translation_unit_calls_are_resolved(unit,
            &unresolved_function, &unresolved_statement) != FT_SUCCESS)
    {
        if (out_diagnostic_code)
            *out_diagnostic_code = TRANSPILE_ERROR_FUNCTION_UNRESOLVED;
        if (out_diagnostic_message && diagnostic_message_size > 0)
        {
            if (unresolved_function && unresolved_statement)
                std::snprintf(out_diagnostic_message,
                    diagnostic_message_size,
                    "unresolved call '%s' in function '%s' at line %zu",
                    unresolved_statement->call_identifier,
                    unresolved_function->source_name,
                    unresolved_statement->source_line);
            else
                ft_strlcpy(out_diagnostic_message,
                    "translation unit contains an unresolved local call",
                    diagnostic_message_size);
        }
        return (FT_FAILURE);
    }
    entry_index = unit->entry_function_index;
    if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
        entry_index = 0;
    entry_function = NULL;
    if (unit->function_count > 0 && entry_index < unit->function_count)
        entry_function = &unit->functions[entry_index];
    entry_uses_exception_context = entry_function
        && cblc_is_external_entry_function(unit)
        && cblc_unit_uses_exceptions(unit);
    cobol_text_builder_init(&builder);
    builder.emitted_method_paragraphs = &emitted_method_paragraphs;
    builder.known_paragraph_definitions = &known_paragraph_definitions;
    generation_phase = "program and working-storage declarations";
    builder.free_format = unit->is_native_standard_library ? 1 : 0;
    if (unit->is_native_standard_library
        && cobol_text_builder_append_line(&builder, ">>SOURCE FORMAT IS FREE") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       ENVIRONMENT DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (unit->file_count > 0)
    {
        if (cobol_text_builder_append_line(&builder, "       INPUT-OUTPUT SECTION.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "       FILE-CONTROL.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->file_count)
        {
            const char *organization_clause;
            const char *record_key_clause;

            organization_clause = unit->files[index].organization
                == TRANSPILE_FILE_ORGANIZATION_INDEXED ? " ORGANIZATION IS INDEXED"
                : (unit->files[index].organization == TRANSPILE_FILE_ORGANIZATION_RELATIVE
                    ? " ORGANIZATION IS RELATIVE" : "");
            record_key_clause = unit->files[index].organization
                == TRANSPILE_FILE_ORGANIZATION_INDEXED ? " RECORD KEY IS " : "";
            if (std::snprintf(line, sizeof(line),
                    "           SELECT %s ASSIGN TO \"%s\"%s%s%s%s FILE STATUS IS %s-STATUS.",
                    unit->files[index].cobol_name, unit->files[index].path,
                    organization_clause, record_key_clause,
                    record_key_clause[0] != '\0' ? unit->files[index].cobol_name : "",
                    record_key_clause[0] != '\0' ? "-RECORD" : "",
                    unit->files[index].cobol_name) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
    }
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (unit->file_count > 0)
    {
        if (cobol_text_builder_append_line(&builder, "       FILE SECTION.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->file_count)
        {
            size_t record_length;

            record_length = unit->files[index].record_length;
            if (record_length == 0)
                record_length = 256;
            if (std::snprintf(line, sizeof(line), "       FD %s.",
                    unit->files[index].cobol_name) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                || std::snprintf(line, sizeof(line), "       01 %s-RECORD PIC X(%zu).",
                    unit->files[index].cobol_name, record_length) < 0
                || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
    }
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
    if (cblc_unit_uses_exceptions(unit) && !entry_uses_exception_context)
    {
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-EX-CONTEXT.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-ACTIVE PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-MATCHED PIC X VALUE 'N'.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-DYNAMIC-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-SIZE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-OWNER PIC X VALUE 'N'.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-FILE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-LINE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-COLUMN PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-RAISING PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-CLEANING PIC X VALUE 'N'.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-TYPE-ID PIC 9(10) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-SOURCE-LINE PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-SECONDARY-SOURCE-COLUMN PIC 9(9) COMP-5 VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-INT-PAYLOAD PIC S9(9) VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "          05 CBLC-EX-MESSAGE.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "             10 CBLC-EX-MESSAGE-LEN PIC 9(4) COMP VALUE 0.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "             10 CBLC-EX-MESSAGE-BUF PIC X(256).") != FT_SUCCESS)
            goto cleanup;
    }
    else if (!cblc_unit_uses_exceptions(unit)
        && cblc_unit_uses_cleanup_guard(unit)
        && cobol_text_builder_append_line(&builder,
            "       01 CBLC-EX-CLEANING PIC X VALUE 'N'.") != FT_SUCCESS)
        goto cleanup;
    if (unit->file_count > 0
        && cobol_text_builder_append_line(&builder,
            "       01 CBLC-EOF-FLAG PIC X VALUE 'N'.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->file_count)
    {
        if (std::snprintf(line, sizeof(line), "       01 %s-STATUS PIC X(2) VALUE SPACES.",
                unit->files[index].cobol_name) < 0
            || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    index = 0;
    while (index < unit->copy_include_count)
    {
        char copy_name[TRANSPILE_IDENTIFIER_MAX];
        size_t name_index;

        ft_strlcpy(copy_name, unit->copy_includes[index].name, sizeof(copy_name));
        name_index = 0;
        while (copy_name[name_index] != '\0')
        {
            if (copy_name[name_index] >= 'a' && copy_name[name_index] <= 'z')
                copy_name[name_index] = static_cast<char>(copy_name[name_index] - 'a' + 'A');
            name_index += 1;
        }
        if (std::snprintf(line, sizeof(line), "       COPY %s.", copy_name) < 0)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    generation_phase = "global object storage";
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].is_template_item)
        {
            index += 1;
            continue ;
        }
        if (unit->data_items[index].is_alias || std::strchr(unit->data_items[index].source_name, '.')
            || std::strstr(unit->data_items[index].source_name, "->")
            || cblc_is_entry_parameter_item(unit, &unit->data_items[index])
            || cblc_is_entry_return_item(unit, &unit->data_items[index]))
        {
            if (cblc_string_item_is_member_storage(&unit->data_items[index])
                && (cblc_string_item_uses_dynamic_storage(&unit->data_items[index])
                    || cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index])))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (unit->data_items[index].initializer_text[0] == '\'')
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE \"%c\".",
                            unit->data_items[index].cobol_name,
                            unit->data_items[index].length,
                            unit->data_items[index].initializer_text[1]) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC X(%zu).",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].length) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_INT)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9)%s OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        cblc_native_numeric_storage_format(unit,
                            unit->data_items[index].cobol_name),
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s VALUE %s.",
                        unit->data_items[index].cobol_name,
                        cblc_native_result_digits(unit,
                            unit->data_items[index].cobol_name),
                        cblc_native_numeric_storage_format(unit,
                            unit->data_items[index].cobol_name),
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s.",
                    unit->data_items[index].cobol_name,
                    cblc_native_result_digits(unit,
                        unit->data_items[index].cobol_name),
                    cblc_native_numeric_storage_format(unit,
                        unit->data_items[index].cobol_name)) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_DOUBLE)
        {
            if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR_BUFFER)
        {
            if (unit->data_items[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (std::snprintf(line, sizeof(line), "          05 %s-LEN PIC 9(4) COMP VALUE %zu.",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].is_const
                        ? unit->data_items[index].initializer_length
                        : 0) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_array_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_string_item_uses_dynamic_storage(&unit->data_items[index]))
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-CAP PIC 9(4) COMP VALUE 0.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line),
                        "          05 %s-PTR USAGE POINTER VALUE NULL.",
                        unit->data_items[index].cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                if (std::snprintf(line, sizeof(line), "       01 %s-BUF BASED PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu).",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            index += 1;
            continue ;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_emit_struct_instance(unit, &unit->data_items[index], &builder) != FT_SUCCESS)
            {
                char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                std::snprintf(diagnostic, sizeof(diagnostic),
                    "failed to emit object storage for '%s' of type '%s'",
                    unit->data_items[index].source_name,
                    unit->data_items[index].struct_type_name);
                cobol_text_builder_set_diagnostic(&builder, 0, diagnostic);
                goto cleanup;
            }
            index += 1;
            continue ;
        }
        else if (cblc_data_kind_is_pointer(unit->data_items[index].kind))
        {
            if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER VALUE NULL.",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else
        {
            goto cleanup;
        }
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    generation_phase = "reusable method result storage";
    if (cblc_emit_reusable_method_results(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    generation_phase = "procedure storage declarations";
    if (cblc_unit_has_pointer_data(unit))
    {
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CURSOR USAGE POINTER VALUE NULL.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-OFFSET PIC S9(9) VALUE 0.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CHAR BASED PIC X.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT BASED PIC S9(9) COMP-5.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-PTR BASED USAGE POINTER.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-CHAR-2 BASED PIC X.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-2 BASED PIC S9(9) COMP-5.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder,
                "       01 CBLC-PTR-INT-PTR-2 BASED USAGE POINTER.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->data_count)
        {
            if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT_POINTER
                && !unit->data_items[index].is_template_item
                && !cblc_struct_pointer_layout_seen(&emitted_pointer_layouts,
                    &unit->data_items[index]))
            {
                char based_name[TRANSPILE_IDENTIFIER_MAX];
                const t_cblc_struct_type *type;

                if (cblc_build_struct_pointer_based_name(unit,
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].struct_type_name,
                        based_name, sizeof(based_name)) != FT_SUCCESS)
                {
                    goto cleanup;
                }
                type = cblc_find_struct_type(unit, unit->data_items[index].struct_type_name);
                if (!type)
                {
                    goto cleanup;
                }
                if (std::snprintf(line, sizeof(line), "       01 %s BASED.",
                        based_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                {
                    goto cleanup;
                }
                if (cblc_emit_struct_fields(unit, type, based_name, 5, &builder)
                    != FT_SUCCESS)
                {
                    goto cleanup;
                }
                if (cblc_emit_nested_pointer_layouts(unit, type,
                        unit->data_items[index].cobol_name, 0, &builder)
                    != FT_SUCCESS)
                {
                    goto cleanup;
                }
            }
            index += 1;
        }
    }
    if (entry_function && cblc_is_external_entry_function(unit)
        && (entry_function->parameter_count > 0
            || entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID
            || entry_uses_exception_context))
    {
        if (cobol_text_builder_append_line(&builder, "       LINKAGE SECTION.") != FT_SUCCESS)
            goto cleanup;
        if (entry_uses_exception_context
            && (cobol_text_builder_append_line(&builder,
                    "       01 CBLC-EX-CONTEXT.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-ACTIVE PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-MATCHED PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-DYNAMIC-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-SIZE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-PAYLOAD-OWNER PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-FILE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-LINE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SOURCE-COLUMN PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-RAISING PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-CLEANING PIC X.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-TYPE-ID PIC 9(10) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-SOURCE-LINE PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-SECONDARY-SOURCE-COLUMN PIC 9(9) COMP-5.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-INT-PAYLOAD PIC S9(9).") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "          05 CBLC-EX-MESSAGE.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "             10 CBLC-EX-MESSAGE-LEN PIC 9(4) COMP.") != FT_SUCCESS
                || cobol_text_builder_append_line(&builder,
                    "             10 CBLC-EX-MESSAGE-BUF PIC X(256).") != FT_SUCCESS))
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT)
            {
                size_t integer_digits;
                int status_parameter;

                integer_digits = 9;
                status_parameter = cblc_native_parameter_is_status(entry_function, index);
                if (unit->is_native_standard_library && index == 2
                    && std::strcmp(unit->program_name, "CBLC-ATOL") == 0)
                    integer_digits = 18;
                if (unit->is_native_standard_library && index == 2
                    && std::strcmp(unit->program_name, "CBLC-ATOLL") == 0)
                    integer_digits = 36;
                if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-ABS") == 0)
                    integer_digits = 18;
                if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 1)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(4).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 2)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(2).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 3)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(2).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (unit->is_native_standard_library
                    && std::strcmp(unit->program_name, "CBLC-DATE-YYYYMMDD") == 0
                    && index == 4)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9(8) COMP-3.",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (status_parameter
                    && (std::strcmp(unit->program_name, "CBLC-STRCPY") != 0
                        && std::strcmp(unit->program_name, "CBLC-STRNCPY") != 0
                        && std::strcmp(unit->program_name, "CBLC-STRCAT") != 0
                        && std::strcmp(unit->program_name, "CBLC-TOUPPER") != 0
                        && std::strcmp(unit->program_name, "CBLC-TOLOWER") != 0))
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC 9.",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu)%s.",
                            entry_function->parameters[index].cobol_name,
                            integer_digits,
                            cblc_native_parameter_uses_comp5(unit, entry_function, index)
                                ? " COMP-5" : "") < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC X.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind
                == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
            {
                t_cblc_data_item parameter_item;

                std::memset(&parameter_item, 0, sizeof(parameter_item));
                parameter_item.kind = CBLC_DATA_KIND_STRUCT;
                ft_strlcpy(parameter_item.source_name,
                    entry_function->parameters[index].source_name,
                    sizeof(parameter_item.source_name));
                ft_strlcpy(parameter_item.cobol_name,
                    entry_function->parameters[index].cobol_name,
                    sizeof(parameter_item.cobol_name));
                ft_strlcpy(parameter_item.struct_type_name,
                    entry_function->parameters[index].type_name,
                    sizeof(parameter_item.struct_type_name));
                if (!cblc_find_struct_type(unit, parameter_item.struct_type_name)
                    || cblc_emit_struct_instance(unit, &parameter_item, &builder) != FT_SUCCESS)
                    goto cleanup;
                index += 1;
                continue ;
            }
            else if (entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_VOID_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT_POINTER
                || entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT_POINTER)
            {
                if (unit->is_native_standard_library
                    && entry_function->parameters[index].kind
                        == TRANSPILE_FUNCTION_PARAMETER_CHAR_POINTER)
                {
                    if (std::snprintf(line, sizeof(line), "       01 %s PIC X(255).",
                            entry_function->parameters[index].cobol_name) < 0)
                        goto cleanup;
                }
                else if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->parameters[index].cobol_name) < 0)
                    goto cleanup;
            }
            else if (entry_function->parameters[index].kind
                == TRANSPILE_FUNCTION_PARAMETER_CHAR_BUFFER)
            {
                size_t parameter_length;

                parameter_length = entry_function->parameters[index].length;
                if (parameter_length == 0)
                    parameter_length = 255;
                if (std::snprintf(line, sizeof(line), "       01 %s.",
                        entry_function->parameters[index].cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "          05 %s-LEN PIC 9(4) COMP.",
                        entry_function->parameters[index].cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS
                    || std::snprintf(line, sizeof(line),
                        "          05 %s-BUF PIC X(%zu).",
                        entry_function->parameters[index].cobol_name,
                        parameter_length) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                index += 1;
                continue ;
            }
            else
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            const t_cblc_data_item *return_item;

            if (entry_function->return_item_index < 0
                || static_cast<size_t>(entry_function->return_item_index) >= unit->data_count)
                goto cleanup;
            return_item = &unit->data_items[entry_function->return_item_index];
            if (return_item->kind == CBLC_DATA_KIND_INT)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(%zu).",
                        entry_function->return_cobol_name,
                        cblc_native_result_digits(unit,
                            entry_function->return_cobol_name)) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_DOUBLE)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE COMP-2.",
                        entry_function->return_cobol_name) < 0
                    || cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_emit_struct_instance(unit, return_item, &builder) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (cblc_data_kind_is_pointer(return_item->kind))
            {
                if (std::snprintf(line, sizeof(line), "       01 %s USAGE POINTER.",
                        entry_function->return_cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else
                goto cleanup;
        }
        ft_strlcpy(line, "       PROCEDURE DIVISION USING", sizeof(line));
        if (entry_uses_exception_context)
            ft_strlcat(line, " CBLC-EX-CONTEXT", sizeof(line));
        if (entry_function->parameter_count == 0
            && entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID
            && !entry_uses_exception_context)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            char clause[TRANSPILE_IDENTIFIER_MAX + 32];
            int by_value;

            by_value = unit->is_native_standard_library
                && entry_function->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_INT
                && cblc_native_parameter_is_by_value(unit, index);
            if (std::snprintf(clause, sizeof(clause), " %s%s",
                    by_value ? "BY VALUE " : (unit->is_native_standard_library
                        ? "BY REFERENCE " : ""),
                    entry_function->parameters[index].cobol_name) < 0)
                goto cleanup;
            if (std::strlen(line) + std::strlen(clause) >= 65)
            {
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                ft_strlcpy(line, "           ", sizeof(line));
            }
            ft_strlcat(line, clause, sizeof(line));
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            char clause[TRANSPILE_IDENTIFIER_MAX + 2];

            if (std::snprintf(clause, sizeof(clause), " %s%s",
                    unit->is_native_standard_library ? "BY REFERENCE " : "",
                    entry_function->return_cobol_name) < 0)
                goto cleanup;
            if (std::strlen(line) + std::strlen(clause) >= 65)
            {
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
                ft_strlcpy(line, "           ", sizeof(line));
            }
            ft_strlcat(line, clause, sizeof(line));
        }
        if (std::strlen(line) + 2 >= sizeof(line))
            goto cleanup;
        ft_strlcat(line, ".", sizeof(line));
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
            goto cleanup;
    }
    emission_phase_started = std::chrono::steady_clock::now();
    if (unit->function_count > 0)
    {
        generation_phase = "function body emission";
        if (cblc_emit_function(unit, &unit->functions[entry_index], &builder, 1) != FT_SUCCESS)
        {
            char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

            if (builder.active_statement)
                std::snprintf(diagnostic, sizeof(diagnostic),
                    "COBOL emission failed in entry function '%s' at statement %zu (kind %d, call '%s', target '%s', source '%.72s')",
                    builder.active_function_name
                        ? builder.active_function_name
                        : unit->functions[entry_index].source_name,
                    builder.active_statement_index,
                    static_cast<int>(builder.active_statement->type),
                    builder.active_statement->call_identifier,
                    builder.active_statement->target,
                    builder.active_statement->source);
            else
                std::snprintf(diagnostic, sizeof(diagnostic),
                    "COBOL emission failed in entry function '%s'",
                    unit->functions[entry_index].source_name);
            cobol_text_builder_set_diagnostic(&builder, 0, diagnostic);
            goto cleanup;
        }
        index = 0;
        while (index < unit->function_count)
        {
            if (index != entry_index && !unit->functions[index].is_template)
            {
                if (cblc_emit_function(unit, &unit->functions[index], &builder, 0) != FT_SUCCESS)
                {
                    char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

                    std::snprintf(diagnostic, sizeof(diagnostic),
                        "COBOL emission failed in function '%s'",
                        unit->functions[index].source_name);
                    cobol_text_builder_set_diagnostic(&builder, 0, diagnostic);
                    goto cleanup;
                }
            }
            index += 1;
        }
    }
    else
    {
        if (cobol_text_builder_append_line(&builder, "MAIN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
            goto cleanup;
    }
    if (trace_cobol_emission)
    {
        std::fprintf(stderr,
            "COBOL emission phase functions: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - emission_phase_started).count(), builder.length);
        emission_phase_started = std::chrono::steady_clock::now();
    }
    generation_phase = "reusable method paragraph emission";
    if (cblc_emit_reusable_method_paragraphs(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (trace_cobol_emission)
    {
        std::fprintf(stderr,
            "COBOL emission phase methods: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - emission_phase_started).count(), builder.length);
        emission_phase_started = std::chrono::steady_clock::now();
    }
    generation_phase = "lifecycle paragraph emission";
    if (cblc_emit_reusable_lifecycle_paragraphs(unit, &builder) != FT_SUCCESS)
    {
        goto cleanup;
    }
    if (cblc_emit_deferred_lifecycle_constructor_paragraphs(unit, &builder)
        != FT_SUCCESS)
    {
        cobol_text_builder_set_diagnostic(&builder, 0,
            "failed to emit deferred indexed-element lifecycle constructors");
        goto cleanup;
    }
    if (cblc_emit_global_failure_unwind(unit, &builder) != FT_SUCCESS)
    {
        cobol_text_builder_set_diagnostic(&builder, 0,
            "failed to emit the global startup failure-unwind path");
        goto cleanup;
    }
    if (trace_cobol_emission)
    {
        std::fprintf(stderr,
            "COBOL emission phase lifecycle: %.3f s (%zu bytes)\n",
            std::chrono::duration<double>(std::chrono::steady_clock::now()
                - emission_phase_started).count(), builder.length);
        emission_phase_started = std::chrono::steady_clock::now();
    }
    if (cblc_unit_uses_exceptions(unit)
        && (cobol_text_builder_append_line(&builder, "       CBLC-TERMINATE.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Fatal CBL-C double exception\"") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception type: \" CBLC-EX-TYPE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception file ID: \" CBLC-EX-SOURCE-FILE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception line: \" CBLC-EX-SOURCE-LINE") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Original exception column: \" CBLC-EX-SOURCE-COLUMN") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception type: \" CBLC-EX-SECONDARY-TYPE-ID") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception line: \" CBLC-EX-SECONDARY-SOURCE-LINE") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder,
                "           DISPLAY \"Secondary exception column: \" CBLC-EX-SECONDARY-SOURCE-COLUMN") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "") != FT_SUCCESS))
        goto cleanup;
    if (builder.deferred_procedure_length > 0
        && (cobol_text_builder_append_string(&builder,
                builder.deferred_procedure_text) != FT_SUCCESS
            || cobol_text_builder_append_line(&builder, "") != FT_SUCCESS))
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       END PROGRAM %s.",
            unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "") != FT_SUCCESS)
        goto cleanup;
    *out_text = builder.data;
    builder.data = NULL;
cleanup:
    if (!*out_text && builder.diagnostic_message[0] == '\0')
    {
        char diagnostic[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

        std::snprintf(diagnostic, sizeof(diagnostic),
            "COBOL generation failed during %s", generation_phase);
        cobol_text_builder_set_diagnostic(&builder, 0, diagnostic);
    }
    if (out_diagnostic_code)
        *out_diagnostic_code = builder.diagnostic_code;
    if (out_diagnostic_message && diagnostic_message_size > 0)
        ft_strlcpy(out_diagnostic_message, builder.diagnostic_message,
            diagnostic_message_size);
    cobol_text_builder_dispose(&builder);
    if (!*out_text)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    return (cblc_generate_cobol_with_diagnostic(unit, out_text, NULL, NULL, 0));
}

#include "src/cblc/transpiler_cblc_context_integration.inc"
