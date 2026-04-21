static int cblc_translation_unit_ensure_data_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_data_item *new_items;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->data_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_items = static_cast<t_cblc_data_item *>(cma_calloc(desired_capacity,
            sizeof(*new_items)));
    if (!new_items)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->data_count)
    {
        new_items[index] = unit->data_items[index];
        index += 1;
    }
    if (unit->data_items)
        cma_free(unit->data_items);
    unit->data_items = new_items;
    unit->data_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_import_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_import *new_imports;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->import_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_imports = static_cast<t_cblc_import *>(cma_calloc(desired_capacity,
            sizeof(*new_imports)));
    if (!new_imports)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->import_count)
    {
        new_imports[index] = unit->imports[index];
        index += 1;
    }
    if (unit->imports)
        cma_free(unit->imports);
    unit->imports = new_imports;
    unit->import_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_copy_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_copy_include *new_copies;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->copy_include_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_copies = static_cast<t_cblc_copy_include *>(cma_calloc(desired_capacity,
            sizeof(*new_copies)));
    if (!new_copies)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->copy_include_count)
    {
        new_copies[index] = unit->copy_includes[index];
        index += 1;
    }
    if (unit->copy_includes)
        cma_free(unit->copy_includes);
    unit->copy_includes = new_copies;
    unit->copy_include_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_struct_type_ensure_field_capacity(t_cblc_struct_type *type,
    size_t desired_capacity)
{
    t_cblc_struct_field *new_fields;
    size_t index;

    if (!type)
        return (FT_FAILURE);
    if (type->field_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_fields = static_cast<t_cblc_struct_field *>(cma_calloc(desired_capacity,
            sizeof(*new_fields)));
    if (!new_fields)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        new_fields[index] = type->fields[index];
        index += 1;
    }
    if (type->fields)
        cma_free(type->fields);
    type->fields = new_fields;
    type->field_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_struct_type_ensure_method_capacity(t_cblc_struct_type *type,
    size_t desired_capacity)
{
    t_cblc_method *new_methods;
    size_t index;

    if (!type)
        return (FT_FAILURE);
    if (type->method_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    new_methods = static_cast<t_cblc_method *>(cma_calloc(desired_capacity,
            sizeof(*new_methods)));
    if (!new_methods)
        return (FT_FAILURE);
    index = 0;
    while (index < type->method_count)
    {
        new_methods[index] = type->methods[index];
        index += 1;
    }
    if (type->methods)
        cma_free(type->methods);
    type->methods = new_methods;
    type->method_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_struct_type_ensure_constructor_capacity(t_cblc_struct_type *type,
    size_t desired_capacity)
{
    t_cblc_constructor *new_constructors;
    size_t index;

    if (!type)
        return (FT_FAILURE);
    if (type->constructor_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    new_constructors = static_cast<t_cblc_constructor *>(cma_calloc(desired_capacity,
            sizeof(*new_constructors)));
    if (!new_constructors)
        return (FT_FAILURE);
    index = 0;
    while (index < type->constructor_count)
    {
        new_constructors[index] = type->constructors[index];
        index += 1;
    }
    if (type->constructors)
        cma_free(type->constructors);
    type->constructors = new_constructors;
    type->constructor_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_constructor_signatures_match(const t_cblc_constructor *constructor,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    size_t index;

    if (!constructor || !parameters)
        return (0);
    if (constructor->parameter_count != parameter_count)
        return (0);
    index = 0;
    while (index < parameter_count)
    {
        if (constructor->parameters[index].kind != parameters[index].kind)
            return (0);
        if (constructor->parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT
            && std::strncmp(constructor->parameters[index].type_name,
                parameters[index].type_name,
                sizeof(constructor->parameters[index].type_name)) != 0)
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_constructor_signature_exists(const t_cblc_struct_type *type,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    size_t index;

    if (!type || !parameters)
        return (0);
    index = 0;
    while (index < type->constructor_count)
    {
        if (cblc_constructor_signatures_match(&type->constructors[index], parameters,
                parameter_count))
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_translation_unit_ensure_struct_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_struct_type *new_types;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->struct_type_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    new_types = static_cast<t_cblc_struct_type *>(cma_calloc(desired_capacity,
            sizeof(*new_types)));
    if (!new_types)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->struct_type_count)
    {
        new_types[index] = unit->struct_types[index];
        index += 1;
    }
    if (unit->struct_types)
        cma_free(unit->struct_types);
    unit->struct_types = new_types;
    unit->struct_type_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_register_builtin_string_type(t_cblc_translation_unit *unit)
{
    t_cblc_struct_type *type;
    t_cblc_method *method;
    t_cblc_constructor *constructor;

    if (!unit)
        return (FT_FAILURE);
    if (unit->struct_type_count >= unit->struct_type_capacity)
    {
        if (cblc_translation_unit_ensure_struct_capacity(unit,
                unit->struct_type_capacity == 0 ? 2 : unit->struct_type_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    type = &unit->struct_types[unit->struct_type_count];
    std::memset(type, 0, sizeof(*type));
    ft_strlcpy(type->source_name, "string", sizeof(type->source_name));
    cblc_identifier_to_cobol(type->source_name, type->cobol_name, sizeof(type->cobol_name));
    type->is_class = 1;
    type->is_builtin = 1;
    type->has_default_constructor = 1;
    type->has_destructor = 1;
    if (cblc_struct_type_ensure_constructor_capacity(type, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    constructor = &type->constructors[type->constructor_count];
    std::memset(constructor, 0, sizeof(*constructor));
    constructor->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_INT;
    ft_strlcpy(constructor->parameters[0].type_name, "int",
        sizeof(constructor->parameters[0].type_name));
    constructor->parameter_count = 1;
    constructor->has_definition = 0;
    type->constructor_count += 1;
    constructor = &type->constructors[type->constructor_count];
    std::memset(constructor, 0, sizeof(*constructor));
    ft_strlcpy(constructor->parameters[0].type_name, "string",
        sizeof(constructor->parameters[0].type_name));
    constructor->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    constructor->parameter_count = 1;
    constructor->has_definition = 0;
    type->constructor_count += 1;
    if (cblc_struct_type_ensure_method_capacity(type, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "append", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_VOID;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "len", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "clear", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_VOID;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "empty", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "equals", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "capacity", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "starts_with", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "ends_with", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "compare", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "contains", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    ft_strlcpy(method->parameters[0].type_name, "string",
        sizeof(method->parameters[0].type_name));
    method->parameter_count = 1;
    method->has_definition = 0;
    type->method_count += 1;
    unit->struct_type_count += 1;
    return (FT_SUCCESS);
}

static int cblc_register_imported_type_stub(t_cblc_translation_unit *unit,
    const t_transpiler_type_signature *signature)
{
    t_cblc_struct_type *type;
    size_t index;
    size_t parameter_index;

    if (!unit || !signature || signature->name[0] == '\0')
        return (FT_FAILURE);
    type = const_cast<t_cblc_struct_type *>(cblc_find_struct_type(unit, signature->name));
    if (!type)
    {
        if (unit->struct_type_count >= unit->struct_type_capacity)
        {
            if (cblc_translation_unit_ensure_struct_capacity(unit,
                    unit->struct_type_capacity == 0 ? 2 : unit->struct_type_capacity * 2)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        type = &unit->struct_types[unit->struct_type_count];
        std::memset(type, 0, sizeof(*type));
        ft_strlcpy(type->source_name, signature->name, sizeof(type->source_name));
        cblc_identifier_to_cobol(type->source_name, type->cobol_name, sizeof(type->cobol_name));
        type->is_class = signature->kind == TRANSPILE_TYPE_CLASS;
        type->is_imported = 1;
        unit->struct_type_count += 1;
    }
    else
    {
        type->is_class = signature->kind == TRANSPILE_TYPE_CLASS;
        type->is_imported = 1;
    }
    type->has_default_constructor = signature->has_default_constructor;
    type->has_destructor = signature->has_destructor;
    if (signature->field_count > 0 && signature->fields)
    {
        if (cblc_struct_type_ensure_field_capacity(type, signature->field_count) != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < signature->field_count)
        {
            ft_strlcpy(type->fields[index].source_name, signature->fields[index].name,
                sizeof(type->fields[index].source_name));
            cblc_identifier_to_cobol(type->fields[index].source_name,
                type->fields[index].cobol_name, sizeof(type->fields[index].cobol_name));
            ft_strlcpy(type->fields[index].declared_type_name,
                signature->fields[index].declared_type_name,
                sizeof(type->fields[index].declared_type_name));
            ft_strlcpy(type->fields[index].struct_type_name,
                signature->fields[index].struct_type_name,
                sizeof(type->fields[index].struct_type_name));
            type->fields[index].length = signature->fields[index].length;
            type->fields[index].array_count = signature->fields[index].array_count;
            type->fields[index].kind = static_cast<t_cblc_data_kind>(signature->fields[index].kind);
            type->fields[index].is_const = signature->fields[index].is_const;
            if (signature->fields[index].visibility == TRANSPILE_SYMBOL_PUBLIC)
                type->fields[index].visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
            else
                type->fields[index].visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
            index += 1;
        }
        type->field_count = signature->field_count;
    }
    if (signature->method_count > 0 && signature->methods)
    {
        if (cblc_struct_type_ensure_method_capacity(type, signature->method_count) != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < signature->method_count)
        {
            ft_strlcpy(type->methods[index].source_name, signature->methods[index].name,
                sizeof(type->methods[index].source_name));
            cblc_identifier_to_cobol(type->methods[index].source_name,
                type->methods[index].cobol_name, sizeof(type->methods[index].cobol_name));
            type->methods[index].return_kind = static_cast<t_cblc_function_return_kind>(
                signature->methods[index].return_kind);
            ft_strlcpy(type->methods[index].return_type_name,
                signature->methods[index].return_type_name,
                sizeof(type->methods[index].return_type_name));
            if (signature->methods[index].visibility == TRANSPILE_SYMBOL_PUBLIC)
                type->methods[index].visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
            else
                type->methods[index].visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
            parameter_index = 0;
            while (parameter_index < signature->methods[index].parameter_count
                && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
            {
                type->methods[index].parameters[parameter_index].kind =
                    signature->methods[index].parameter_kinds[parameter_index];
                ft_strlcpy(type->methods[index].parameters[parameter_index].source_name,
                    signature->methods[index].parameter_source_names[parameter_index],
                    sizeof(type->methods[index].parameters[parameter_index].source_name));
                ft_strlcpy(type->methods[index].parameters[parameter_index].actual_source_name,
                    signature->methods[index].parameter_actual_source_names[parameter_index],
                    sizeof(type->methods[index].parameters[parameter_index].actual_source_name));
                ft_strlcpy(type->methods[index].parameters[parameter_index].cobol_name,
                    signature->methods[index].parameter_cobol_names[parameter_index],
                    sizeof(type->methods[index].parameters[parameter_index].cobol_name));
                ft_strlcpy(type->methods[index].parameters[parameter_index].type_name,
                    signature->methods[index].parameter_type_names[parameter_index],
                    sizeof(type->methods[index].parameters[parameter_index].type_name));
                parameter_index += 1;
            }
            type->methods[index].parameter_count = signature->methods[index].parameter_count;
            if (type->methods[index].statements)
                cma_free(type->methods[index].statements);
            type->methods[index].statements = NULL;
            type->methods[index].statement_count = 0;
            type->methods[index].statement_capacity = 0;
            if (signature->methods[index].statement_count > 0
                && signature->methods[index].statements)
            {
                type->methods[index].statements = static_cast<t_cblc_statement *>(cma_calloc(
                    signature->methods[index].statement_count,
                    sizeof(*type->methods[index].statements)));
                if (!type->methods[index].statements)
                    return (FT_FAILURE);
                std::memcpy(type->methods[index].statements,
                    signature->methods[index].statements,
                    signature->methods[index].statement_count
                        * sizeof(*type->methods[index].statements));
                type->methods[index].statement_count = signature->methods[index].statement_count;
                type->methods[index].statement_capacity = signature->methods[index].statement_count;
            }
            type->methods[index].has_definition = signature->methods[index].has_definition;
            if (cblc_register_imported_parameter_storage(unit,
                    type->methods[index].parameters,
                    type->methods[index].parameter_count) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        type->method_count = signature->method_count;
    }
    if (signature->constructor_count > 0 && signature->constructors)
    {
        if (cblc_struct_type_ensure_constructor_capacity(type,
                signature->constructor_count) != FT_SUCCESS)
            return (FT_FAILURE);
        index = 0;
        while (index < signature->constructor_count)
        {
            parameter_index = 0;
            while (parameter_index < signature->constructors[index].parameter_count
                && parameter_index < TRANSPILE_FUNCTION_PARAMETER_MAX)
            {
                type->constructors[index].parameters[parameter_index].kind =
                    signature->constructors[index].parameter_kinds[parameter_index];
                ft_strlcpy(type->constructors[index].parameters[parameter_index].source_name,
                    signature->constructors[index].parameter_source_names[parameter_index],
                    sizeof(type->constructors[index].parameters[parameter_index].source_name));
                ft_strlcpy(type->constructors[index].parameters[parameter_index].actual_source_name,
                    signature->constructors[index].parameter_actual_source_names[parameter_index],
                    sizeof(type->constructors[index].parameters[parameter_index].actual_source_name));
                ft_strlcpy(type->constructors[index].parameters[parameter_index].cobol_name,
                    signature->constructors[index].parameter_cobol_names[parameter_index],
                    sizeof(type->constructors[index].parameters[parameter_index].cobol_name));
                ft_strlcpy(type->constructors[index].parameters[parameter_index].type_name,
                    signature->constructors[index].parameter_type_names[parameter_index],
                    sizeof(type->constructors[index].parameters[parameter_index].type_name));
                parameter_index += 1;
            }
            type->constructors[index].parameter_count =
                signature->constructors[index].parameter_count;
            if (type->constructors[index].statements)
                cma_free(type->constructors[index].statements);
            type->constructors[index].statements = NULL;
            type->constructors[index].statement_count = 0;
            type->constructors[index].statement_capacity = 0;
            if (signature->constructors[index].statement_count > 0
                && signature->constructors[index].statements)
            {
                type->constructors[index].statements = static_cast<t_cblc_statement *>(
                    cma_calloc(signature->constructors[index].statement_count,
                        sizeof(*type->constructors[index].statements)));
                if (!type->constructors[index].statements)
                    return (FT_FAILURE);
                std::memcpy(type->constructors[index].statements,
                    signature->constructors[index].statements,
                    signature->constructors[index].statement_count
                        * sizeof(*type->constructors[index].statements));
                type->constructors[index].statement_count =
                    signature->constructors[index].statement_count;
                type->constructors[index].statement_capacity =
                    signature->constructors[index].statement_count;
            }
            type->constructors[index].has_definition =
                signature->constructors[index].has_definition;
            if (cblc_register_imported_parameter_storage(unit,
                    type->constructors[index].parameters,
                    type->constructors[index].parameter_count) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        type->constructor_count = signature->constructor_count;
    }
    if (type->destructor_statements)
        cma_free(type->destructor_statements);
    type->destructor_statements = NULL;
    type->destructor_statement_count = 0;
    type->destructor_statement_capacity = 0;
    if (signature->destructor_statement_count > 0 && signature->destructor_statements)
    {
        type->destructor_statements = static_cast<t_cblc_statement *>(cma_calloc(
            signature->destructor_statement_count, sizeof(*type->destructor_statements)));
        if (!type->destructor_statements)
            return (FT_FAILURE);
        std::memcpy(type->destructor_statements, signature->destructor_statements,
            signature->destructor_statement_count * sizeof(*type->destructor_statements));
        type->destructor_statement_count = signature->destructor_statement_count;
        type->destructor_statement_capacity = signature->destructor_statement_count;
    }
    type->has_destructor_definition = signature->has_destructor_definition;
    return (FT_SUCCESS);
}

static int cblc_register_imported_parameter_storage(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count)
{
    size_t index;

    if (!unit || !parameters)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        t_cblc_data_kind kind;

        if (parameters[index].actual_source_name[0] == '\0'
            || parameters[index].cobol_name[0] == '\0')
        {
            index += 1;
            continue ;
        }
        if (cblc_find_data_item(unit, parameters[index].actual_source_name))
        {
            index += 1;
            continue ;
        }
        if (cblc_data_kind_from_parameter_kind(parameters[index].kind, &kind) != FT_SUCCESS)
            return (FT_FAILURE);
        if (kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *type;

            type = cblc_find_struct_type(unit, parameters[index].type_name);
            if (!type)
                return (FT_FAILURE);
            if (cblc_add_named_data_item(unit, parameters[index].actual_source_name,
                    parameters[index].cobol_name, kind, 0, 0, parameters[index].type_name,
                    NULL, 0, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_add_struct_instance_field_items(unit, type,
                    parameters[index].actual_source_name, parameters[index].cobol_name,
                    NULL, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_add_named_data_item(unit, parameters[index].actual_source_name,
                    parameters[index].cobol_name, kind, 0, 0, parameters[index].type_name,
                    NULL, 0, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_function_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_function *new_functions;
    size_t index;

    if (!unit)
        return (FT_FAILURE);
    if (unit->function_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    new_functions = static_cast<t_cblc_function *>(cma_calloc(desired_capacity,
            sizeof(*new_functions)));
    if (!new_functions)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->function_count)
    {
        new_functions[index] = unit->functions[index];
        index += 1;
    }
    if (unit->functions)
        cma_free(unit->functions);
    unit->functions = new_functions;
    unit->function_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_function_ensure_statement_capacity(t_cblc_function *function,
    size_t desired_capacity)
{
    t_cblc_statement *new_statements;
    size_t index;

    if (!function)
        return (FT_FAILURE);
    if (function->statement_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(desired_capacity,
            sizeof(*new_statements)));
    if (!new_statements)
        return (FT_FAILURE);
    index = 0;
    while (index < function->statement_count)
    {
        new_statements[index] = function->statements[index];
        index += 1;
    }
    if (function->statements)
        cma_free(function->statements);
    function->statements = new_statements;
    function->statement_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_function_ensure_local_destructor_capacity(t_cblc_function *function,
    size_t desired_capacity)
{
    char (*new_targets)[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!function)
        return (FT_FAILURE);
    if (function->local_destructor_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(desired_capacity,
            sizeof(*new_targets)));
    if (!new_targets)
        return (FT_FAILURE);
    index = 0;
    while (index < function->local_destructor_count)
    {
        ft_strlcpy(new_targets[index], function->local_destructor_targets[index],
            TRANSPILE_IDENTIFIER_MAX);
        index += 1;
    }
    if (function->local_destructor_targets)
        cma_free(function->local_destructor_targets);
    function->local_destructor_targets = new_targets;
    function->local_destructor_capacity = desired_capacity;
    return (FT_SUCCESS);
}
