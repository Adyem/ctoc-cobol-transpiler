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
    type->constructor_count += 1;
    constructor = &type->constructors[type->constructor_count];
    std::memset(constructor, 0, sizeof(*constructor));
    ft_strlcpy(constructor->parameters[0].type_name, "string",
        sizeof(constructor->parameters[0].type_name));
    constructor->parameters[0].kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
    constructor->parameter_count = 1;
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
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "len", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "clear", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_VOID;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "empty", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
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
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "capacity", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
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
    type->method_count += 1;
    unit->struct_type_count += 1;
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
