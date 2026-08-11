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
        if (constructor->parameters[index].length != parameters[index].length
            || constructor->parameters[index].array_count != parameters[index].array_count)
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

static int cblc_translation_unit_ensure_type_ref_capacity(t_cblc_translation_unit *unit,
    size_t desired_capacity)
{
    t_cblc_type_ref *new_refs;

    if (!unit)
        return (FT_FAILURE);
    if (unit->type_ref_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_refs = static_cast<t_cblc_type_ref *>(cma_calloc(desired_capacity,
        sizeof(*new_refs)));
    if (!new_refs)
        return (FT_FAILURE);
    if (unit->type_refs)
    {
        std::memcpy(new_refs, unit->type_refs,
            unit->type_ref_count * sizeof(*new_refs));
        cma_free(unit->type_refs);
    }
    unit->type_refs = new_refs;
    unit->type_ref_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static unsigned long long cblc_layout_hash_bytes(unsigned long long hash,
    const void *data, size_t length)
{
    const unsigned char *bytes;
    size_t index;

    bytes = static_cast<const unsigned char *>(data);
    index = 0;
    while (index < length)
    {
        hash ^= bytes[index];
        hash *= 1099511628211ULL;
        index += 1;
    }
    return (hash);
}

static unsigned long long cblc_layout_hash_u64(unsigned long long hash,
    unsigned long long value)
{
    unsigned char bytes[8];
    size_t index;

    index = 0;
    while (index < sizeof(bytes))
    {
        bytes[index] = static_cast<unsigned char>((value >> (index * 8)) & 0xffU);
        index += 1;
    }
    return (cblc_layout_hash_bytes(hash, bytes, sizeof(bytes)));
}

static unsigned long long cblc_layout_hash_text(unsigned long long hash,
    const char *text)
{
    size_t length;

    length = text ? std::strlen(text) : 0;
    hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(length));
    if (length > 0)
        hash = cblc_layout_hash_bytes(hash, text, length);
    return (hash);
}

static unsigned long long cblc_struct_layout_fingerprint(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type, size_t depth)
{
    unsigned long long hash;
    size_t index;

    if (!unit || !type || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (0);
    hash = 1469598103934665603ULL;
    hash = cblc_layout_hash_text(hash, "CBLC-LAYOUT@1");
    hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(type->is_class));
    hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(type->field_count));
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;
        const t_cblc_struct_type *nested_type;
        unsigned long long nested_fingerprint;

        field = &type->fields[index];
        hash = cblc_layout_hash_text(hash, field->source_name);
        hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(field->kind));
        hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(field->length));
        hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(field->array_count));
        hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(field->is_const));
        hash = cblc_layout_hash_u64(hash, static_cast<unsigned long long>(field->is_borrowed));
        hash = cblc_layout_hash_text(hash, field->declared_type_name);
        hash = cblc_layout_hash_text(hash, field->struct_type_name);
        nested_type = NULL;
        if (field->kind == CBLC_DATA_KIND_STRUCT)
            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
        nested_fingerprint = nested_type
            ? cblc_struct_layout_fingerprint(unit, nested_type, depth + 1) : 0;
        hash = cblc_layout_hash_u64(hash, nested_fingerprint);
        index += 1;
    }
    return (hash);
}

static size_t cblc_exception_payload_size(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, size_t depth)
{
    size_t total;
    size_t index;

    if (!unit || !type || depth > CBLC_TEMPLATE_PARAMETER_MAX)
        return (0);
    total = 0;
    index = 0;
    while (index < type->field_count)
    {
        const t_cblc_struct_field *field;
        size_t element_size;
        size_t element_count;
        size_t field_size;

        field = &type->fields[index];
        element_count = field->array_count == 0 ? 1 : field->array_count;
        if (field->kind == CBLC_DATA_KIND_INT)
            element_size = sizeof(int);
        else if (field->kind == CBLC_DATA_KIND_CHAR)
            element_size = field->length == 0 ? 1 : field->length;
        else if (field->kind == CBLC_DATA_KIND_STRING)
            element_size = (field->length == 0 ? 1 : field->length) +
                sizeof(size_t) * 2;
        else if (field->kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *nested_type;

            nested_type = cblc_find_struct_type(unit, field->struct_type_name);
            if (!nested_type && field->is_template_parameter)
                element_size = sizeof(int);
            else
                element_size = cblc_exception_payload_size(unit, nested_type, depth + 1);
        }
        else
            element_size = sizeof(void *);
        if (element_size == 0 || element_count > CBLC_EXCEPTION_PAYLOAD_MAX
            || element_size > CBLC_EXCEPTION_PAYLOAD_MAX / element_count)
            return (CBLC_EXCEPTION_PAYLOAD_MAX + 1);
        field_size = element_size * element_count;
        if (total > CBLC_EXCEPTION_PAYLOAD_MAX - field_size)
            return (CBLC_EXCEPTION_PAYLOAD_MAX + 1);
        total += field_size;
        index += 1;
    }
    return (total);
}

static int cblc_type_ref_equal(const t_cblc_type_ref *left,
    const t_cblc_type_ref *right)
{
    size_t index;

    if (!left || !right || left->kind != right->kind
        || left->builtin_kind != right->builtin_kind
        || std::strcmp(left->name, right->name) != 0
        || left->parameter_index != right->parameter_index
        || left->pointer_depth != right->pointer_depth
        || left->length != right->length
        || left->array_count != right->array_count
        || left->layout_fingerprint != right->layout_fingerprint
        || left->argument_count != right->argument_count
        || left->argument_count > CBLC_TYPE_REF_ARGUMENT_MAX)
        return (0);
    index = 0;
    while (index < left->argument_count)
    {
        if (left->argument_ids[index] != right->argument_ids[index])
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_type_ref_append(t_cblc_translation_unit *unit,
    const t_cblc_type_ref *reference, size_t *out_id)
{
    size_t index;

    if (!unit || !reference || !out_id)
        return (FT_FAILURE);
    index = 0;
    while (index < unit->type_ref_count)
    {
        if (cblc_type_ref_equal(&unit->type_refs[index], reference))
        {
            *out_id = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    if (unit->type_ref_count >= unit->type_ref_capacity
        && cblc_translation_unit_ensure_type_ref_capacity(unit,
            unit->type_ref_capacity == 0 ? 4 : unit->type_ref_capacity * 2) != FT_SUCCESS)
        return (FT_FAILURE);
    unit->type_refs[unit->type_ref_count] = *reference;
    *out_id = unit->type_ref_count;
    unit->type_ref_count += 1;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_template_definition_capacity(
    t_cblc_translation_unit *unit, size_t desired_capacity)
{
    t_cblc_template_definition *definitions;

    if (!unit)
        return (FT_FAILURE);
    if (unit->template_definition_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    definitions = static_cast<t_cblc_template_definition *>(cma_calloc(
        desired_capacity, sizeof(*definitions)));
    if (!definitions)
        return (FT_FAILURE);
    if (unit->template_definitions)
    {
        std::memcpy(definitions, unit->template_definitions,
            unit->template_definition_count * sizeof(*definitions));
        cma_free(unit->template_definitions);
    }
    unit->template_definitions = definitions;
    unit->template_definition_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_template_instantiation_capacity(
    t_cblc_translation_unit *unit, size_t desired_capacity)
{
    t_cblc_template_instantiation *instantiations;

    if (!unit)
        return (FT_FAILURE);
    if (unit->template_instantiation_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    instantiations = static_cast<t_cblc_template_instantiation *>(cma_calloc(
        desired_capacity, sizeof(*instantiations)));
    if (!instantiations)
        return (FT_FAILURE);
    if (unit->template_instantiations)
    {
        std::memcpy(instantiations, unit->template_instantiations,
            unit->template_instantiation_count * sizeof(*instantiations));
        cma_free(unit->template_instantiations);
    }
    unit->template_instantiations = instantiations;
    unit->template_instantiation_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_function_instantiation_capacity(
    t_cblc_translation_unit *unit, size_t desired_capacity)
{
    t_cblc_function_instantiation *instantiations;

    if (!unit)
        return (FT_FAILURE);
    if (unit->function_instantiation_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 2)
        desired_capacity = 2;
    instantiations = static_cast<t_cblc_function_instantiation *>(cma_calloc(
        desired_capacity, sizeof(*instantiations)));
    if (!instantiations)
        return (FT_FAILURE);
    if (unit->function_instantiations)
    {
        std::memcpy(instantiations, unit->function_instantiations,
            unit->function_instantiation_count * sizeof(*instantiations));
        cma_free(unit->function_instantiations);
    }
    unit->function_instantiations = instantiations;
    unit->function_instantiation_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_register_string_class_method(t_cblc_struct_type *type,
    const char *name, t_cblc_function_return_kind return_kind,
    const char *return_type_name, const char *program_name,
    const t_transpiler_function_parameter_kind *parameter_kinds,
    const char *const *parameter_type_names, size_t parameter_count)
{
    t_cblc_method *method;
    size_t index;

    if (!type || !name || !program_name || parameter_count > TRANSPILE_FUNCTION_PARAMETER_MAX
        || (parameter_count > 0 && (!parameter_kinds || !parameter_type_names)))
        return (FT_FAILURE);
    if (type->method_count >= type->method_capacity)
        return (FT_FAILURE);
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, name, sizeof(method->source_name));
    cblc_identifier_to_cobol(name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = return_kind;
    if (return_type_name)
        ft_strlcpy(method->return_type_name, return_type_name,
            sizeof(method->return_type_name));
    ft_strlcpy(method->implementation_program_name, program_name,
        sizeof(method->implementation_program_name));
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    method->has_definition = 0;
    index = 0;
    while (index < parameter_count)
    {
        method->parameters[index].kind = parameter_kinds[index];
        if (parameter_type_names[index])
            ft_strlcpy(method->parameters[index].type_name, parameter_type_names[index],
                sizeof(method->parameters[index].type_name));
        index += 1;
    }
    method->parameter_count = parameter_count;
    type->method_count += 1;
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
    if (cblc_struct_type_ensure_method_capacity(type, 24) != FT_SUCCESS)
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
    {
        const t_transpiler_function_parameter_kind string_parameter[] = {
            TRANSPILE_FUNCTION_PARAMETER_STRING
        };
        const t_transpiler_function_parameter_kind string_string_parameters[] = {
            TRANSPILE_FUNCTION_PARAMETER_STRING, TRANSPILE_FUNCTION_PARAMETER_STRING
        };
        const t_transpiler_function_parameter_kind string_int_parameters[] = {
            TRANSPILE_FUNCTION_PARAMETER_STRING, TRANSPILE_FUNCTION_PARAMETER_INT
        };
        const t_transpiler_function_parameter_kind int_parameter[] = {
            TRANSPILE_FUNCTION_PARAMETER_INT
        };
        const char *string_type[] = {"string"};
        const char *string_string_types[] = {"string", "string"};
        const char *string_int_types[] = {"string", "int"};
        const char *int_type[] = {"int"};

        if (cblc_register_string_class_method(type, "atoi",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-ATOI-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "atol",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-ATOL-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "atoll",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-ATOLL-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strlen",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-STRLEN-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strnlen",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-STRNLEN-STRING",
                int_parameter, int_type, 1) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strcmp",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-STRCMP-STRING",
                string_parameter, string_type, 1) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strcpy",
                CBLC_FUNCTION_RETURN_VOID, NULL, "CBLC-STRCPY-STRING",
                string_parameter, string_type, 1) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strncpy",
                CBLC_FUNCTION_RETURN_VOID, NULL, "CBLC-STRNCPY-STRING",
                string_int_parameters, string_int_types, 2) != FT_SUCCESS
            || cblc_register_string_class_method(type, "memcmp",
                CBLC_FUNCTION_RETURN_INT, "int", "CBLC-MEMCMP-STRING",
                string_int_parameters, string_int_types, 2) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strcat",
                CBLC_FUNCTION_RETURN_VOID, NULL, "CBLC-STRCAT-STRING",
                string_string_parameters, string_string_types, 2) != FT_SUCCESS
            || cblc_register_string_class_method(type, "strtod",
                CBLC_FUNCTION_RETURN_DOUBLE, "double", "CBLC-STRTOD-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "toupper",
                CBLC_FUNCTION_RETURN_VOID, NULL, "CBLC-TOUPPER-STRING",
                NULL, NULL, 0) != FT_SUCCESS
            || cblc_register_string_class_method(type, "tolower",
                CBLC_FUNCTION_RETURN_VOID, NULL, "CBLC-TOLOWER-STRING",
                NULL, NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
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
        type->has_base_type = signature->has_base_type;
        type->base_is_public = signature->base_is_public;
        type->exception_abi_version = signature->exception_abi_version;
        type->exception_type_id = signature->exception_type_id;
        type->exception_payload_size = signature->exception_payload_size;
        ft_strlcpy(type->base_type_name, signature->base_type_name,
            sizeof(type->base_type_name));
        type->is_class = signature->kind == TRANSPILE_TYPE_CLASS;
        type->is_imported = 1;
        type->has_base_type = signature->has_base_type;
        type->base_is_public = signature->base_is_public;
        type->exception_abi_version = signature->exception_abi_version;
        type->exception_type_id = signature->exception_type_id;
        type->exception_payload_size = signature->exception_payload_size;
        ft_strlcpy(type->base_type_name, signature->base_type_name,
            sizeof(type->base_type_name));
        unit->struct_type_count += 1;
    }
    else
    {
        unsigned long long existing_layout_fingerprint;

        existing_layout_fingerprint = cblc_struct_layout_fingerprint(unit, type, 0);
        if (signature->layout_fingerprint != 0
            && existing_layout_fingerprint != 0
            && signature->layout_fingerprint != existing_layout_fingerprint)
            return (FT_FAILURE);
        if (!type->is_imported)
            return (FT_SUCCESS);
        type->is_class = signature->kind == TRANSPILE_TYPE_CLASS;
        type->is_imported = 1;
    }
    type->has_default_constructor = signature->has_default_constructor;
    type->has_destructor = signature->has_destructor;
    type->exception_payload_size = signature->exception_payload_size;
    type->layout_fingerprint = signature->layout_fingerprint;
    type->is_template = signature->is_template;
    type->template_parameter_count = signature->template_parameter_count;
    size_t template_parameter_index = 0;
    while (template_parameter_index < type->template_parameter_count
        && template_parameter_index < CBLC_TEMPLATE_PARAMETER_MAX)
    {
        ft_strlcpy(type->template_parameter_names[template_parameter_index],
            signature->template_parameter_names[template_parameter_index],
            sizeof(type->template_parameter_names[template_parameter_index]));
        template_parameter_index += 1;
    }
    ft_strlcpy(type->template_parameter_name, signature->template_parameter_name,
        sizeof(type->template_parameter_name));
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
            type->fields[index].is_borrowed = signature->fields[index].is_borrowed;
            type->fields[index].is_template_parameter =
                signature->fields[index].is_template_parameter;
            type->fields[index].template_parameter_pointer_depth =
                signature->fields[index].template_parameter_pointer_depth;
            ft_strlcpy(type->fields[index].template_parameter_name,
                signature->fields[index].template_parameter_name,
                sizeof(type->fields[index].template_parameter_name));
            if (signature->fields[index].visibility == TRANSPILE_SYMBOL_PUBLIC)
                type->fields[index].visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
            else
                type->fields[index].visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
            index += 1;
        }
        type->field_count = signature->field_count;
    }
    if (type->layout_fingerprint == 0)
        type->layout_fingerprint = cblc_struct_layout_fingerprint(unit, type, 0);
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
            if (std::strncmp(type->methods[index].cobol_name, "CBLC-USER-", 10) == 0)
                std::memmove(type->methods[index].cobol_name,
                    type->methods[index].cobol_name + 10,
                    std::strlen(type->methods[index].cobol_name + 10) + 1);
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
                    type->methods[index].parameters[parameter_index].array_count =
                    signature->methods[index].parameter_array_counts[parameter_index];
                type->methods[index].parameters[parameter_index].length =
                    signature->methods[index].parameter_lengths[parameter_index];
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
            type->methods[index].exception_type_count =
                signature->methods[index].exception_type_count;
            std::memcpy(type->methods[index].exception_type_ids,
                signature->methods[index].exception_type_ids,
                sizeof(type->methods[index].exception_type_ids));
            type->methods[index].exception_types_unknown =
                signature->methods[index].exception_types_unknown;
            if (cblc_register_imported_parameter_storage(unit,
                    type->methods[index].parameters,
                    type->methods[index].parameter_count, 1) != FT_SUCCESS)
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
                    type->constructors[index].parameters[parameter_index].array_count =
                    signature->constructors[index].parameter_array_counts[parameter_index];
                type->constructors[index].parameters[parameter_index].length =
                    signature->constructors[index].parameter_lengths[parameter_index];
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
            type->constructors[index].exception_type_count =
                signature->constructors[index].exception_type_count;
            std::memcpy(type->constructors[index].exception_type_ids,
                signature->constructors[index].exception_type_ids,
                sizeof(type->constructors[index].exception_type_ids));
            type->constructors[index].exception_types_unknown =
                signature->constructors[index].exception_types_unknown;
            if (cblc_register_imported_parameter_storage(unit,
                    type->constructors[index].parameters,
                    type->constructors[index].parameter_count, 1) != FT_SUCCESS)
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
    const t_cblc_parameter *parameters, size_t parameter_count, int is_imported)
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
                    parameters[index].cobol_name, kind, parameters[index].length,
                    parameters[index].array_count,
                    parameters[index].type_name,
                    NULL, 0, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            unit->data_items[unit->data_count - 1].is_imported = is_imported;
            if (cblc_add_struct_instance_field_items(unit, type,
                    parameters[index].actual_source_name, parameters[index].cobol_name,
                    NULL, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_add_named_data_item(unit, parameters[index].actual_source_name,
                    parameters[index].cobol_name, kind, parameters[index].length,
                    parameters[index].array_count,
                    parameters[index].type_name,
                    NULL, 0, 0, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            unit->data_items[unit->data_count - 1].is_imported = is_imported;
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_register_imported_data_stub(t_cblc_translation_unit *unit,
    const t_transpiler_data_signature *signature)
{
    t_cblc_data_item *item;
    char source_name[TRANSPILE_IDENTIFIER_MAX];
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !signature || signature->name[0] == '\0')
        return (FT_FAILURE);
    std::memset(source_name, 0, sizeof(source_name));
    std::memset(cobol_name, 0, sizeof(cobol_name));
    ft_strlcpy(source_name, signature->name, sizeof(source_name));
    cblc_identifier_to_cobol(source_name, cobol_name, sizeof(cobol_name));
    if (cblc_add_named_data_item(unit, signature->name, cobol_name,
            static_cast<t_cblc_data_kind>(signature->kind), signature->length,
            signature->array_count, signature->struct_type_name, NULL, 0, 0,
            signature->is_const) != FT_SUCCESS)
        return (FT_FAILURE);
    item = &unit->data_items[unit->data_count - 1];
    item->is_imported = 1;
    if (signature->declared_type_name[0] != '\0')
        ft_strlcpy(item->declared_type_name, signature->declared_type_name,
            sizeof(item->declared_type_name));
    if (signature->struct_type_name[0] != '\0')
        ft_strlcpy(item->struct_type_name, signature->struct_type_name,
            sizeof(item->struct_type_name));
    item->has_initializer = signature->has_initializer;
    item->initializer_length = signature->initializer_length;
    ft_strlcpy(item->initializer_text, signature->initializer_text,
        sizeof(item->initializer_text));
    ft_strlcpy(item->constructor_arguments, signature->constructor_arguments,
        sizeof(item->constructor_arguments));
    item->constructor_argument_count = signature->constructor_argument_count;
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
