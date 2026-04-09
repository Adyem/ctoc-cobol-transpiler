#include "cblc_transpiler.hpp"

#include "compatibility/memory_compat.hpp"
#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

typedef struct s_cobol_text_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cobol_text_builder;

typedef struct s_cblc_constructor_parse_state
{
    const t_cblc_struct_type *type;
    int *initialized_fields;
    int active;
}   t_cblc_constructor_parse_state;

static t_cblc_constructor_parse_state g_cblc_constructor_parse_state = {NULL, NULL, 0};
static const t_cblc_struct_type *g_cblc_member_access_type = NULL;

static int cblc_parse_std_strcpy(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity);
static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity);
static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_parse_local_string_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function);
static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count);
static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count);
static int cblc_parse_statement_block(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int allow_return);
static int cblc_parse_numeric_expression(const char **cursor, t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size);
static int cblc_parse_numeric_expression_until_paren(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size);
static int cblc_parse_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size,
    size_t *out_count);
static int cblc_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size);
static int cblc_argument_matches_parameter(const t_cblc_translation_unit *unit,
    const char *argument, const t_cblc_parameter *parameter);
static int cblc_normalize_call_arguments(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count, char *buffer,
    size_t buffer_size);
static int cblc_parse_string_constructor_clause(const char **cursor,
    t_cblc_translation_unit *unit, size_t *out_length, char *out_arguments,
    size_t out_arguments_size, size_t *out_argument_count);
static int cblc_expression_append(char *buffer, size_t buffer_size, const char *token);
static int cblc_starts_with_function_declaration(const t_cblc_translation_unit *unit,
    const char *cursor);
static int cblc_parse_parameter_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_parameter *parameters,
    size_t *parameter_count, const char *scope_source_name,
    const char *scope_cobol_name, const char *owner_function_name,
    const t_cblc_struct_type *self_type);
static int cblc_add_parameter_aliases(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count,
    const t_cblc_struct_type *self_type);
static int cblc_add_local_struct_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name,
    const char *actual_cobol_name);
static void cblc_build_constructor_scope_name(const char *base, const char *separator,
    size_t arity, char *buffer, size_t buffer_size);
static int cblc_parse_function_return_type(const char **cursor,
    const t_cblc_translation_unit *unit, t_cblc_function_return_kind *out_kind,
    char *type_name, size_t type_name_size);
static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item);
static const t_cblc_data_item *cblc_find_data_item_by_cobol(
    const t_cblc_translation_unit *unit, const char *identifier);
static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, const char *owner_function_name, int is_function_local,
    int is_alias, int is_const);
static int cblc_constructor_signatures_match(const t_cblc_constructor *constructor,
    const t_cblc_parameter *parameters, size_t parameter_count);
static const t_cblc_constructor *cblc_find_constructor_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *call_arguments, size_t call_argument_count);

static void cobol_text_builder_init(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static void cobol_text_builder_dispose(t_cobol_text_builder *builder)
{
    if (!builder)
        return ;
    if (builder->data)
        cma_free(builder->data);
    builder->data = NULL;
    builder->length = 0;
    builder->capacity = 0;
}

static int cobol_text_builder_reserve(t_cobol_text_builder *builder, size_t desired_capacity)
{
    char *new_data;

    if (!builder)
        return (FT_FAILURE);
    if (builder->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 128)
        desired_capacity = 128;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (builder->data && builder->length > 0)
        std::memcpy(new_data, builder->data, builder->length);
    if (builder->data)
        cma_free(builder->data);
    builder->data = new_data;
    builder->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_span(t_cobol_text_builder *builder, const char *text, size_t length)
{
    if (!builder)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (cobol_text_builder_reserve(builder, builder->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    std::memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_string(t_cobol_text_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cobol_text_builder_append_span(builder, text, std::strlen(text)));
}

static int cobol_text_builder_append_line(t_cobol_text_builder *builder, const char *text)
{
    if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_match_keyword(const char **cursor, const char *keyword)
{
    size_t length;

    if (!cursor || !*cursor || !keyword)
        return (0);
    length = std::strlen(keyword);
    if (std::strncmp(*cursor, keyword, length) != 0)
        return (0);
    if ((*cursor)[length] != '\0' && ((*cursor)[length] == '_' || ((*cursor)[length] >= 'a'
                && (*cursor)[length] <= 'z') || ((*cursor)[length] >= 'A'
                && (*cursor)[length] <= 'Z') || ((*cursor)[length] >= '0'
                && (*cursor)[length] <= '9')))
        return (0);
    *cursor += length;
    return (1);
}

static void cblc_skip_whitespace(const char **cursor)
{
    if (!cursor || !*cursor)
        return ;
    while (**cursor == ' ' || **cursor == '\t' || **cursor == '\n' || **cursor == '\r')
        *cursor += 1;
}

static int cblc_is_identifier_char(char character)
{
    if (character == '_')
        return (1);
    if (character >= 'a' && character <= 'z')
        return (1);
    if (character >= 'A' && character <= 'Z')
        return (1);
    if (character >= '0' && character <= '9')
        return (1);
    return (0);
}

static int cblc_parse_identifier(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    length = 0;
    while (cblc_is_identifier_char((*cursor)[length]))
        length += 1;
    while ((*cursor)[length] == ':' && (*cursor)[length + 1] == ':'
        && (*cursor)[length + 2] != '\0'
        && cblc_is_identifier_char((*cursor)[length + 2]))
    {
        length += 2;
        while (cblc_is_identifier_char((*cursor)[length]))
            length += 1;
    }
    if (length == 0)
        return (FT_FAILURE);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, *cursor, length);
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static void cblc_identifier_to_cobol(const char *identifier, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!identifier || !buffer || buffer_size == 0)
        return ;
    index = 0;
    while (identifier[index] != '\0' && index + 1 < buffer_size)
    {
        char character;

        character = identifier[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        else if (character == '_')
            character = '-';
        buffer[index] = character;
        index += 1;
    }
    buffer[index] = '\0';
}

static void cblc_build_string_component_name(const char *base, const char *suffix, char *buffer,
    size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return ;
    if (!base)
    {
        buffer[0] = '\0';
        return ;
    }
    ft_strlcpy(buffer, base, buffer_size);
    if (suffix)
        ft_strlcat(buffer, suffix, buffer_size);
}

static int cblc_parse_numeric_literal(const char **cursor, size_t *out_value)
{
    size_t value;
    const char *start;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    value = 0;
    start = *cursor;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    if (*cursor == start)
        return (FT_FAILURE);
    *out_value = value;
    return (FT_SUCCESS);
}

static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size);

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
    if (cblc_struct_type_ensure_constructor_capacity(type, 1) != FT_SUCCESS)
        return (FT_FAILURE);
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

static int cblc_parse_char_literal(const char **cursor, char *out_value)
{
    char value;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    if (**cursor != '\'')
        return (FT_FAILURE);
    *cursor += 1;
    if (**cursor == '\0')
        return (FT_FAILURE);
    if (**cursor == '\\')
    {
        *cursor += 1;
        if (**cursor == 'n')
            value = '\n';
        else if (**cursor == 'r')
            value = '\r';
        else if (**cursor == 't')
            value = '\t';
        else if (**cursor == '\\')
            value = '\\';
        else if (**cursor == '\'')
            value = '\'';
        else if (**cursor == '"')
            value = '"';
        else
            return (FT_FAILURE);
    }
    else
    {
        value = **cursor;
    }
    if (**cursor == '\0')
        return (FT_FAILURE);
    *cursor += 1;
    if (**cursor != '\'')
        return (FT_FAILURE);
    *cursor += 1;
    *out_value = value;
    return (FT_SUCCESS);
}

static int cblc_parse_char_const_initializer(const char **cursor, char *buffer, size_t buffer_size,
    size_t length, size_t *out_initializer_length)
{
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_value;
    size_t literal_length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (!out_initializer_length)
        return (FT_FAILURE);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        literal_length = std::strlen(literal_buffer);
        if (length != 0 && literal_length > length)
            return (FT_FAILURE);
        buffer[0] = '"';
        buffer[1] = '\0';
        ft_strlcat(buffer, literal_buffer, buffer_size);
        ft_strlcat(buffer, "\"", buffer_size);
        *out_initializer_length = literal_length;
        return (FT_SUCCESS);
    }
    if (**cursor != '\'')
        return (FT_FAILURE);
    if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
        return (FT_FAILURE);
    if (length != 0 && length < 1)
        return (FT_FAILURE);
    buffer[0] = '\'';
    buffer[1] = literal_value;
    buffer[2] = '\'';
    buffer[3] = '\0';
    *out_initializer_length = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_string_const_initializer(const char **cursor, char *buffer, size_t buffer_size,
    size_t length, size_t *out_initializer_length)
{
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_value;
    size_t literal_length;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (!out_initializer_length)
        return (FT_FAILURE);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        literal_length = std::strlen(literal_buffer);
        if (length != 0 && literal_length > length)
            return (FT_FAILURE);
        buffer[0] = '"';
        buffer[1] = '\0';
        ft_strlcat(buffer, literal_buffer, buffer_size);
        ft_strlcat(buffer, "\"", buffer_size);
        *out_initializer_length = literal_length;
        return (FT_SUCCESS);
    }
    if (**cursor != '\'')
        return (FT_FAILURE);
    if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
        return (FT_FAILURE);
    if (length != 0 && length < 1)
        return (FT_FAILURE);
    buffer[0] = '"';
    buffer[1] = literal_value;
    buffer[2] = '"';
    buffer[3] = '\0';
    *out_initializer_length = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_int_const_initializer(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t literal_value;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_numeric_literal(cursor, &literal_value) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%zu", literal_value) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_array_count_clause(const char **cursor, size_t *out_count)
{
    if (!cursor || !*cursor || !out_count)
        return (FT_FAILURE);
    if (**cursor != '[')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_numeric_literal(cursor, out_count) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ']')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    return (FT_SUCCESS);
}

static int cblc_parse_string_capacity_clause(const char **cursor, size_t *out_length)
{
    if (!cursor || !*cursor || !out_length)
        return (FT_FAILURE);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_numeric_literal(cursor, out_length) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    return (FT_SUCCESS);
}

static int cblc_finalize_data_item(t_cblc_translation_unit *unit, const char *identifier,
    size_t length, size_t array_count, t_cblc_data_kind kind, int is_const, int has_initializer,
    const char *initializer_text, size_t initializer_length)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (FT_FAILURE);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(item->declared_type_name, "string", sizeof(item->declared_type_name));
    item->length = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_active = 1;
    item->is_const = is_const;
    item->has_initializer = has_initializer;
    item->initializer_length = initializer_length;
    item->initializer_text[0] = '\0';
    if (initializer_text)
        ft_strlcpy(item->initializer_text, initializer_text, sizeof(item->initializer_text));
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_char_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t initializer_length;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "char"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    length = 1;
    if (**cursor == '[')
    {
        size_t candidate_length;
        int saw_dynamic_dimension;

        *cursor += 1;
        cblc_skip_whitespace(cursor);
        candidate_length = 1;
        saw_dynamic_dimension = 0;
        if (cblc_parse_numeric_literal(cursor, &candidate_length) == FT_SUCCESS)
        {
            length = candidate_length;
        }
        else
        {
            char dimension_identifier[TRANSPILE_IDENTIFIER_MAX];

            if (cblc_parse_identifier(cursor, dimension_identifier,
                    sizeof(dimension_identifier)) != FT_SUCCESS)
                return (FT_FAILURE);
            saw_dynamic_dimension = 1;
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ']')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == '[')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            if (cblc_parse_numeric_literal(cursor, &candidate_length) != FT_SUCCESS)
                return (FT_FAILURE);
            length = candidate_length;
            cblc_skip_whitespace(cursor);
            if (**cursor != ']')
                return (FT_FAILURE);
            *cursor += 1;
            cblc_skip_whitespace(cursor);
        }
        else if (saw_dynamic_dimension)
            length = 0;
    }
    initializer_text[0] = '\0';
    initializer_length = 0;
    if (is_const)
    {
        if (length == 0)
            return (FT_FAILURE);
        if (**cursor != '=')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_char_const_initializer(cursor, initializer_text,
                sizeof(initializer_text), length, &initializer_length) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_finalize_data_item(unit, identifier, length, 0, CBLC_DATA_KIND_CHAR,
            is_const, is_const, initializer_text, initializer_length));
}

static int cblc_parse_string_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t array_count;
    size_t initializer_length;
    size_t constructor_argument_count;
    const char *after_array;
    const char *constructor_start;
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "string"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    length = 1;
    array_count = 0;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (**cursor == '[')
    {
        after_array = *cursor;
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
            return (FT_FAILURE);
        if (**cursor == '(')
        {
            if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            *cursor = after_array;
            if (cblc_parse_array_count_clause(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
            array_count = 0;
        }
    }
    else if (**cursor == '(')
    {
        constructor_start = *cursor;
        if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
        {
            *cursor = constructor_start;
            if (cblc_parse_string_constructor_clause(cursor, unit, &length,
                    constructor_arguments, sizeof(constructor_arguments),
                    &constructor_argument_count) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    initializer_text[0] = '\0';
    initializer_length = 0;
    if (is_const)
    {
        if (length == 0 || array_count > 0 || constructor_argument_count > 0)
            return (FT_FAILURE);
        if (**cursor != '=')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_string_const_initializer(cursor, initializer_text,
                sizeof(initializer_text), length, &initializer_length) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (array_count > 0 && constructor_argument_count > 0)
        return (FT_FAILURE);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_finalize_data_item(unit, identifier, length, array_count, CBLC_DATA_KIND_STRING,
            is_const, is_const, initializer_text, initializer_length) != FT_SUCCESS)
        return (FT_FAILURE);
    if (constructor_argument_count == 0)
        return (FT_SUCCESS);
    item = &unit->data_items[unit->data_count - 1];
    ft_strlcpy(item->constructor_arguments, constructor_arguments,
        sizeof(item->constructor_arguments));
    item->constructor_argument_count = constructor_argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_int_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t array_count;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "int"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    array_count = 0;
    if (**cursor == '[')
    {
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    initializer_text[0] = '\0';
    if (is_const)
    {
        if (array_count > 0)
            return (FT_FAILURE);
        if (**cursor != '=')
            return (FT_FAILURE);
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_int_const_initializer(cursor, initializer_text,
                sizeof(initializer_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_finalize_data_item(unit, identifier, 0, array_count, CBLC_DATA_KIND_INT,
            is_const, is_const, initializer_text, 0));
}

static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!cursor || !*cursor || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (**cursor != '"')
        return (FT_FAILURE);
    *cursor += 1;
    index = 0;
    while (**cursor != '\0' && **cursor != '"')
    {
        if (index + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[index] = **cursor;
        index += 1;
        *cursor += 1;
    }
    if (**cursor != '"')
        return (FT_FAILURE);
    buffer[index] = '\0';
    *cursor += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_import(const char **cursor, t_cblc_translation_unit *unit)
{
    char path[TRANSPILE_FILE_PATH_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "import"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_string_literal(cursor, path, sizeof(path)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->import_count >= unit->import_capacity)
    {
        if (cblc_translation_unit_ensure_import_capacity(unit,
                unit->import_capacity == 0 ? 4 : unit->import_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->imports[unit->import_count].path, path,
        sizeof(unit->imports[unit->import_count].path));
    unit->import_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_copy(const char **cursor, t_cblc_translation_unit *unit)
{
    char name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "copy"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_string_literal(cursor, name, sizeof(name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->copy_include_count >= unit->copy_include_capacity)
    {
        if (cblc_translation_unit_ensure_copy_capacity(unit,
                unit->copy_include_capacity == 0 ? 4 : unit->copy_include_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(unit->copy_includes[unit->copy_include_count].name, name,
        sizeof(unit->copy_includes[unit->copy_include_count].name));
    unit->copy_include_count += 1;
    return (FT_SUCCESS);
}

static t_cblc_data_item *cblc_find_data_item(t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = unit->data_count;
    while (index > 0)
    {
        index -= 1;
        if (!unit->data_items[index].is_active)
            continue ;
        if (std::strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (&unit->data_items[index]);
    }
    return (NULL);
}

static const t_cblc_data_item *cblc_find_data_item_const(
    const t_cblc_translation_unit *unit, const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = unit->data_count;
    while (index > 0)
    {
        index -= 1;
        if (!unit->data_items[index].is_active)
            continue ;
        if (std::strncmp(unit->data_items[index].source_name, identifier,
                sizeof(unit->data_items[index].source_name)) == 0)
            return (&unit->data_items[index]);
    }
    return (NULL);
}

static int cblc_parse_string_constructor_clause(const char **cursor,
    t_cblc_translation_unit *unit, size_t *out_length, char *out_arguments,
    size_t out_arguments_size, size_t *out_argument_count)
{
    const t_cblc_data_item *actual_item;
    const t_cblc_data_item *item;
    const char *start;
    const char *literal_cursor;
    t_cblc_statement statement;
    char argument[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit || !out_length || !out_arguments
        || out_arguments_size == 0 || !out_argument_count)
        return (FT_FAILURE);
    start = *cursor;
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, out_arguments, out_arguments_size,
            out_argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (*out_argument_count != 1)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, out_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = *out_argument_count;
    if (cblc_extract_call_argument(&statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (argument[0] == '"')
    {
        literal_cursor = argument;
        if (cblc_parse_string_literal(&literal_cursor, literal_buffer, sizeof(literal_buffer))
            != FT_SUCCESS || *literal_cursor != '\0')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *out_length = std::strlen(literal_buffer);
        if (*out_length == 0)
            *out_length = 1;
        return (FT_SUCCESS);
    }
    item = cblc_find_data_item_const(unit, argument);
    if (!item || item->kind != CBLC_DATA_KIND_STRING || item->array_count > 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    actual_item = cblc_find_data_item_by_cobol(unit, item->cobol_name);
    if (actual_item && !actual_item->is_alias)
        ft_strlcpy(out_arguments, actual_item->source_name, out_arguments_size);
    *out_length = item->length;
    if (*out_length == 0)
        *out_length = 1;
    return (FT_SUCCESS);
}

static const t_cblc_data_item *cblc_find_data_item_by_cobol(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;
    const t_cblc_data_item *alias_match;

    if (!unit || !identifier)
        return (NULL);
    alias_match = NULL;
    index = 0;
    while (index < unit->data_count)
    {
        if (std::strncmp(unit->data_items[index].cobol_name, identifier,
                sizeof(unit->data_items[index].cobol_name)) == 0)
        {
            if (!unit->data_items[index].is_active)
            {
                index += 1;
                continue ;
            }
            if (!unit->data_items[index].is_alias)
                return (&unit->data_items[index]);
            if (!alias_match)
                alias_match = &unit->data_items[index];
        }
        index += 1;
    }
    return (alias_match);
}

static const t_cblc_struct_type *cblc_find_struct_type(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->struct_type_count)
    {
        if (std::strncmp(unit->struct_types[index].source_name, identifier,
                sizeof(unit->struct_types[index].source_name)) == 0)
            return (&unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_method *cblc_find_method_on_type(const t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;

    if (!type || !identifier)
        return (NULL);
    index = 0;
    while (index < type->method_count)
    {
        if (std::strncmp(type->methods[index].source_name, identifier,
                sizeof(type->methods[index].source_name)) == 0)
            return (&type->methods[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_struct_field *cblc_find_field_on_type(const t_cblc_struct_type *type,
    const char *identifier)
{
    size_t index;

    if (!type || !identifier)
        return (NULL);
    index = 0;
    while (index < type->field_count)
    {
        if (std::strncmp(type->fields[index].source_name, identifier,
                sizeof(type->fields[index].source_name)) == 0)
            return (&type->fields[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_member_visibility_is_allowed(const t_cblc_struct_type *owner_type,
    t_cblc_member_visibility visibility)
{
    if (visibility == CBLC_MEMBER_VISIBILITY_PUBLIC)
        return (1);
    if (!owner_type || !g_cblc_member_access_type)
        return (0);
    if (std::strncmp(owner_type->source_name, g_cblc_member_access_type->source_name,
            sizeof(owner_type->source_name)) == 0)
        return (1);
    return (0);
}

static int cblc_parse_member_visibility_label(const char **cursor,
    t_cblc_member_visibility *current_visibility)
{
    const char *start;
    t_cblc_member_visibility visibility;

    if (!cursor || !*cursor || !current_visibility)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_match_keyword(cursor, "private"))
        visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
    else if (cblc_match_keyword(cursor, "public"))
        visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ':')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    *current_visibility = visibility;
    return (FT_SUCCESS);
}

static const t_cblc_struct_type *cblc_find_declared_type_for_item(
    const t_cblc_translation_unit *unit, const t_cblc_data_item *item)
{
    if (!unit || !item || item->declared_type_name[0] == '\0')
        return (NULL);
    if (g_cblc_member_access_type
        && std::strncmp(item->declared_type_name, g_cblc_member_access_type->source_name,
            sizeof(item->declared_type_name)) == 0)
        return (g_cblc_member_access_type);
    return (cblc_find_struct_type(unit, item->declared_type_name));
}

static int cblc_type_requires_default_construct(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!unit || !type)
        return (0);
    if (type->has_default_constructor)
        return (1);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (field_type && cblc_type_requires_default_construct(unit, field_type))
                return (1);
        }
        index += 1;
    }
    return (0);
}

static int cblc_type_requires_destruct(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type)
{
    size_t index;

    if (!unit || !type)
        return (0);
    if (type->has_destructor)
        return (1);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (field_type && cblc_type_requires_destruct(unit, field_type))
                return (1);
        }
        index += 1;
    }
    return (0);
}

static int cblc_item_requires_default_construct(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_struct_type *type;

    if (!unit || !item)
        return (0);
    if (item->constructor_argument_count > 0)
        return (1);
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        type = cblc_find_struct_type(unit, item->struct_type_name);
        return (type && cblc_type_requires_default_construct(unit, type));
    }
    type = cblc_find_declared_type_for_item(unit, item);
    return (type && cblc_type_requires_default_construct(unit, type));
}

static int cblc_item_requires_destruct(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_struct_type *type;

    if (!unit || !item)
        return (0);
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        type = cblc_find_struct_type(unit, item->struct_type_name);
        return (type && cblc_type_requires_destruct(unit, type));
    }
    type = cblc_find_declared_type_for_item(unit, item);
    return (type && cblc_type_requires_destruct(unit, type));
}

static void cblc_build_field_source_name(const char *base, const char *field, char *buffer,
    size_t buffer_size)
{
    if (!buffer || buffer_size == 0)
        return ;
    if (!base || !field)
    {
        buffer[0] = '\0';
        return ;
    }
    if (std::snprintf(buffer, buffer_size, "%s.%s", base, field) < 0)
        buffer[0] = '\0';
}

static int cblc_parse_array_literal_index(const char **cursor, size_t *out_index)
{
    size_t value;

    if (!cursor || !*cursor || !out_index)
        return (FT_FAILURE);
    if (**cursor != '[')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor < '0' || **cursor > '9')
        return (FT_FAILURE);
    value = 0;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ']')
        return (FT_FAILURE);
    *cursor += 1;
    *out_index = value;
    return (FT_SUCCESS);
}

static int cblc_parse_array_variable_index(const char **cursor, t_cblc_translation_unit *unit,
    char *out_expression, size_t out_expression_size)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !out_expression || out_expression_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item || item->kind != CBLC_DATA_KIND_INT || item->array_count > 0)
        return (FT_FAILURE);
    ft_strlcpy(out_expression, item->cobol_name, out_expression_size);
    return (FT_SUCCESS);
}

static int cblc_format_occurs_reference(const char *base_name, size_t zero_based_index,
    char *buffer, size_t buffer_size)
{
    if (!base_name || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s(%zu)", base_name, zero_based_index + 1) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_expression_indexed_reference(const char *base_name, const char *expression,
    char *buffer, size_t buffer_size)
{
    if (!base_name || !expression || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s[%s]", base_name, expression) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_string_component_reference(const char *base_name, size_t has_index,
    size_t zero_based_index, const char *component_suffix, char *buffer, size_t buffer_size)
{
    if (!base_name || !component_suffix || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (has_index)
    {
        if (std::snprintf(buffer, buffer_size, "%s%s(%zu)", base_name, component_suffix,
                zero_based_index + 1) < 0)
            return (FT_FAILURE);
    }
    else if (std::snprintf(buffer, buffer_size, "%s%s", base_name, component_suffix) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_format_string_component_expression_reference(const char *base_name,
    const char *expression, const char *component_suffix, char *buffer, size_t buffer_size)
{
    if (!base_name || !expression || !component_suffix || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    if (std::snprintf(buffer, buffer_size, "%s%s[%s]", base_name, component_suffix,
            expression) < 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_occurs_reference_index(const char *reference, size_t *out_zero_based_index)
{
    const char *open_paren;
    size_t value;

    if (!reference || !out_zero_based_index)
        return (FT_FAILURE);
    open_paren = std::strchr(reference, '(');
    if (!open_paren)
        return (FT_FAILURE);
    open_paren += 1;
    if (*open_paren < '0' || *open_paren > '9')
        return (FT_FAILURE);
    value = 0;
    while (*open_paren >= '0' && *open_paren <= '9')
    {
        value = value * 10 + static_cast<size_t>(*open_paren - '0');
        open_paren += 1;
    }
    if (*open_paren != ')' || value == 0)
        return (FT_FAILURE);
    *out_zero_based_index = value - 1;
    return (FT_SUCCESS);
}

static int cblc_parse_cobol_reference_base(const char *reference, char *base_name,
    size_t base_name_size)
{
    const char *open_paren;
    const char *open_bracket;
    size_t length;

    if (!reference || !base_name || base_name_size == 0)
        return (FT_FAILURE);
    open_paren = std::strchr(reference, '(');
    open_bracket = std::strchr(reference, '[');
    if (open_paren && (!open_bracket || open_paren < open_bracket))
        length = static_cast<size_t>(open_paren - reference);
    else if (open_bracket)
        length = static_cast<size_t>(open_bracket - reference);
    else
        length = std::strlen(reference);
    if (length + 1 > base_name_size)
        return (FT_FAILURE);
    if (length > 0)
        std::memcpy(base_name, reference, length);
    base_name[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_translate_expression_for_cobol(const char *expression, char *buffer,
    size_t buffer_size)
{
    size_t index;

    if (!expression || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    index = 0;
    while (expression[index] != '\0')
    {
        char character;

        character = expression[index];
        if (character == ' ')
        {
            index += 1;
            continue ;
        }
        if (character == '+' || character == '-' || character == '*' || character == '/')
        {
            char operator_buffer[2];

            operator_buffer[0] = character;
            operator_buffer[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        if ((character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z')
            || character == '_')
        {
            char token[TRANSPILE_STATEMENT_TEXT_MAX];
            char mapped[TRANSPILE_STATEMENT_TEXT_MAX];
            size_t token_index;
            size_t length;

            token_index = index;
            length = 0;
            while ((expression[token_index] >= 'A' && expression[token_index] <= 'Z')
                || (expression[token_index] >= 'a' && expression[token_index] <= 'z')
                || (expression[token_index] >= '0' && expression[token_index] <= '9')
                || expression[token_index] == '-' || expression[token_index] == '_')
            {
                if (length + 1 >= sizeof(token))
                    return (FT_FAILURE);
                token[length] = expression[token_index];
                length += 1;
                token_index += 1;
            }
            token[length] = '\0';
            if (expression[token_index] == '[')
            {
                const char *expr_start;
                size_t expr_length;
                char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_index[TRANSPILE_STATEMENT_TEXT_MAX];

                expr_start = &expression[token_index + 1];
                while (expression[token_index] != '\0' && expression[token_index] != ']')
                    token_index += 1;
                if (expression[token_index] != ']')
                    return (FT_FAILURE);
                expr_length = static_cast<size_t>(&expression[token_index] - expr_start);
                if (expr_length + 1 >= sizeof(index_expression))
                    return (FT_FAILURE);
                std::memcpy(index_expression, expr_start, expr_length);
                index_expression[expr_length] = '\0';
                if (cblc_translate_expression_for_cobol(index_expression, translated_index,
                        sizeof(translated_index)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(mapped, sizeof(mapped), "%s(%s + 1)", token,
                        translated_index) < 0)
                    return (FT_FAILURE);
                token_index += 1;
            }
            else if (expression[token_index] == '(')
            {
                size_t paren_depth;

                paren_depth = 0;
                while (expression[token_index] != '\0')
                {
                    if (length + 1 >= sizeof(token))
                        return (FT_FAILURE);
                    token[length] = expression[token_index];
                    length += 1;
                    if (expression[token_index] == '(')
                        paren_depth += 1;
                    else if (expression[token_index] == ')')
                    {
                        paren_depth -= 1;
                        if (paren_depth == 0)
                        {
                            token_index += 1;
                            break ;
                        }
                    }
                    token_index += 1;
                }
                if (paren_depth != 0)
                    return (FT_FAILURE);
                token[length] = '\0';
                ft_strlcpy(mapped, token, sizeof(mapped));
            }
            else
                ft_strlcpy(mapped, token, sizeof(mapped));
            if (cblc_expression_append(buffer, buffer_size, mapped) != FT_SUCCESS)
                return (FT_FAILURE);
            index = token_index;
            continue ;
        }
        if (character >= '0' && character <= '9')
        {
            char number_token[32];
            size_t length;

            length = 0;
            while (expression[index] >= '0' && expression[index] <= '9')
            {
                if (length + 1 >= sizeof(number_token))
                    return (FT_FAILURE);
                number_token[length] = expression[index];
                length += 1;
                index += 1;
            }
            number_token[length] = '\0';
            if (cblc_expression_append(buffer, buffer_size, number_token) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_data_reference_text(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference, char *out_reference,
    size_t out_reference_size)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char reference_text[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;
    size_t has_index;
    size_t array_index;
    int has_expression_index;
    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit || !out_item)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item)
        return (FT_FAILURE);
    ft_strlcpy(reference_text, item->cobol_name, sizeof(reference_text));
    has_index = 0;
    array_index = 0;
    has_expression_index = 0;
    index_expression[0] = '\0';
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '[')
        {
            if (has_index || item->array_count == 0)
                return (FT_FAILURE);
            {
                const char *index_start;

                index_start = *cursor;
                if (cblc_parse_array_literal_index(cursor, &array_index) == FT_SUCCESS)
                {
                    if (cblc_format_occurs_reference(item->cobol_name, array_index, reference_text,
                            sizeof(reference_text)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    has_index = 1;
                    has_expression_index = 0;
                }
                else
                {
                    *cursor = index_start;
                    if (**cursor != '[')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_array_variable_index(cursor, unit, index_expression,
                            sizeof(index_expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ']')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (cblc_format_expression_indexed_reference(item->cobol_name,
                            index_expression, reference_text, sizeof(reference_text))
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    has_index = 0;
                    has_expression_index = 1;
                }
            }
            continue ;
        }
        if (**cursor != '.')
            break ;
        {
            char member_identifier[TRANSPILE_IDENTIFIER_MAX];
            char field_source_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_struct_type *owner_type;
            const t_cblc_struct_field *field;

            *cursor += 1;
            if (cblc_parse_identifier(cursor, member_identifier,
                    sizeof(member_identifier)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (item->kind == CBLC_DATA_KIND_STRING
                && std::strncmp(member_identifier, "len", sizeof(member_identifier)) == 0)
            {
                if (out_len_reference)
                    *out_len_reference = 1;
                if (out_reference)
                {
                    if (has_expression_index)
                    {
                        if (cblc_format_string_component_expression_reference(item->cobol_name,
                                index_expression, "-LEN", out_reference, out_reference_size)
                            != FT_SUCCESS)
                            return (FT_FAILURE);
                    }
                    else if (cblc_format_string_component_reference(item->cobol_name, has_index,
                            array_index, "-LEN", out_reference, out_reference_size)
                            != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                *out_item = item;
                return (FT_SUCCESS);
            }
            if (item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            owner_type = cblc_find_receiver_type(unit, item);
            field = cblc_find_field_on_type(owner_type, member_identifier);
            if (!field || !cblc_member_visibility_is_allowed(owner_type, field->visibility))
                return (FT_FAILURE);
            cblc_build_field_source_name(item->source_name, member_identifier, field_source_name,
                sizeof(field_source_name));
            item = cblc_find_data_item(unit, field_source_name);
            if (!item)
                return (FT_FAILURE);
            ft_strlcpy(reference_text, item->cobol_name, sizeof(reference_text));
            has_index = 0;
            array_index = 0;
            has_expression_index = 0;
            index_expression[0] = '\0';
        }
    }
    if (item->array_count > 0 && !has_index && !has_expression_index)
        return (FT_FAILURE);
    if (out_len_reference)
        *out_len_reference = 0;
    if (out_reference)
        ft_strlcpy(out_reference, reference_text, out_reference_size);
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_parse_data_reference(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference)
{
    return (cblc_parse_data_reference_text(cursor, unit, out_item, out_len_reference,
            NULL, 0));
}

static int cblc_struct_type_append_field(t_cblc_struct_type *type, const char *identifier,
    size_t length, size_t array_count, t_cblc_data_kind kind, const char *struct_type_name,
    int is_const, t_cblc_member_visibility visibility)
{
    t_cblc_struct_field *field;

    if (!type || !identifier)
        return (FT_FAILURE);
    if (type->field_count >= type->field_capacity)
    {
        if (cblc_struct_type_ensure_field_capacity(type,
                type->field_capacity == 0 ? 4 : type->field_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    field = &type->fields[type->field_count];
    ft_strlcpy(field->source_name, identifier, sizeof(field->source_name));
    cblc_identifier_to_cobol(identifier, field->cobol_name, sizeof(field->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(field->declared_type_name, "string", sizeof(field->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(field->declared_type_name, struct_type_name, sizeof(field->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(field->struct_type_name, struct_type_name, sizeof(field->struct_type_name));
    else
        field->struct_type_name[0] = '\0';
    field->length = length;
    field->array_count = array_count;
    field->kind = kind;
    field->is_const = is_const;
    field->visibility = visibility;
    type->field_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_struct_field_declaration(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type, t_cblc_member_visibility visibility)
{
    const char *start;
    const t_cblc_struct_type *field_type;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
    size_t array_count;
    int is_const;
    const char *after_array;

    if (!cursor || !*cursor || !unit || !type)
        return (FT_FAILURE);
    is_const = 0;
    if (cblc_match_keyword(cursor, "const"))
    {
        is_const = 1;
        cblc_skip_whitespace(cursor);
    }
    if (cblc_match_keyword(cursor, "int"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        array_count = 0;
        if (**cursor == '[')
        {
            if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, 0, array_count,
                CBLC_DATA_KIND_INT, NULL,
                is_const, visibility));
    }
    if (cblc_match_keyword(cursor, "char"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        length = 1;
        if (**cursor == '[')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            if (cblc_parse_numeric_literal(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ']')
                return (FT_FAILURE);
            *cursor += 1;
            cblc_skip_whitespace(cursor);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, length, 0, CBLC_DATA_KIND_CHAR, NULL,
                is_const, visibility));
    }
    if (cblc_match_keyword(cursor, "string"))
    {
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        length = 1;
        array_count = 0;
        if (**cursor == '[')
        {
            after_array = *cursor;
            if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
                return (FT_FAILURE);
            if (**cursor == '(')
            {
                if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                *cursor = after_array;
                if (cblc_parse_array_count_clause(cursor, &length) != FT_SUCCESS)
                    return (FT_FAILURE);
                array_count = 0;
            }
        }
        else if (**cursor == '(')
        {
            if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, length, array_count,
                CBLC_DATA_KIND_STRING, NULL,
                is_const, visibility));
    }
    start = *cursor;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    field_type = cblc_find_struct_type(unit, type_identifier);
    if (!field_type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, "std::", std::strlen("std::")) == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    return (cblc_struct_type_append_field(type, identifier, 0, 0, CBLC_DATA_KIND_STRUCT,
        field_type->source_name, is_const, visibility));
}

static int cblc_parse_lifecycle_declaration(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type)
{
    const char *start;
    t_cblc_constructor *constructor;
    t_cblc_function constructor_function;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    int is_destructor;
    size_t constructor_parameter_count;
    t_cblc_parameter constructor_parameters[TRANSPILE_FUNCTION_PARAMETER_MAX];

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    is_destructor = 0;
    std::memset(&constructor_function, 0, sizeof(constructor_function));
    constructor_parameter_count = 0;
    std::memset(constructor_parameters, 0, sizeof(constructor_parameters));
    if (**cursor == '~')
    {
        is_destructor = 1;
        *cursor += 1;
    }
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, type->source_name, sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (!is_destructor)
    {
        if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__ctor",
                type->source_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-CTOR",
                type->cobol_name) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (type->constructor_count > 0)
        {
            cblc_build_constructor_scope_name(type->source_name, "__ctor_",
                type->constructor_count, scope_source_name,
                sizeof(scope_source_name));
            cblc_build_constructor_scope_name(type->cobol_name, "-CTOR-",
                type->constructor_count, scope_cobol_name,
                sizeof(scope_cobol_name));
        }
        if (cblc_parse_parameter_list(cursor, unit, constructor_parameters,
                &constructor_parameter_count, scope_source_name, scope_cobol_name,
                NULL, type) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        ft_strlcpy(constructor_function.source_name, scope_source_name,
            sizeof(constructor_function.source_name));
        ft_strlcpy(constructor_function.cobol_name, scope_cobol_name,
            sizeof(constructor_function.cobol_name));
        std::memcpy(constructor_function.parameters, constructor_parameters,
            sizeof(constructor_parameters));
        constructor_function.parameter_count = constructor_parameter_count;
    }
    else
        cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ';')
    {
        *cursor += 1;
        if (is_destructor)
            type->has_destructor = 1;
        else
        {
            if (cblc_constructor_signature_exists(type, constructor_parameters,
                    constructor_parameter_count))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            if (type->constructor_count >= type->constructor_capacity)
            {
                if (cblc_struct_type_ensure_constructor_capacity(type,
                        type->constructor_capacity == 0 ? 2 : type->constructor_capacity * 2)
                    != FT_SUCCESS)
                {
                    *cursor = start;
                    return (FT_FAILURE);
                }
            }
            constructor = &type->constructors[type->constructor_count];
            std::memset(constructor, 0, sizeof(*constructor));
            constructor->parameter_count = constructor_parameter_count;
            std::memcpy(constructor->parameters, constructor_parameters,
                sizeof(constructor->parameters));
            type->constructor_count += 1;
            if (constructor_parameter_count == 0)
                type->has_default_constructor = 1;
        }
        return (FT_SUCCESS);
    }
    if (**cursor == '{' || (!is_destructor && **cursor == ':'))
    {
        t_cblc_statement *statements;
        size_t statement_count;
        size_t statement_capacity;

        statements = NULL;
        statement_count = 0;
        statement_capacity = 0;
        if ((is_destructor
                ? cblc_capture_lifecycle_body(cursor, unit, type, &statements, &statement_count,
                    &statement_capacity)
                : cblc_capture_constructor_body(cursor, unit, type, &constructor_function,
                    &statements, &statement_count, &statement_capacity)) != FT_SUCCESS)
        {
            if (constructor_function.local_destructor_targets)
                cma_free(constructor_function.local_destructor_targets);
            *cursor = start;
            return (FT_FAILURE);
        }
        if (is_destructor)
        {
            if (type->destructor_statements)
                cma_free(type->destructor_statements);
            type->destructor_statements = statements;
            type->destructor_statement_count = statement_count;
            type->destructor_statement_capacity = statement_capacity;
            type->has_destructor = 1;
        }
        else
        {
            if (cblc_constructor_signature_exists(type, constructor_parameters,
                    constructor_parameter_count))
            {
                cma_free(statements);
                if (constructor_function.local_destructor_targets)
                    cma_free(constructor_function.local_destructor_targets);
                *cursor = start;
                return (FT_FAILURE);
            }
            if (type->constructor_count >= type->constructor_capacity)
            {
                if (cblc_struct_type_ensure_constructor_capacity(type,
                        type->constructor_capacity == 0 ? 2 : type->constructor_capacity * 2)
                    != FT_SUCCESS)
                {
                    cma_free(statements);
                    if (constructor_function.local_destructor_targets)
                        cma_free(constructor_function.local_destructor_targets);
                    *cursor = start;
                    return (FT_FAILURE);
                }
            }
            constructor = &type->constructors[type->constructor_count];
            std::memset(constructor, 0, sizeof(*constructor));
            constructor->statements = statements;
            constructor->statement_count = statement_count;
            constructor->statement_capacity = statement_capacity;
            constructor->parameter_count = constructor_parameter_count;
            std::memcpy(constructor->parameters, constructor_parameters,
                sizeof(constructor->parameters));
            type->constructor_count += 1;
            if (constructor_parameter_count == 0)
                type->has_default_constructor = 1;
        }
        if (constructor_function.local_destructor_targets)
            cma_free(constructor_function.local_destructor_targets);
        return (FT_SUCCESS);
    }
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_normalize_call_arguments(const t_cblc_translation_unit *unit,
    const char *call_arguments, size_t call_argument_count, char *buffer,
    size_t buffer_size)
{
    size_t index;
    t_cblc_statement statement;

    if (!unit || !call_arguments || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    std::memset(&statement, 0, sizeof(statement));
    ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
    statement.call_argument_count = call_argument_count;
    index = 0;
    while (index < call_argument_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        const char *resolved_argument;
        const t_cblc_data_item *item;

        if (cblc_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        resolved_argument = argument;
        if (argument[0] != '"')
        {
            item = cblc_find_data_item_const(unit, argument);
            if (item)
            {
                const t_cblc_data_item *actual_item;

                actual_item = cblc_find_data_item_by_cobol(unit, item->cobol_name);
                if (actual_item && !actual_item->is_alias)
                    resolved_argument = actual_item->source_name;
            }
        }
        if (index > 0)
        {
            ft_strlcat(buffer, ", ", buffer_size);
        }
        ft_strlcat(buffer, resolved_argument, buffer_size);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_capture_method_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *method_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !method_function
        || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_add_parameter_aliases(unit, method_function->parameters,
            method_function->parameter_count, type) != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        return (FT_FAILURE);
    }
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, method_function, 1);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS
        || (method_function->return_kind != CBLC_FUNCTION_RETURN_VOID
            && !method_function->saw_return))
    {
        return (FT_FAILURE);
    }
    *out_statements = method_function->statements;
    *out_count = method_function->statement_count;
    *out_capacity = method_function->statement_capacity;
    method_function->statements = NULL;
    method_function->statement_count = 0;
    method_function->statement_capacity = 0;
    return (FT_SUCCESS);
}

static int cblc_parse_method_definition(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type, t_cblc_member_visibility visibility)
{
    const char *start;
    t_cblc_method *method;
    t_cblc_function method_function;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char scope_source_name[TRANSPILE_IDENTIFIER_MAX];
    char scope_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function_return_kind return_kind;
    char return_type_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    return_type_name[0] = '\0';
    if (cblc_parse_function_return_type(cursor, unit, &return_kind,
            return_type_name, sizeof(return_type_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::strncmp(identifier, type->source_name, sizeof(identifier)) == 0
        || cblc_find_method_on_type(type, identifier))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    std::memset(&method_function, 0, sizeof(method_function));
    method_function.return_kind = return_kind;
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    else if (return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        ft_strlcpy(method_function.return_type_name, return_type_name,
            sizeof(method_function.return_type_name));
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    }
    if (std::snprintf(scope_source_name, sizeof(scope_source_name), "%s__%s",
            type->source_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(scope_cobol_name, sizeof(scope_cobol_name), "%s-%s",
            type->cobol_name, identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(method_function.source_name, scope_source_name,
        sizeof(method_function.source_name));
    ft_strlcpy(method_function.cobol_name, scope_cobol_name,
        sizeof(method_function.cobol_name));
    if (cblc_parse_parameter_list(cursor, unit, method_function.parameters,
            &method_function.parameter_count, scope_source_name, scope_cobol_name,
            NULL, type) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statements = NULL;
    statement_count = 0;
    statement_capacity = 0;
    if (cblc_capture_method_body(cursor, unit, type, &method_function, &statements,
            &statement_count, &statement_capacity) != FT_SUCCESS)
    {
        if (method_function.local_destructor_targets)
            cma_free(method_function.local_destructor_targets);
        *cursor = start;
        return (FT_FAILURE);
    }
    if (type->method_count >= type->method_capacity)
    {
        if (cblc_struct_type_ensure_method_capacity(type,
                type->method_capacity == 0 ? 2 : type->method_capacity * 2) != FT_SUCCESS)
        {
            cma_free(statements);
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, identifier, sizeof(method->source_name));
    cblc_identifier_to_cobol(identifier, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = return_kind;
    ft_strlcpy(method->return_type_name, return_type_name,
        sizeof(method->return_type_name));
    method->visibility = visibility;
    method->parameter_count = method_function.parameter_count;
    std::memcpy(method->parameters, method_function.parameters, sizeof(method->parameters));
    method->statements = statements;
    method->statement_count = statement_count;
    method->statement_capacity = statement_capacity;
    type->method_count += 1;
    if (method_function.local_destructor_targets)
        cma_free(method_function.local_destructor_targets);
    return (FT_SUCCESS);
}

static int cblc_parse_struct_definition(const char **cursor, t_cblc_translation_unit *unit)
{
    t_cblc_struct_type *type;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    int is_class;
    t_cblc_member_visibility current_visibility;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (cblc_match_keyword(cursor, "struct"))
        is_class = 0;
    else if (cblc_match_keyword(cursor, "class"))
        is_class = 1;
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_find_struct_type(unit, identifier))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    if (unit->struct_type_count >= unit->struct_type_capacity)
    {
        if (cblc_translation_unit_ensure_struct_capacity(unit,
                unit->struct_type_capacity == 0 ? 2 : unit->struct_type_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    type = &unit->struct_types[unit->struct_type_count];
    std::memset(type, 0, sizeof(*type));
    ft_strlcpy(type->source_name, identifier, sizeof(type->source_name));
    cblc_identifier_to_cobol(identifier, type->cobol_name, sizeof(type->cobol_name));
    type->is_class = is_class;
    if (is_class)
        current_visibility = CBLC_MEMBER_VISIBILITY_PRIVATE;
    else
        current_visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    *cursor += 1;
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
            break ;
        if (type->is_class
            && cblc_parse_member_visibility_label(cursor, &current_visibility) == FT_SUCCESS)
            continue ;
        if (cblc_parse_lifecycle_declaration(cursor, unit, type) == FT_SUCCESS)
            continue ;
        if (cblc_parse_method_definition(cursor, unit, type, current_visibility) == FT_SUCCESS)
            continue ;
        if (cblc_parse_struct_field_declaration(cursor, unit, type, current_visibility)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (**cursor != '}')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    unit->struct_type_count += 1;
    return (FT_SUCCESS);
}

static int cblc_add_struct_instance_field_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *instance_source_name, const char *instance_cobol_name,
    const char *owner_function_name, int is_function_local, int is_alias)
{
    size_t index;

    if (!unit || !type || !instance_source_name || !instance_cobol_name)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char field_source_name[TRANSPILE_IDENTIFIER_MAX];
        char field_cobol_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_item *item;

        cblc_build_field_source_name(instance_source_name, type->fields[index].source_name,
            field_source_name, sizeof(field_source_name));
        if (std::snprintf(field_cobol_name, sizeof(field_cobol_name), "%s-%s",
                instance_cobol_name, type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (unit->data_count >= unit->data_capacity)
        {
            if (cblc_translation_unit_ensure_data_capacity(unit,
                    unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        item = &unit->data_items[unit->data_count];
        std::memset(item, 0, sizeof(*item));
        ft_strlcpy(item->source_name, field_source_name, sizeof(item->source_name));
        ft_strlcpy(item->cobol_name, field_cobol_name, sizeof(item->cobol_name));
        if (owner_function_name)
            ft_strlcpy(item->owner_function_name, owner_function_name, sizeof(item->owner_function_name));
        if (type->fields[index].declared_type_name[0] != '\0')
            ft_strlcpy(item->declared_type_name, type->fields[index].declared_type_name,
                sizeof(item->declared_type_name));
        item->length = type->fields[index].length;
        item->array_count = type->fields[index].array_count;
        item->kind = type->fields[index].kind;
        item->is_const = type->fields[index].is_const;
        item->is_function_local = is_function_local;
        item->is_alias = is_alias;
        item->is_active = 1;
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            ft_strlcpy(item->struct_type_name, type->fields[index].struct_type_name,
                sizeof(item->struct_type_name));
            unit->data_count += 1;
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_struct_instance_field_items(unit, field_type, item->source_name,
                    item->cobol_name, owner_function_name, is_function_local, is_alias) != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
            continue ;
        }
        unit->data_count += 1;
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit)
{
    const char *start;
    const t_cblc_struct_type *type;
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_argument_count;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char instance_identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, instance_identifier, sizeof(instance_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '(')
    {
        *cursor += 1;
        if (cblc_parse_call_argument_list(cursor, unit, constructor_arguments,
                sizeof(constructor_arguments), &constructor_argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count > 0
        && !cblc_find_constructor_for_arguments(unit, type, constructor_arguments,
            constructor_argument_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count == 0
        && type->constructor_count > 0
        && !type->has_default_constructor)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_find_data_item(unit, instance_identifier))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, instance_identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(instance_identifier, item->cobol_name, sizeof(item->cobol_name));
    ft_strlcpy(item->declared_type_name, type->source_name, sizeof(item->declared_type_name));
    ft_strlcpy(item->struct_type_name, type->source_name, sizeof(item->struct_type_name));
    ft_strlcpy(item->constructor_arguments, constructor_arguments, sizeof(item->constructor_arguments));
    item->constructor_argument_count = constructor_argument_count;
    item->kind = CBLC_DATA_KIND_STRUCT;
    item->is_active = 1;
    unit->data_count += 1;
    if (cblc_add_struct_instance_field_items(unit, type, item->source_name, item->cobol_name,
            NULL, 0, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    return (FT_SUCCESS);
}

static t_cblc_data_item *cblc_add_generated_char_item(t_cblc_translation_unit *unit,
    const char *identifier, size_t length)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (NULL);
    item = cblc_find_data_item(unit, identifier);
    if (item)
        return (item);
    if (length == 0)
        length = 1;
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (NULL);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    item->owner_function_name[0] = '\0';
    item->length = length;
    item->kind = CBLC_DATA_KIND_CHAR;
    unit->data_count += 1;
    return (item);
}

static t_cblc_data_item *cblc_add_generated_int_item(t_cblc_translation_unit *unit,
    const char *identifier)
{
    t_cblc_data_item *item;

    if (!unit || !identifier)
        return (NULL);
    item = cblc_find_data_item(unit, identifier);
    if (item)
        return (item);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (NULL);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, identifier, sizeof(item->source_name));
    cblc_identifier_to_cobol(identifier, item->cobol_name, sizeof(item->cobol_name));
    item->owner_function_name[0] = '\0';
    item->length = 0;
    item->kind = CBLC_DATA_KIND_INT;
    unit->data_count += 1;
    return (item);
}

static t_cblc_data_item *cblc_create_return_item(t_cblc_translation_unit *unit,
    const char *function_identifier, t_cblc_function_return_kind return_kind,
    const char *return_type_name)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function_identifier)
        return (NULL);
    if (std::snprintf(identifier, sizeof(identifier), "cblc_return_%s",
            function_identifier) < 0)
        return (NULL);
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        return (cblc_add_generated_int_item(unit, identifier));
    if (return_kind != CBLC_FUNCTION_RETURN_STRUCT || !return_type_name)
        return (NULL);
    cblc_identifier_to_cobol(identifier, cobol_name, sizeof(cobol_name));
    if (cblc_add_named_data_item(unit, identifier, cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            return_type_name, NULL, 0, 0, 0) != FT_SUCCESS)
        return (NULL);
    return (&unit->data_items[unit->data_count - 1]);
}

static int cblc_parse_function_return_type(const char **cursor, const t_cblc_translation_unit *unit,
    t_cblc_function_return_kind *out_kind, char *type_name, size_t type_name_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !out_kind || !type_name || type_name_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    type_name[0] = '\0';
    if (cblc_match_keyword(cursor, "void"))
    {
        *out_kind = CBLC_FUNCTION_RETURN_VOID;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "int"))
    {
        *out_kind = CBLC_FUNCTION_RETURN_INT;
        return (FT_SUCCESS);
    }
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (!cblc_find_struct_type(unit, identifier))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *out_kind = CBLC_FUNCTION_RETURN_STRUCT;
    ft_strlcpy(type_name, identifier, type_name_size);
    return (FT_SUCCESS);
}

static int cblc_function_return_matches_target(const t_cblc_function *function,
    const t_cblc_data_item *target_item)
{
    if (!function || !target_item)
        return (0);
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
        return (target_item->kind == CBLC_DATA_KIND_INT);
    if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(target_item->struct_type_name, function->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    return (0);
}

static int cblc_parameter_kind_from_data_kind(t_cblc_data_kind kind,
    t_transpiler_function_parameter_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (kind == CBLC_DATA_KIND_INT)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_INT;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_STRING)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_STRING;
        return (FT_SUCCESS);
    }
    if (kind == CBLC_DATA_KIND_STRUCT)
    {
        *out_kind = TRANSPILE_FUNCTION_PARAMETER_STRUCT;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_parse_parameter_type(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_kind *out_kind, char *type_name, size_t type_name_size,
    const t_cblc_struct_type *self_type)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    const t_cblc_struct_type *type;
    const char *start;

    if (!cursor || !*cursor || !out_kind)
        return (FT_FAILURE);
    if (type_name && type_name_size > 0)
        type_name[0] = '\0';
    start = *cursor;
    if (cblc_match_keyword(cursor, "int"))
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    if (cblc_match_keyword(cursor, "string"))
    {
        *out_kind = CBLC_DATA_KIND_STRING;
        return (FT_SUCCESS);
    }
    if (!unit || !type_name || type_name_size == 0)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, identifier);
    if (!type)
    {
        if (!self_type
            || std::strncmp(identifier, self_type->source_name, sizeof(identifier)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    *out_kind = CBLC_DATA_KIND_STRUCT;
    ft_strlcpy(type_name, identifier, type_name_size);
    return (FT_SUCCESS);
}

static int cblc_data_kind_from_parameter_kind(t_transpiler_function_parameter_kind parameter_kind,
    t_cblc_data_kind *out_kind)
{
    if (!out_kind)
        return (FT_FAILURE);
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        *out_kind = CBLC_DATA_KIND_INT;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
    {
        *out_kind = CBLC_DATA_KIND_STRING;
        return (FT_SUCCESS);
    }
    if (parameter_kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        *out_kind = CBLC_DATA_KIND_STRUCT;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int cblc_add_parameter_binding(t_cblc_translation_unit *unit,
    t_cblc_parameter *parameters, size_t *parameter_count,
    const char *scope_source_name, const char *scope_cobol_name,
    const char *parameter_name, t_cblc_data_kind kind, const char *type_name,
    const char *owner_function_name, const t_cblc_struct_type *self_type)
{
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_function_parameter_kind parameter_kind;
    const t_cblc_struct_type *struct_type;

    if (!unit || !parameters || !parameter_count || !scope_source_name
        || !scope_cobol_name || !parameter_name)
        return (FT_FAILURE);
    if (*parameter_count >= TRANSPILE_FUNCTION_PARAMETER_MAX)
        return (FT_FAILURE);
    if (cblc_parameter_kind_from_data_kind(kind, &parameter_kind) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            scope_source_name, parameter_name) < 0)
        return (FT_FAILURE);
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            scope_cobol_name, parameter_name) < 0)
        return (FT_FAILURE);
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name,
        sizeof(actual_cobol_name));
    struct_type = NULL;
    if (kind == CBLC_DATA_KIND_STRUCT)
    {
        if (!type_name || type_name[0] == '\0')
            return (FT_FAILURE);
        if (self_type
            && std::strncmp(type_name, self_type->source_name, TRANSPILE_IDENTIFIER_MAX) == 0)
            struct_type = self_type;
        else
            struct_type = cblc_find_struct_type(unit, type_name);
        if (!struct_type)
            return (FT_FAILURE);
        if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                type_name, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_struct_instance_field_items(unit, struct_type, actual_source_name,
                actual_cobol_name, owner_function_name, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_local_struct_alias_items(unit, struct_type, parameter_name, actual_cobol_name)
            != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else
    {
        if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, kind, 0, 0,
                NULL, owner_function_name, 0, 0, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_named_data_item(unit, parameter_name, actual_cobol_name, kind, 0, 0,
                NULL, NULL, 0, 1, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(parameters[*parameter_count].source_name, parameter_name,
        sizeof(parameters[*parameter_count].source_name));
    ft_strlcpy(parameters[*parameter_count].actual_source_name,
        actual_source_name,
        sizeof(parameters[*parameter_count].actual_source_name));
    ft_strlcpy(parameters[*parameter_count].cobol_name,
        actual_cobol_name,
        sizeof(parameters[*parameter_count].cobol_name));
    if (type_name)
        ft_strlcpy(parameters[*parameter_count].type_name, type_name,
            sizeof(parameters[*parameter_count].type_name));
    parameters[*parameter_count].kind = parameter_kind;
    *parameter_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_parameter_list(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_parameter *parameters,
    size_t *parameter_count, const char *scope_source_name,
    const char *scope_cobol_name, const char *owner_function_name,
    const t_cblc_struct_type *self_type)
{
    t_cblc_data_kind kind;
    char parameter_name[TRANSPILE_IDENTIFIER_MAX];
    char parameter_type_name[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !parameters || !parameter_count
        || !scope_source_name || !scope_cobol_name)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
        return (FT_SUCCESS);
    while (1)
    {
        if (cblc_parse_parameter_type(cursor, unit, &kind, parameter_type_name,
                sizeof(parameter_type_name), self_type) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (cblc_parse_identifier(cursor, parameter_name, sizeof(parameter_name)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_add_parameter_binding(unit, parameters, parameter_count,
                scope_source_name, scope_cobol_name, parameter_name, kind,
                parameter_type_name,
                owner_function_name, self_type) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor == ',')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            continue ;
        }
        if (**cursor == ')')
            return (FT_SUCCESS);
        return (FT_FAILURE);
    }
}

static int cblc_parse_function_parameters(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    return (cblc_parse_parameter_list(cursor, unit, function->parameters,
            &function->parameter_count, function->source_name, function->cobol_name,
            function->source_name, NULL));
}

static int cblc_add_parameter_aliases(t_cblc_translation_unit *unit,
    const t_cblc_parameter *parameters, size_t parameter_count,
    const t_cblc_struct_type *self_type)
{
    size_t index;

    if (!unit || !parameters)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        t_cblc_data_kind kind;

        if (cblc_data_kind_from_parameter_kind(parameters[index].kind, &kind) != FT_SUCCESS)
            return (FT_FAILURE);
        if (kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *type;

            if (self_type
                && std::strncmp(parameters[index].type_name, self_type->source_name,
                    sizeof(parameters[index].type_name)) == 0)
                type = self_type;
            else
                type = cblc_find_struct_type(unit, parameters[index].type_name);
            if (!type)
                return (FT_FAILURE);
            if (cblc_add_local_struct_alias_items(unit, type, parameters[index].source_name,
                    parameters[index].cobol_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cblc_add_named_data_item(unit, parameters[index].source_name,
                    parameters[index].cobol_name, kind, 0, 0, NULL, NULL, 0, 1, 0)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_build_constructor_scope_name(const char *base, const char *separator,
    size_t arity, char *buffer, size_t buffer_size)
{
    char arity_suffix[32];

    if (!buffer || buffer_size == 0)
        return ;
    std::snprintf(arity_suffix, sizeof(arity_suffix), "%zu", arity);
    cblc_build_string_component_name(base, separator, buffer, buffer_size);
    ft_strlcat(buffer, arity_suffix, buffer_size);
}

static int cblc_parse_call_argument_list(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size,
    size_t *out_count)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t count;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0 || !out_count)
        return (FT_FAILURE);
    buffer[0] = '\0';
    count = 0;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
    {
        *out_count = 0;
        return (FT_SUCCESS);
    }
    while (1)
    {
        {
            const char *argument_start;
            const char *argument_end;
            t_cblc_data_item *argument_item;
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
            int is_length_reference;

            argument_start = *cursor;
            if (**cursor == '"')
            {
                if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                expression[0] = '"';
                expression[1] = '\0';
                ft_strlcat(expression, literal_buffer, sizeof(expression));
                ft_strlcat(expression, "\"", sizeof(expression));
            }
            else if (cblc_parse_data_reference_text(cursor, unit, &argument_item, &is_length_reference,
                    expression, sizeof(expression)) == FT_SUCCESS)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor != ')' && **cursor != ',')
                {
                    *cursor = argument_start;
                    if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                            sizeof(expression)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    size_t argument_length;

                    argument_end = *cursor;
                    while (argument_end > argument_start
                        && std::isspace(static_cast<unsigned char>(argument_end[-1])))
                        argument_end -= 1;
                    argument_length = static_cast<size_t>(argument_end - argument_start);
                    if (argument_length + 1 > sizeof(expression))
                        return (FT_FAILURE);
                    std::memcpy(expression, argument_start, argument_length);
                    expression[argument_length] = '\0';
                }
            }
            else
            {
                *cursor = argument_start;
                if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                        sizeof(expression)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        if (count > 0)
            ft_strlcat(buffer, ",", buffer_size);
        ft_strlcat(buffer, expression, buffer_size);
        count += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor == ',')
        {
            *cursor += 1;
            cblc_skip_whitespace(cursor);
            continue ;
        }
        if (**cursor == ')')
        {
            *out_count = count;
            return (FT_SUCCESS);
        }
        return (FT_FAILURE);
    }
}

static t_cblc_data_item *cblc_ensure_helper_status(t_cblc_translation_unit *unit)
{
    t_cblc_data_item *item;

    if (!unit)
        return (NULL);
    if (unit->helper_status_index >= 0
        && static_cast<size_t>(unit->helper_status_index) < unit->data_count)
        return (&unit->data_items[unit->helper_status_index]);
    item = cblc_add_generated_int_item(unit, "cblc_helper_status");
    if (!item)
        return (NULL);
    unit->helper_status_index = static_cast<int>(item - unit->data_items);
    return (item);
}

static t_cblc_data_item *cblc_create_literal_buffer(t_cblc_translation_unit *unit,
    size_t literal_length)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit)
        return (NULL);
    unit->helper_literal_counter += 1;
    index = unit->helper_literal_counter;
    if (std::snprintf(identifier, sizeof(identifier), "cblc_literal_%zu", index) < 0)
        return (NULL);
    return (cblc_add_generated_char_item(unit, identifier,
        literal_length > 0 ? literal_length : 1));
}

static const t_cblc_function *cblc_find_function_in_unit(const t_cblc_translation_unit *unit,
    const char *identifier)
{
    size_t index;

    if (!unit || !identifier)
        return (NULL);
    index = 0;
    while (index < unit->function_count)
    {
        if (std::strncmp(unit->functions[index].source_name, identifier,
                sizeof(unit->functions[index].source_name)) == 0)
            return (&unit->functions[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_append_statement(t_cblc_function *function, t_cblc_statement_type type,
    const char *target, const char *source, int is_literal)
{
    t_cblc_statement *statement;

    if (!function)
        return (FT_FAILURE);
    if (function->statement_count >= function->statement_capacity)
    {
        if (cblc_function_ensure_statement_capacity(function,
                function->statement_capacity == 0 ? 4 : function->statement_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count];
    statement->type = type;
    statement->is_literal = is_literal;
    statement->target[0] = '\0';
    statement->source[0] = '\0';
    statement->call_identifier[0] = '\0';
    statement->call_is_external = 0;
    statement->call_arguments[0] = '\0';
    statement->call_argument_count = 0;
    if (target)
        ft_strlcpy(statement->target, target, sizeof(statement->target));
    if (source)
        ft_strlcpy(statement->source, source, sizeof(statement->source));
    function->statement_count += 1;
    return (FT_SUCCESS);
}

static int cblc_append_existing_statement(t_cblc_function *function,
    const t_cblc_statement *statement)
{
    t_cblc_statement *appended;

    if (!function || !statement)
        return (FT_FAILURE);
    if (cblc_append_statement(function, statement->type, statement->target,
            statement->source, statement->is_literal) != FT_SUCCESS)
        return (FT_FAILURE);
    appended = &function->statements[function->statement_count - 1];
    ft_strlcpy(appended->call_identifier, statement->call_identifier,
        sizeof(appended->call_identifier));
    appended->call_is_external = statement->call_is_external;
    ft_strlcpy(appended->call_arguments, statement->call_arguments,
        sizeof(appended->call_arguments));
    appended->call_argument_count = statement->call_argument_count;
    return (FT_SUCCESS);
}

static int cblc_append_call_statement(t_cblc_function *function, const char *identifier,
    const char *arguments)
{
    t_cblc_statement *statement;

    if (!function || !identifier)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL, NULL, NULL, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (function->statement_count == 0)
        return (FT_FAILURE);
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier, sizeof(statement->call_identifier));
    statement->call_is_external = 1;
    if (arguments)
        ft_strlcpy(statement->call_arguments, arguments, sizeof(statement->call_arguments));
    return (FT_SUCCESS);
}

static int cblc_add_temp_alias_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, int is_const)
{
    t_cblc_data_item *item;

    if (!unit || !source_name || !cobol_name)
        return (FT_FAILURE);
    if (cblc_find_data_item(unit, source_name))
        return (FT_SUCCESS);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, source_name, sizeof(item->source_name));
    ft_strlcpy(item->cobol_name, cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(item->declared_type_name, "string", sizeof(item->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(item->declared_type_name, struct_type_name, sizeof(item->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(item->struct_type_name, struct_type_name, sizeof(item->struct_type_name));
    item->length = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_const = is_const;
    item->is_active = 1;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, size_t array_count,
    const char *struct_type_name, const char *owner_function_name, int is_function_local,
    int is_alias, int is_const)
{
    t_cblc_data_item *item;

    if (!unit || !source_name || !cobol_name)
        return (FT_FAILURE);
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    std::memset(item, 0, sizeof(*item));
    ft_strlcpy(item->source_name, source_name, sizeof(item->source_name));
    ft_strlcpy(item->cobol_name, cobol_name, sizeof(item->cobol_name));
    if (kind == CBLC_DATA_KIND_STRING)
        ft_strlcpy(item->declared_type_name, "string", sizeof(item->declared_type_name));
    else if (struct_type_name)
        ft_strlcpy(item->declared_type_name, struct_type_name, sizeof(item->declared_type_name));
    if (struct_type_name)
        ft_strlcpy(item->struct_type_name, struct_type_name, sizeof(item->struct_type_name));
    if (owner_function_name)
        ft_strlcpy(item->owner_function_name, owner_function_name, sizeof(item->owner_function_name));
    item->length = length;
    item->array_count = array_count;
    item->kind = kind;
    item->is_const = is_const;
    item->is_function_local = is_function_local;
    item->is_alias = is_alias;
    item->is_active = 1;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    if (!unit || !item)
        return (NULL);
    if (item->declared_type_name[0] != '\0')
    {
        if (g_cblc_member_access_type
            && std::strncmp(item->declared_type_name, g_cblc_member_access_type->source_name,
                sizeof(item->declared_type_name)) == 0)
            return (g_cblc_member_access_type);
        return (cblc_find_struct_type(unit, item->declared_type_name));
    }
    if (item->kind == CBLC_DATA_KIND_STRUCT)
    {
        if (g_cblc_member_access_type
            && std::strncmp(item->struct_type_name, g_cblc_member_access_type->source_name,
                sizeof(item->struct_type_name)) == 0)
            return (g_cblc_member_access_type);
        return (cblc_find_struct_type(unit, item->struct_type_name));
    }
    return (NULL);
}

static int cblc_parse_method_receiver(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_receiver, char *method_name, size_t method_name_size)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !out_receiver || !method_name || method_name_size == 0)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    while (**cursor == '.')
    {
        const char *member_start;
        char member_identifier[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_struct_type *receiver_type;
        const t_cblc_struct_field *field;
        const t_cblc_method *method;
        char field_source_name[TRANSPILE_IDENTIFIER_MAX];
        t_cblc_data_item *next_item;
        const char *after_member;

        *cursor += 1;
        member_start = *cursor;
        if (cblc_parse_identifier(cursor, member_identifier, sizeof(member_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        after_member = *cursor;
        receiver_type = cblc_find_receiver_type(unit, item);
        method = cblc_find_method_on_type(receiver_type, member_identifier);
        if (receiver_type && method
            && cblc_member_visibility_is_allowed(receiver_type, method->visibility))
        {
            const char *probe;

            probe = after_member;
            cblc_skip_whitespace(&probe);
            if (*probe == '(')
            {
                ft_strlcpy(method_name, member_identifier, method_name_size);
                *out_receiver = item;
                return (FT_SUCCESS);
            }
        }
        if (item->kind == CBLC_DATA_KIND_STRING
            && std::strncmp(member_identifier, "len", sizeof(member_identifier)) == 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (item->kind != CBLC_DATA_KIND_STRUCT)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        field = cblc_find_field_on_type(receiver_type, member_identifier);
        if (!field || !cblc_member_visibility_is_allowed(receiver_type, field->visibility))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_build_field_source_name(item->source_name, member_identifier, field_source_name,
            sizeof(field_source_name));
        next_item = cblc_find_data_item(unit, field_source_name);
        if (!next_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        item = next_item;
        (void)member_start;
    }
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_bind_lifecycle_scope(t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    size_t *saved_data_count)
{
    const char *root_source;
    const char *root_cobol;
    size_t alias_start;
    size_t alias_end;
    size_t index;

    if (!unit || !type || !saved_data_count)
        return (FT_FAILURE);
    *saved_data_count = unit->data_count;
    root_source = "this";
    root_cobol = "CBLC-THIS";
    if (cblc_add_temp_alias_item(unit, root_source, root_cobol, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_add_struct_instance_field_items(unit, type, root_source, root_cobol,
            NULL, 0, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    alias_start = *saved_data_count + 1;
    alias_end = unit->data_count;
    index = alias_start;
    while (index < alias_end)
    {
        char source_name[TRANSPILE_IDENTIFIER_MAX];
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char struct_type_name[TRANSPILE_IDENTIFIER_MAX];
        const char *declared_struct_type;
        t_cblc_data_kind kind;
        size_t length;
        size_t array_count;
        int is_const;

        ft_strlcpy(source_name, unit->data_items[index].source_name, sizeof(source_name));
        ft_strlcpy(cobol_name, unit->data_items[index].cobol_name, sizeof(cobol_name));
        ft_strlcpy(struct_type_name, unit->data_items[index].struct_type_name,
            sizeof(struct_type_name));
        kind = unit->data_items[index].kind;
        length = unit->data_items[index].length;
        array_count = unit->data_items[index].array_count;
        is_const = unit->data_items[index].is_const;
        if (std::strncmp(source_name, "this.", 5) == 0)
        {
            declared_struct_type = NULL;
            if (struct_type_name[0] != '\0')
                declared_struct_type = struct_type_name;
            if (cblc_add_temp_alias_item(unit, source_name + 5, cobol_name,
                    kind, length, array_count, declared_struct_type, is_const) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static void cblc_unbind_lifecycle_scope(t_cblc_translation_unit *unit, size_t saved_data_count)
{
    if (!unit)
        return ;
    if (saved_data_count <= unit->data_count)
        unit->data_count = saved_data_count;
}

static int cblc_parse_statement_block(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, int allow_return)
{
    size_t scope_destructor_base;
    size_t scope_data_base;
    int saw_return_in_block;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    scope_destructor_base = function->local_destructor_count;
    scope_data_base = unit->data_count;
    saw_return_in_block = 0;
    *cursor += 1;
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
        {
            if (!saw_return_in_block)
            {
                while (function->local_destructor_count > scope_destructor_base)
                {
                    function->local_destructor_count -= 1;
                    if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                            function->local_destructor_targets[function->local_destructor_count],
                            NULL, 0) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
                function->local_destructor_count = scope_destructor_base;
            while (scope_data_base < unit->data_count)
            {
                if (unit->data_items[scope_data_base].is_alias)
                    unit->data_items[scope_data_base].is_active = 0;
                scope_data_base += 1;
            }
            *cursor += 1;
            return (FT_SUCCESS);
        }
        if (**cursor == '{')
        {
            if (cblc_parse_statement_block(cursor, unit, function, allow_return) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(cursor, "return"))
        {
            (*cursor) -= std::strlen("return");
            if (!allow_return)
                return (FT_FAILURE);
            if (cblc_parse_return(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            saw_return_in_block = 1;
            continue ;
        }
        if (cblc_match_keyword(cursor, "display"))
        {
            (*cursor) -= std::strlen("display");
            if (cblc_parse_display(cursor, unit, function) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_local_string_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_local_struct_instance_declaration(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_std_strcpy(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_method_call(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_call(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_assignment(cursor, unit, function) == FT_SUCCESS)
            continue ;
        return (FT_FAILURE);
    }
    return (FT_FAILURE);
}

static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity)
{
    t_cblc_function lifecycle_function;
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    std::memset(&lifecycle_function, 0, sizeof(lifecycle_function));
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
        return (FT_FAILURE);
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, &lifecycle_function, 0);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS)
    {
        if (lifecycle_function.statements)
            cma_free(lifecycle_function.statements);
        return (FT_FAILURE);
    }
    *out_statements = lifecycle_function.statements;
    *out_count = lifecycle_function.statement_count;
    *out_capacity = lifecycle_function.statement_capacity;
    return (FT_SUCCESS);
}

static int cblc_find_field_index(const t_cblc_struct_type *type, const char *identifier,
    size_t *out_index)
{
    size_t index;

    if (!type || !identifier || !out_index)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (std::strncmp(type->fields[index].source_name, identifier,
                sizeof(type->fields[index].source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_find_current_constructor_field_index(const t_cblc_data_item *item,
    size_t *out_index)
{
    const t_cblc_struct_type *type;
    size_t index;
    char expected_cobol[TRANSPILE_IDENTIFIER_MAX];
    char expected_source[TRANSPILE_IDENTIFIER_MAX];

    if (!item || !out_index || !g_cblc_constructor_parse_state.active
        || !g_cblc_constructor_parse_state.type)
        return (FT_FAILURE);
    type = g_cblc_constructor_parse_state.type;
    index = 0;
    while (index < type->field_count)
    {
        if (std::snprintf(expected_cobol, sizeof(expected_cobol), "CBLC-THIS-%s",
                type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->cobol_name, expected_cobol, sizeof(item->cobol_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::strncmp(item->source_name, type->fields[index].source_name,
                sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        if (std::snprintf(expected_source, sizeof(expected_source), "this.%s",
                type->fields[index].source_name) < 0)
            return (FT_FAILURE);
        if (std::strncmp(item->source_name, expected_source, sizeof(item->source_name)) == 0)
        {
            *out_index = index;
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_append_string_initializer_literal(t_cblc_function *function,
    const t_cblc_data_item *item, const char *literal_text)
{
    char buffer_target[TRANSPILE_IDENTIFIER_MAX];
    char length_target[TRANSPILE_IDENTIFIER_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char length_literal[32];
    size_t assigned_length;

    if (!function || !item || !literal_text)
        return (FT_FAILURE);
    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_target,
        sizeof(buffer_target));
    cblc_build_string_component_name(item->cobol_name, "-LEN", length_target,
        sizeof(length_target));
    assigned_length = std::strlen(literal_text);
    if (assigned_length > item->length)
        assigned_length = item->length;
    cobol_source[0] = '"';
    cobol_source[1] = '\0';
    ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
    ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
    if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
            cobol_source, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
            length_literal, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_constructor_member_initializer(const char **cursor,
    t_cblc_translation_unit *unit, const t_cblc_struct_type *type, int *seen_fields,
    t_cblc_function *field_functions)
{
    char member_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;
    size_t field_index;

    if (!cursor || !*cursor || !unit || !type || !seen_fields || !field_functions)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, member_name, sizeof(member_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_find_field_index(type, member_name, &field_index) != FT_SUCCESS)
        return (FT_FAILURE);
    if (seen_fields[field_index])
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, member_name);
    if (!item)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == ')')
    {
        if (item->kind == CBLC_DATA_KIND_INT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                    item->cobol_name, "0", 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (item->kind == CBLC_DATA_KIND_STRING || item->kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                    item->cobol_name, NULL, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_INT)
    {
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_parse_numeric_expression_until_paren(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_COMPUTE,
                item->cobol_name, expression, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (item->kind == CBLC_DATA_KIND_STRING)
    {
        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[2];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            if (cblc_append_string_initializer_literal(&field_functions[field_index], item,
                    literal_text) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else if (item->kind == CBLC_DATA_KIND_CHAR)
    {
        char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];

        if (**cursor == '"')
        {
            char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = '"';
            literal_text[1] = literal_value;
            literal_text[2] = '"';
            literal_text[3] = '\0';
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, literal_text, 1) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            t_cblc_data_item *source_item;

            if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
                return (FT_FAILURE);
            if (cblc_append_statement(&field_functions[field_index], CBLC_STATEMENT_ASSIGNMENT,
                    item->cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    seen_fields[field_index] = 1;
    return (FT_SUCCESS);
}

static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function *constructor_function,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    t_cblc_function *field_functions;
    int *seen_fields;
    size_t saved_data_count;
    size_t index;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !constructor_function
        || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    field_functions = static_cast<t_cblc_function *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*field_functions)));
    seen_fields = static_cast<int *>(cma_calloc(type->field_count == 0 ? 1 : type->field_count,
            sizeof(*seen_fields)));
    if (!field_functions || !seen_fields)
    {
        if (field_functions)
            cma_free(field_functions);
        if (seen_fields)
            cma_free(seen_fields);
        return (FT_FAILURE);
    }
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
    {
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    if (cblc_add_parameter_aliases(unit, constructor_function->parameters,
            constructor_function->parameter_count, type) != FT_SUCCESS)
    {
        cblc_unbind_lifecycle_scope(unit, saved_data_count);
        cma_free(field_functions);
        cma_free(seen_fields);
        return (FT_FAILURE);
    }
    g_cblc_constructor_parse_state.type = type;
    g_cblc_constructor_parse_state.initialized_fields = seen_fields;
    g_cblc_constructor_parse_state.active = 1;
    g_cblc_member_access_type = type;
    cblc_skip_whitespace(cursor);
    if (**cursor == ':')
    {
        *cursor += 1;
        while (**cursor != '\0')
        {
            cblc_skip_whitespace(cursor);
            if (cblc_parse_constructor_member_initializer(cursor, unit, type, seen_fields,
                    field_functions) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            cblc_skip_whitespace(cursor);
            if (**cursor == ',')
            {
                *cursor += 1;
                continue ;
            }
            break ;
        }
    }
    index = 0;
    while (index < type->field_count)
    {
        t_cblc_data_item *field_item;
        size_t statement_index;

        field_item = cblc_find_data_item(unit, type->fields[index].source_name);
        if (!field_item)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        if (!seen_fields[index])
        {
            if (field_item->kind == CBLC_DATA_KIND_STRING || field_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_append_statement(constructor_function, CBLC_STATEMENT_DEFAULT_CONSTRUCT,
                        field_item->cobol_name, NULL, 0) != FT_SUCCESS)
                {
                    status = FT_FAILURE;
                    goto cleanup;
                }
            }
        }
        statement_index = 0;
        while (statement_index < field_functions[index].statement_count)
        {
            if (cblc_append_existing_statement(constructor_function,
                    &field_functions[index].statements[statement_index]) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            statement_index += 1;
        }
        index += 1;
    }
    status = cblc_parse_statement_block(cursor, unit, constructor_function, 0);
    if (status != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].is_const && !seen_fields[index])
        {
            status = FT_FAILURE;
            goto cleanup;
        }
        index += 1;
    }
    *out_statements = constructor_function->statements;
    *out_count = constructor_function->statement_count;
    *out_capacity = constructor_function->statement_capacity;
    constructor_function->statements = NULL;
    constructor_function->statement_count = 0;
    constructor_function->statement_capacity = 0;
    status = FT_SUCCESS;
cleanup:
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = saved_access_type;
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    index = 0;
    while (index < type->field_count)
    {
        if (field_functions[index].statements)
            cma_free(field_functions[index].statements);
        index += 1;
    }
    cma_free(field_functions);
    cma_free(seen_fields);
    return (status);
}

static int cblc_inject_entry_lifecycle(t_cblc_translation_unit *unit)
{
    t_cblc_function *function;
    t_cblc_statement *new_statements;
    char (*construct_targets)[TRANSPILE_IDENTIFIER_MAX];
    char (*construct_arguments)[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t *construct_argument_counts;
    char (*destruct_targets)[TRANSPILE_IDENTIFIER_MAX];
    size_t construct_count;
    size_t destruct_count;
    size_t return_count;
    size_t new_count;
    size_t index;
    size_t write_index;

    if (!unit || unit->function_count == 0)
        return (FT_SUCCESS);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (FT_SUCCESS);
    function = &unit->functions[unit->entry_function_index];
    index = unit->data_count;
    if (index == 0)
        index = 1;
    construct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*construct_targets)));
    construct_arguments = static_cast<char (*)[TRANSPILE_STATEMENT_TEXT_MAX]>(cma_calloc(index,
            sizeof(*construct_arguments)));
    construct_argument_counts = static_cast<size_t *>(cma_calloc(index,
            sizeof(*construct_argument_counts)));
    destruct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_targets)));
    if (!construct_targets || !construct_arguments || !construct_argument_counts || !destruct_targets)
    {
        if (construct_targets)
            cma_free(construct_targets);
        if (construct_arguments)
            cma_free(construct_arguments);
        if (construct_argument_counts)
            cma_free(construct_argument_counts);
        if (destruct_targets)
            cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    construct_count = 0;
    destruct_count = 0;
    index = 0;
    while (index < unit->data_count)
    {
        const t_cblc_data_item *item;
        const t_cblc_struct_type *type;

        item = &unit->data_items[index];
        if (item->is_alias || item->is_function_local || std::strchr(item->source_name, '.'))
        {
            index += 1;
            continue ;
        }
        type = cblc_find_declared_type_for_item(unit, item);
        if (item->kind == CBLC_DATA_KIND_STRUCT && !type)
            type = cblc_find_struct_type(unit, item->struct_type_name);
        if ((item->declared_type_name[0] != '\0' || item->kind == CBLC_DATA_KIND_STRUCT) && !type)
        {
            cma_free(construct_targets);
            cma_free(construct_arguments);
            cma_free(construct_argument_counts);
            cma_free(destruct_targets);
            return (FT_FAILURE);
        }
        if (cblc_item_requires_default_construct(unit, item))
        {
            ft_strlcpy(construct_targets[construct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            ft_strlcpy(construct_arguments[construct_count], item->constructor_arguments,
                TRANSPILE_STATEMENT_TEXT_MAX);
            construct_argument_counts[construct_count] = item->constructor_argument_count;
            construct_count += 1;
        }
        if (cblc_item_requires_destruct(unit, item))
        {
            ft_strlcpy(destruct_targets[destruct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
            destruct_count += 1;
        }
        index += 1;
    }
    return_count = 0;
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
            return_count += 1;
        index += 1;
    }
    new_count = function->statement_count + construct_count + (destruct_count * return_count);
    if (return_count == 0)
        new_count += destruct_count;
    if (new_count == 0)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_SUCCESS);
    }
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(new_count, sizeof(*new_statements)));
    if (!new_statements)
    {
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    write_index = 0;
    index = 0;
    while (index < construct_count)
    {
        new_statements[write_index].type = CBLC_STATEMENT_DEFAULT_CONSTRUCT;
        ft_strlcpy(new_statements[write_index].target, construct_targets[index],
            sizeof(new_statements[write_index].target));
        ft_strlcpy(new_statements[write_index].call_arguments, construct_arguments[index],
            sizeof(new_statements[write_index].call_arguments));
        new_statements[write_index].call_argument_count = construct_argument_counts[index];
        write_index += 1;
        index += 1;
    }
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
        {
            size_t destruct_index;

            destruct_index = destruct_count;
            while (destruct_index > 0)
            {
                destruct_index -= 1;
                new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
                ft_strlcpy(new_statements[write_index].target, destruct_targets[destruct_index],
                    sizeof(new_statements[write_index].target));
                write_index += 1;
            }
        }
        new_statements[write_index] = function->statements[index];
        write_index += 1;
        index += 1;
    }
    if (return_count == 0)
    {
        index = destruct_count;
        while (index > 0)
        {
            index -= 1;
            new_statements[write_index].type = CBLC_STATEMENT_DESTRUCT;
            ft_strlcpy(new_statements[write_index].target, destruct_targets[index],
                sizeof(new_statements[write_index].target));
            write_index += 1;
        }
    }
    if (write_index != new_count)
    {
        cma_free(new_statements);
        cma_free(construct_targets);
        cma_free(construct_arguments);
        cma_free(construct_argument_counts);
        cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    if (function->statements)
        cma_free(function->statements);
    function->statements = new_statements;
    function->statement_count = new_count;
    function->statement_capacity = new_count;
    cma_free(construct_targets);
    cma_free(construct_arguments);
    cma_free(construct_argument_counts);
    cma_free(destruct_targets);
    return (FT_SUCCESS);
}

static int cblc_append_local_destructor_statements(t_cblc_function *function)
{
    size_t index;

    if (!function)
        return (FT_FAILURE);
    index = function->local_destructor_count;
    while (index > 0)
    {
        index -= 1;
        if (cblc_append_statement(function, CBLC_STATEMENT_DESTRUCT,
                function->local_destructor_targets[index], NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_add_local_struct_alias_items(t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *alias_source_name, const char *actual_cobol_name)
{
    size_t index;

    if (!unit || !type || !alias_source_name || !actual_cobol_name)
        return (FT_FAILURE);
    if (cblc_add_named_data_item(unit, alias_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, NULL, 0, 1, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        char alias_field_source[TRANSPILE_IDENTIFIER_MAX];
        char actual_field_cobol[TRANSPILE_IDENTIFIER_MAX];

        cblc_build_field_source_name(alias_source_name, type->fields[index].source_name,
            alias_field_source, sizeof(alias_field_source));
        if (std::snprintf(actual_field_cobol, sizeof(actual_field_cobol), "%s-%s",
                actual_cobol_name, type->fields[index].cobol_name) < 0)
            return (FT_FAILURE);
        if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_add_local_struct_alias_items(unit, field_type, alias_field_source,
                    actual_field_cobol) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (cblc_add_named_data_item(unit, alias_field_source, actual_field_cobol,
                type->fields[index].kind, type->fields[index].length,
                type->fields[index].array_count,
                type->fields[index].struct_type_name[0] != '\0'
                    ? type->fields[index].struct_type_name : NULL,
                NULL, 0, 1, type->fields[index].is_const) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_local_string_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t array_count;
    size_t constructor_argument_count;
    const char *after_array;
    const char *constructor_start;
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (!cblc_match_keyword(cursor, "string"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    length = 1;
    array_count = 0;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (**cursor == '[')
    {
        after_array = *cursor;
        if (cblc_parse_array_count_clause(cursor, &array_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (**cursor == '(')
        {
            if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
        else
        {
            *cursor = after_array;
            if (cblc_parse_array_count_clause(cursor, &length) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            array_count = 0;
        }
    }
    else if (**cursor == '(')
    {
        constructor_start = *cursor;
        if (cblc_parse_string_capacity_clause(cursor, &length) != FT_SUCCESS)
        {
            *cursor = constructor_start;
            if (cblc_parse_string_constructor_clause(cursor, unit, &length,
                    constructor_arguments, sizeof(constructor_arguments),
                    &constructor_argument_count) != FT_SUCCESS)
            {
                *cursor = start;
                return (FT_FAILURE);
            }
        }
    }
    if (array_count > 0 || (array_count > 0 && constructor_argument_count > 0))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name, sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, CBLC_DATA_KIND_STRING,
            length, 0, NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count - 1];
    ft_strlcpy(item->constructor_arguments, constructor_arguments,
        sizeof(item->constructor_arguments));
    item->constructor_argument_count = constructor_argument_count;
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, CBLC_DATA_KIND_STRING,
            length, 0, NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
            NULL, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_strlcpy(function->statements[function->statement_count - 1].call_arguments,
        constructor_arguments, sizeof(function->statements[function->statement_count - 1].call_arguments));
    function->statements[function->statement_count - 1].call_argument_count = constructor_argument_count;
    if (function->local_destructor_count >= function->local_destructor_capacity)
    {
        if (cblc_function_ensure_local_destructor_capacity(function,
                function->local_destructor_capacity == 0 ? 4
                : function->local_destructor_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    ft_strlcpy(function->local_destructor_targets[function->local_destructor_count],
        actual_cobol_name, TRANSPILE_IDENTIFIER_MAX);
    function->local_destructor_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_local_struct_instance_declaration(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    const t_cblc_struct_type *type;
    char constructor_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t constructor_argument_count;
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    constructor_arguments[0] = '\0';
    constructor_argument_count = 0;
    if (cblc_parse_identifier(cursor, type_identifier, sizeof(type_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, type_identifier);
    if (!type)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, local_identifier, sizeof(local_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor == '(')
    {
        *cursor += 1;
        if (cblc_parse_call_argument_list(cursor, unit, constructor_arguments,
                sizeof(constructor_arguments), &constructor_argument_count) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count > 0
        && !cblc_find_constructor_for_arguments(unit, type, constructor_arguments,
            constructor_argument_count))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (constructor_argument_count == 0
        && type->constructor_count > 0
        && !type->has_default_constructor)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_source_name, sizeof(actual_source_name), "%s__%s",
            function->source_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(actual_cobol_source, sizeof(actual_cobol_source), "%s-%s",
            function->cobol_name, local_identifier) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_identifier_to_cobol(actual_cobol_source, actual_cobol_name, sizeof(actual_cobol_name));
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0, 0,
            type->source_name, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_struct_instance_field_items(unit, type, actual_source_name, actual_cobol_name,
            function->source_name, 1, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_local_struct_alias_items(unit, type, local_identifier, actual_cobol_name)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_type_requires_default_construct(unit, type) || constructor_argument_count > 0)
    {
        if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
                NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
        ft_strlcpy(function->statements[function->statement_count - 1].call_arguments,
            constructor_arguments, sizeof(function->statements[function->statement_count - 1].call_arguments));
        function->statements[function->statement_count - 1].call_argument_count = constructor_argument_count;
    }
    if (cblc_type_requires_destruct(unit, type))
    {
        if (function->local_destructor_count >= function->local_destructor_capacity)
        {
            if (cblc_function_ensure_local_destructor_capacity(function,
                    function->local_destructor_capacity == 0 ? 4
                    : function->local_destructor_capacity * 2) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        ft_strlcpy(function->local_destructor_targets[function->local_destructor_count],
            actual_cobol_name, TRANSPILE_IDENTIFIER_MAX);
        function->local_destructor_count += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_parse_std_strcpy(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char destination_identifier[TRANSPILE_IDENTIFIER_MAX];
    char source_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
    char destination_length_literal[32];
    char source_length_literal[32];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_source_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *target_item;
    t_cblc_data_item *source_item;
    t_cblc_data_item *status_item;
    size_t source_length;
    size_t target_index;
    size_t source_index;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::strcpy", sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, destination_identifier,
            sizeof(destination_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_item = cblc_find_data_item(unit, destination_identifier);
    if (!target_item || target_item->kind != CBLC_DATA_KIND_CHAR
        || target_item->is_const)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_index = static_cast<size_t>(target_item - unit->data_items);
    cblc_skip_whitespace(cursor);
    if (**cursor != ',')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    source_item = NULL;
    literal_text[0] = '\0';
    source_length = 0;
    source_index = 0;
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
            != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_length = std::strlen(literal_buffer);
        literal_text[0] = '\"';
        literal_text[1] = '\0';
        ft_strlcat(literal_text, literal_buffer, sizeof(literal_text));
        ft_strlcat(literal_text, "\"", sizeof(literal_text));
        source_item = cblc_create_literal_buffer(unit, source_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, literal_text, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_index = static_cast<size_t>(source_item - unit->data_items);
        target_item = &unit->data_items[target_index];
    }
    else if (**cursor == '\'')
    {
        char literal_value;
        char literal_token[4];

        if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        literal_token[0] = literal_value;
        literal_token[1] = '\0';
        literal_text[0] = '\"';
        literal_text[1] = '\0';
        if (literal_value == '"')
            ft_strlcat(literal_text, "\"\"", sizeof(literal_text));
        else
            ft_strlcat(literal_text, literal_token, sizeof(literal_text));
        ft_strlcat(literal_text, "\"", sizeof(literal_text));
        source_length = 1;
        source_item = cblc_create_literal_buffer(unit, source_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, literal_text, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_index = static_cast<size_t>(source_item - unit->data_items);
        target_item = &unit->data_items[target_index];
    }
    else
    {
        if (cblc_parse_identifier(cursor, source_identifier,
                sizeof(source_identifier)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_item = cblc_find_data_item(unit, source_identifier);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        source_length = source_item->length;
        source_index = static_cast<size_t>(source_item - unit->data_items);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (!source_item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(destination_length_literal, sizeof(destination_length_literal),
            "%zu", target_item->length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(source_length_literal, sizeof(source_length_literal), "%zu",
            source_length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    status_item = cblc_ensure_helper_status(unit);
    if (!status_item)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    target_item = &unit->data_items[target_index];
    source_item = &unit->data_items[source_index];
    ft_strlcpy(cobol_source_name, source_item->cobol_name,
        sizeof(cobol_source_name));
    if (std::snprintf(call_arguments, sizeof(call_arguments),
            "BY REFERENCE %s BY VALUE %s BY REFERENCE %s BY VALUE %s BY REFERENCE %s",
            target_item->cobol_name, destination_length_literal, cobol_source_name,
            source_length_literal, status_item->cobol_name) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_call_statement(function, "CBLC-STRCPY", call_arguments) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_expression_append(char *buffer, size_t buffer_size, const char *token)
{
    size_t length;
    size_t token_length;

    if (!buffer || !token)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length > 0)
    {
        if (length + 1 >= buffer_size)
            return (FT_FAILURE);
        buffer[length] = ' ';
        length += 1;
        buffer[length] = '\0';
    }
    token_length = std::strlen(token);
    if (length + token_length >= buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer + length, token, token_length);
    buffer[length + token_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_parse_numeric_expression(const char **cursor, t_cblc_translation_unit *unit,
    char *buffer, size_t buffer_size)
{
    int expect_operand;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    expect_operand = 1;
    while (**cursor != '\0' && **cursor != ';')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '\0' || **cursor == ';')
            break ;
        if (expect_operand)
        {
            if (std::strncmp(*cursor, "std::abs", std::strlen("std::abs")) == 0)
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(identifier, "std::abs", sizeof(identifier)) == 0)
                {
                    char function_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
                    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
                    t_cblc_data_item *argument_item;

                    cblc_skip_whitespace(cursor);
                    if (**cursor != '(')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_identifier(cursor, argument_identifier,
                            sizeof(argument_identifier)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    argument_item = cblc_find_data_item(unit, argument_identifier);
                    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_INT)
                        return (FT_FAILURE);
                    ft_strlcpy(cobol_argument, argument_item->cobol_name,
                        sizeof(cobol_argument));
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (std::snprintf(function_buffer, sizeof(function_buffer),
                            "FUNCTION ABS(%s)", cobol_argument) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, function_buffer) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                return (FT_FAILURE);
            }
            if (**cursor >= '0' && **cursor <= '9')
            {
                char number_token[32];
                size_t length;

                length = 0;
                while ((*cursor)[length] >= '0' && (*cursor)[length] <= '9'
                    && length + 1 < sizeof(number_token))
                {
                    number_token[length] = (*cursor)[length];
                    length += 1;
                }
                if (length == 0)
                    return (FT_FAILURE);
                number_token[length] = '\0';
                if (cblc_expression_append(buffer, buffer_size, number_token) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += length;
                expect_operand = 0;
                continue ;
            }
            if (**cursor == '+' || **cursor == '-')
            {
                char operator_buffer[2];

                operator_buffer[0] = **cursor;
                operator_buffer[1] = '\0';
                if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += 1;
                continue ;
            }
            if ((**cursor >= 'a' && **cursor <= 'z') || (**cursor >= 'A' && **cursor <= 'Z')
                || **cursor == '_')
            {
                t_cblc_data_item *item;
                int is_length_reference;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                        reference, sizeof(reference))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            return (FT_FAILURE);
        }
        if (**cursor == '+' || **cursor == '-' || **cursor == '*' || **cursor == '/')
        {
            char operator_buffer[2];

            operator_buffer[0] = **cursor;
            operator_buffer[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            *cursor += 1;
            expect_operand = 1;
            continue ;
        }
        return (FT_FAILURE);
    }
    if (expect_operand)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_numeric_expression_until_paren(const char **cursor,
    t_cblc_translation_unit *unit, char *buffer, size_t buffer_size)
{
    int expect_operand;

    if (!cursor || !*cursor || !unit || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    expect_operand = 1;
    while (**cursor != '\0' && **cursor != ')' && **cursor != ',')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '\0' || **cursor == ')' || **cursor == ',')
            break ;
        if (expect_operand)
        {
            if (std::strncmp(*cursor, "std::abs", std::strlen("std::abs")) == 0)
            {
                char identifier[TRANSPILE_IDENTIFIER_MAX];

                if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(identifier, "std::abs", sizeof(identifier)) == 0)
                {
                    char function_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
                    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
                    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
                    t_cblc_data_item *argument_item;

                    cblc_skip_whitespace(cursor);
                    if (**cursor != '(')
                        return (FT_FAILURE);
                    *cursor += 1;
                    cblc_skip_whitespace(cursor);
                    if (cblc_parse_identifier(cursor, argument_identifier,
                            sizeof(argument_identifier)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    argument_item = cblc_find_data_item(unit, argument_identifier);
                    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_INT)
                        return (FT_FAILURE);
                    ft_strlcpy(cobol_argument, argument_item->cobol_name,
                        sizeof(cobol_argument));
                    cblc_skip_whitespace(cursor);
                    if (**cursor != ')')
                        return (FT_FAILURE);
                    *cursor += 1;
                    if (std::snprintf(function_buffer, sizeof(function_buffer),
                            "FUNCTION ABS(%s)", cobol_argument) < 0)
                        return (FT_FAILURE);
                    if (cblc_expression_append(buffer, buffer_size, function_buffer) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                return (FT_FAILURE);
            }
            if (**cursor >= '0' && **cursor <= '9')
            {
                char number_token[32];
                size_t length;

                length = 0;
                while ((*cursor)[length] >= '0' && (*cursor)[length] <= '9'
                    && length + 1 < sizeof(number_token))
                {
                    number_token[length] = (*cursor)[length];
                    length += 1;
                }
                if (length == 0)
                    return (FT_FAILURE);
                number_token[length] = '\0';
                if (cblc_expression_append(buffer, buffer_size, number_token) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += length;
                expect_operand = 0;
                continue ;
            }
            if (**cursor == '+' || **cursor == '-')
            {
                char operator_buffer[2];

                operator_buffer[0] = **cursor;
                operator_buffer[1] = '\0';
                if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                    return (FT_FAILURE);
                *cursor += 1;
                continue ;
            }
            if ((**cursor >= 'a' && **cursor <= 'z') || (**cursor >= 'A' && **cursor <= 'Z')
                || **cursor == '_')
            {
                t_cblc_data_item *item;
                int is_length_reference;
                char reference[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                        reference, sizeof(reference))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, reference) != FT_SUCCESS)
                    return (FT_FAILURE);
                expect_operand = 0;
                continue ;
            }
            return (FT_FAILURE);
        }
        if (**cursor == '+' || **cursor == '-' || **cursor == '*' || **cursor == '/')
        {
            char operator_buffer[2];

            operator_buffer[0] = **cursor;
            operator_buffer[1] = '\0';
            if (cblc_expression_append(buffer, buffer_size, operator_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            *cursor += 1;
            expect_operand = 1;
            continue ;
        }
        return (FT_FAILURE);
    }
    if (expect_operand)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_parse_function_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    const t_cblc_function *called_function;
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    called_function = cblc_find_function_in_unit(unit, identifier);
    if (called_function && !cblc_function_return_matches_target(called_function, target_item))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (called_function)
    {
        if (called_function->return_cobol_name[0] == '\0')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (called_function->parameter_count != argument_count)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
            target_item->cobol_name,
            called_function ? called_function->return_cobol_name : NULL, 0)
        != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier,
        sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    statement->call_is_external = called_function ? 0 : 1;
    *cursor += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_method_call_assignment(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function,
    t_cblc_data_item *target_item)
{
    const char *start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    call_arguments[0] = '\0';
    normalized_call_arguments[0] = '\0';
    argument_count = 0;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    method = cblc_find_method_on_type(receiver_type, method_name);
    if (!method)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
    {
        if (target_item->kind != CBLC_DATA_KIND_INT)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (target_item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(target_item->struct_type_name, method->return_type_name,
                sizeof(target_item->struct_type_name)) != 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
    }
    else
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->parameter_count != argument_count)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        size_t index;
        t_cblc_statement probe_statement;

        std::memset(&probe_statement, 0, sizeof(probe_statement));
        ft_strlcpy(probe_statement.call_arguments, call_arguments,
            sizeof(probe_statement.call_arguments));
        probe_statement.call_argument_count = argument_count;
        index = 0;
        while (index < argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(&probe_statement, index, argument,
                    sizeof(argument)) != FT_SUCCESS
                || !cblc_argument_matches_parameter(unit, argument, &method->parameters[index]))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            index += 1;
        }
    }
    if (cblc_normalize_call_arguments(unit, call_arguments, argument_count,
            normalized_call_arguments, sizeof(normalized_call_arguments)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL_ASSIGN,
            target_item->cobol_name, receiver_item->cobol_name, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, normalized_call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_builtin_string_append_call(const char **cursor,
    t_cblc_translation_unit *unit, t_cblc_function *function)
{
    const char *start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    t_cblc_data_item *argument_item;
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    char receiver_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char source_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_literal[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement *statement;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    if (!receiver_type || !receiver_type->is_builtin
        || std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) != 0
        || std::strncmp(method_name, "append", sizeof(method_name)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    ft_strlcpy(receiver_cobol_name, receiver_item->cobol_name, sizeof(receiver_cobol_name));
    if (**cursor == '"')
    {
        t_cblc_data_item *source_item;
        size_t literal_length;

        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        literal_length = std::strlen(literal_buffer);
        cobol_literal[0] = '"';
        cobol_literal[1] = '\0';
        ft_strlcat(cobol_literal, literal_buffer, sizeof(cobol_literal));
        ft_strlcat(cobol_literal, "\"", sizeof(cobol_literal));
        source_item = cblc_create_literal_buffer(unit, literal_length);
        if (!source_item)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                source_item->cobol_name, cobol_literal, 1) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ')')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        if (!cblc_ensure_helper_status(unit))
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
                receiver_cobol_name, source_item->cobol_name, 0) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        statement = &function->statements[function->statement_count - 1];
        ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
        return (FT_SUCCESS);
    }
    if (cblc_parse_data_reference(cursor, unit, &argument_item, NULL) != FT_SUCCESS
        || !argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    ft_strlcpy(source_cobol_name, argument_item->cobol_name, sizeof(source_cobol_name));
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (!cblc_ensure_helper_status(unit))
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
            receiver_cobol_name, source_cobol_name, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    return (FT_SUCCESS);
}

static int cblc_parse_std_strlen_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function, t_cblc_data_item *target_item)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char argument_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char declared_length_literal[32];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *argument_item;
    size_t literal_length;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::strlen", sizeof(identifier)) != 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
            != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ')')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        *cursor += 1;
        literal_length = std::strlen(literal_buffer);
        if (std::snprintf(declared_length_literal, sizeof(declared_length_literal),
                "%zu", literal_length) < 0)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                target_item->cobol_name, declared_length_literal, 1));
    }
    if (cblc_parse_identifier(cursor, argument_identifier,
            sizeof(argument_identifier)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    argument_item = cblc_find_data_item(unit, argument_identifier);
    if (!argument_item || argument_item->kind != CBLC_DATA_KIND_CHAR)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (std::snprintf(declared_length_literal, sizeof(declared_length_literal), "%zu",
            argument_item->length) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (std::snprintf(call_arguments, sizeof(call_arguments),
            "BY REFERENCE %s BY VALUE %s BY REFERENCE %s", argument_item->cobol_name,
            declared_length_literal, target_item->cobol_name) < 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_call_statement(function, "CBLC-STRLEN", call_arguments) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char expression_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
    char cobol_source[TRANSPILE_STATEMENT_TEXT_MAX];
    char target_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    char source_reference[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *target_item;
    int target_is_length_reference;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    if (cblc_parse_data_reference_text(cursor, unit, &target_item, &target_is_length_reference,
            target_reference, sizeof(target_reference))
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (target_is_length_reference)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '=')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (target_item->is_const)
    {
        size_t field_index;

        if (cblc_find_current_constructor_field_index(target_item, &field_index) != FT_SUCCESS
            || !g_cblc_constructor_parse_state.initialized_fields)
            return (FT_FAILURE);
        if (g_cblc_constructor_parse_state.initialized_fields[field_index])
            return (FT_FAILURE);
        g_cblc_constructor_parse_state.initialized_fields[field_index] = 1;
    }
    if (target_item->kind == CBLC_DATA_KIND_STRING)
    {
        t_cblc_data_item *source_item;
        char buffer_target[TRANSPILE_IDENTIFIER_MAX];
        char length_target[TRANSPILE_IDENTIFIER_MAX];
        char length_literal[32];
        size_t assigned_length;
        size_t target_index;

        if (std::strncmp(target_reference, target_item->cobol_name,
                sizeof(target_item->cobol_name)) == 0)
        {
            cblc_build_string_component_name(target_item->cobol_name, "-BUF", buffer_target,
                sizeof(buffer_target));
            cblc_build_string_component_name(target_item->cobol_name, "-LEN", length_target,
                sizeof(length_target));
        }
        else
        {
            if (std::strchr(target_reference, '['))
            {
                char base_name[TRANSPILE_IDENTIFIER_MAX];
                char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_parse_cobol_reference_base(target_reference, base_name, sizeof(base_name))
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::strncmp(base_name, target_item->cobol_name,
                        sizeof(target_item->cobol_name)) != 0)
                    return (FT_FAILURE);
                {
                    const char *open_bracket;
                    size_t index_length;

                    open_bracket = std::strchr(target_reference, '[');
                    if (!open_bracket)
                        return (FT_FAILURE);
                    open_bracket += 1;
                    index_length = std::strcspn(open_bracket, "]");
                    if (index_length + 1 >= sizeof(index_expression) || open_bracket[index_length] != ']')
                        return (FT_FAILURE);
                    std::memcpy(index_expression, open_bracket, index_length);
                    index_expression[index_length] = '\0';
                }
                if (cblc_format_string_component_expression_reference(target_item->cobol_name,
                        index_expression, "-BUF", buffer_target, sizeof(buffer_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_expression_reference(target_item->cobol_name,
                        index_expression, "-LEN", length_target, sizeof(length_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (cblc_parse_occurs_reference_index(target_reference, &target_index) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_reference(target_item->cobol_name, 1, target_index,
                        "-BUF", buffer_target, sizeof(buffer_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_format_string_component_reference(target_item->cobol_name, 1, target_index,
                        "-LEN", length_target, sizeof(length_target)) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        if (**cursor == '"')
        {
            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer))
                != FT_SUCCESS)
                return (FT_FAILURE);
            assigned_length = std::strlen(literal_buffer);
            if (assigned_length > target_item->length)
                assigned_length = target_item->length;
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
                    cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
                    length_literal, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            cobol_source[0] = '"';
            cobol_source[1] = '\0';
            if (literal_value == '"')
                ft_strlcat(cobol_source, "\"\"", sizeof(cobol_source));
            else
                ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            assigned_length = target_item->length > 0 ? 1 : 0;
            if (std::snprintf(length_literal, sizeof(length_literal), "%zu", assigned_length) < 0)
                return (FT_FAILURE);
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, buffer_target,
                    cobol_source, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT, length_target,
                    length_literal, 1) != FT_SUCCESS)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    if (target_item->kind == CBLC_DATA_KIND_CHAR)
    {
        t_cblc_data_item *source_item;

        if (**cursor == '"')
        {
            if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
                return (FT_FAILURE);
            cobol_source[0] = '\"';
            cobol_source[1] = '\0';
            ft_strlcat(cobol_source, literal_buffer, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, cobol_source, 1));
        }
        if (**cursor == '\'')
        {
            char literal_value;
            char literal_text[4];

            if (cblc_parse_char_literal(cursor, &literal_value) != FT_SUCCESS)
                return (FT_FAILURE);
            literal_text[0] = literal_value;
            literal_text[1] = '\0';
            cobol_source[0] = '\"';
            cobol_source[1] = '\0';
            if (literal_value == '"')
                ft_strlcat(cobol_source, "\"\"", sizeof(cobol_source));
            else
                ft_strlcat(cobol_source, literal_text, sizeof(cobol_source));
            ft_strlcat(cobol_source, "\"", sizeof(cobol_source));
            cblc_skip_whitespace(cursor);
            if (**cursor != ';')
                return (FT_FAILURE);
            *cursor += 1;
            return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                    target_reference, cobol_source, 1));
        }
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    if (target_item->kind == CBLC_DATA_KIND_INT)
    {
        const char *call_start;
        const char *reference_start;
        t_cblc_data_item *source_item;
        int source_is_length_reference;

        call_start = *cursor;
        if (cblc_parse_method_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_std_strlen_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_function_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        reference_start = *cursor;
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, &source_is_length_reference,
                source_reference, sizeof(source_reference)) == FT_SUCCESS)
        {
            if (!source_is_length_reference && source_item && source_item->kind == CBLC_DATA_KIND_INT)
            {
                cblc_skip_whitespace(cursor);
                if (**cursor == ';')
                {
                    *cursor += 1;
                    return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                            target_reference, source_reference, 0));
                }
            }
        }
        *cursor = reference_start;
        if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                sizeof(expression_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                target_reference, expression_buffer, 0));
    }
    if (target_item->kind == CBLC_DATA_KIND_STRUCT)
    {
        t_cblc_data_item *source_item;
        const char *call_start;

        call_start = *cursor;
        if (cblc_parse_method_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_function_call_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        *cursor = call_start;
        if (cblc_parse_data_reference_text(cursor, unit, &source_item, NULL, source_reference,
                sizeof(source_reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(source_item->struct_type_name, target_item->struct_type_name,
                sizeof(source_item->struct_type_name)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_reference, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_reference, cobol_source, 0));
    }
    return (FT_FAILURE);
}

static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char cobol_argument[TRANSPILE_STATEMENT_TEXT_MAX];
    int is_literal;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "display"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor == '"')
    {
        if (cblc_parse_string_literal(cursor, literal_buffer, sizeof(literal_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cobol_argument[0] = '\"';
        cobol_argument[1] = '\0';
        ft_strlcat(cobol_argument, literal_buffer, sizeof(cobol_argument));
        ft_strlcat(cobol_argument, "\"", sizeof(cobol_argument));
        is_literal = 1;
    }
    else
    {
        t_cblc_data_item *item;
        int is_length_reference;
        char reference[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_parse_data_reference_text(cursor, unit, &item, &is_length_reference,
                reference, sizeof(reference)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (is_length_reference)
        {
            ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            is_literal = 0;
        }
        else
        {
            if (item->kind == CBLC_DATA_KIND_STRING)
                ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            else
                ft_strlcpy(cobol_argument, reference, sizeof(cobol_argument));
            is_literal = 0;
        }
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_append_statement(function, CBLC_STATEMENT_DISPLAY, NULL, cobol_argument, is_literal));
}

static int cblc_parse_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strncmp(identifier, "std::", std::strlen("std::")) == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL, NULL, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (function->statement_count == 0)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, identifier, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    char call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    char normalized_call_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;
    size_t argument_count;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
    call_arguments[0] = '\0';
    normalized_call_arguments[0] = '\0';
    argument_count = 0;
    if (cblc_parse_builtin_string_append_call(cursor, unit, function) == FT_SUCCESS)
        return (FT_SUCCESS);
    *cursor = start;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    method = cblc_find_method_on_type(receiver_type, method_name);
    if (!method || method->return_kind != CBLC_FUNCTION_RETURN_VOID)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_parse_call_argument_list(cursor, unit, call_arguments,
            sizeof(call_arguments), &argument_count) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (method->parameter_count != argument_count)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    {
        size_t index;
        t_cblc_statement probe_statement;

        std::memset(&probe_statement, 0, sizeof(probe_statement));
        ft_strlcpy(probe_statement.call_arguments, call_arguments,
            sizeof(probe_statement.call_arguments));
        probe_statement.call_argument_count = argument_count;
        index = 0;
        while (index < argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(&probe_statement, index, argument,
                    sizeof(argument)) != FT_SUCCESS
                || !cblc_argument_matches_parameter(unit, argument, &method->parameters[index]))
            {
                *cursor = start;
                return (FT_FAILURE);
            }
            index += 1;
        }
    }
    if (cblc_normalize_call_arguments(unit, call_arguments, argument_count,
            normalized_call_arguments, sizeof(normalized_call_arguments)) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
            receiver_item->cobol_name, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    ft_strlcpy(statement->call_arguments, normalized_call_arguments,
        sizeof(statement->call_arguments));
    statement->call_argument_count = argument_count;
    return (FT_SUCCESS);
}

static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "return"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (function->return_kind == CBLC_FUNCTION_RETURN_VOID)
    {
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        function->saw_return = 1;
        if (cblc_append_local_destructor_statements(function) != FT_SUCCESS)
            return (FT_FAILURE);
        return (cblc_append_statement(function, CBLC_STATEMENT_RETURN, NULL, NULL, 0));
    }
    if (function->return_kind == CBLC_FUNCTION_RETURN_INT)
    {
        if (cblc_parse_numeric_expression(cursor, unit, expression,
                sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    else if (function->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
    {
        if (cblc_parse_identifier(cursor, expression, sizeof(expression)) != FT_SUCCESS)
            return (FT_FAILURE);
        item = cblc_find_data_item(unit, expression);
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT
            || std::strncmp(item->struct_type_name, function->return_type_name,
                sizeof(item->struct_type_name)) != 0)
            return (FT_FAILURE);
        ft_strlcpy(expression, item->cobol_name, sizeof(expression));
    }
    else
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    function->saw_return = 1;
    if (function->return_cobol_name[0] == '\0')
        return (FT_FAILURE);
    if (cblc_append_local_destructor_statements(function) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_append_statement(function, CBLC_STATEMENT_RETURN,
            function->return_cobol_name, expression, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
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
    if (return_kind == CBLC_FUNCTION_RETURN_STRUCT)
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
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    const char *lookahead;

    if (!unit || !cursor)
        return (0);
    lookahead = cursor;
    if (cblc_match_keyword(&lookahead, "void") || cblc_match_keyword(&lookahead, "int"))
    {
    }
    else
    {
        if (cblc_parse_identifier(&lookahead, type_identifier, sizeof(type_identifier))
            != FT_SUCCESS)
            return (0);
        if (!cblc_find_struct_type(unit, type_identifier))
            return (0);
    }
    cblc_skip_whitespace(&lookahead);
    if (cblc_parse_identifier(&lookahead, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (0);
    cblc_skip_whitespace(&lookahead);
    if (*lookahead != '(')
        return (0);
    lookahead += 1;
    cblc_skip_whitespace(&lookahead);
    if (*lookahead == ')')
        return (1);
    if (!cblc_match_keyword(&lookahead, "int"))
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
            if (cblc_parse_char_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "int"))
        {
            cursor -= std::strlen("int");
            if (cblc_parse_int_declaration(&cursor, unit, 0) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
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

static int cblc_emit_lifecycle_recursive(const t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, const char *prefix, t_cobol_text_builder *builder)
{
    char line[256];
    char nested_prefix[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !type || !prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-%s-LEN.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-%s-BUF.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-%s.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-%s.",
                    prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            if (std::snprintf(nested_prefix, sizeof(nested_prefix), "%s-%s", prefix,
                    type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_emit_lifecycle_recursive(unit, field_type, nested_prefix, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder);
static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder);
static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder);
static int cblc_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size);

static int cblc_replace_this_prefix(const char *input, const char *replacement, char *output,
    size_t output_size)
{
    const char *needle;
    size_t needle_length;
    size_t replacement_length;
    size_t out_length;

    if (!input || !replacement || !output || output_size == 0)
        return (FT_FAILURE);
    needle = "CBLC-THIS";
    needle_length = std::strlen(needle);
    replacement_length = std::strlen(replacement);
    out_length = 0;
    while (*input != '\0')
    {
        if (std::strncmp(input, needle, needle_length) == 0)
        {
            if (out_length + replacement_length >= output_size)
                return (FT_FAILURE);
            std::memcpy(output + out_length, replacement, replacement_length);
            out_length += replacement_length;
            input += needle_length;
            continue ;
        }
        if (out_length + 1 >= output_size)
            return (FT_FAILURE);
        output[out_length] = *input;
        out_length += 1;
        input += 1;
    }
    output[out_length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_emit_substituted_statement(const t_cblc_translation_unit *unit,
    const t_cblc_function *function, const t_cblc_statement *statement, const char *replacement,
    t_cobol_text_builder *builder)
{
    t_cblc_statement substituted;

    auto append_statement_line = [&](char *line) -> int
    {
        size_t length;

        length = std::strlen(line);
        if (length == 0 || line[length - 1] != '.')
        {
            if (length + 1 >= 256)
                return (FT_FAILURE);
            line[length] = '.';
            line[length + 1] = '\0';
        }
        return (cobol_text_builder_append_line(builder, line));
    };

    if (!unit || !statement || !replacement || !builder)
        return (FT_FAILURE);
    substituted = *statement;
    if (cblc_replace_this_prefix(statement->target, replacement, substituted.target,
            sizeof(substituted.target)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_replace_this_prefix(statement->source, replacement, substituted.source,
            sizeof(substituted.source)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (substituted.type == CBLC_STATEMENT_ASSIGNMENT)
    {
        char line[256];
        char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

        if (substituted.is_literal)
            ft_strlcpy(source_text, substituted.source, sizeof(source_text));
        else if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(substituted.target, target_text,
                sizeof(target_text)) != FT_SUCCESS)
            return (FT_FAILURE);

        if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                source_text, target_text) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_DISPLAY)
    {
        char line[256];
        char base_name[TRANSPILE_IDENTIFIER_MAX];
        const t_cblc_data_item *item;

        if (substituted.is_literal)
        {
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s", substituted.source) < 0)
                return (FT_FAILURE);
            return (append_statement_line(line));
        }
        if (cblc_parse_cobol_reference_base(substituted.source, base_name, sizeof(base_name))
            == FT_SUCCESS)
        {
            item = cblc_find_data_item_by_cobol(unit, base_name);
            if (item && item->kind == CBLC_DATA_KIND_STRING)
            {
                char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                char length_name[TRANSPILE_IDENTIFIER_MAX];
                char translated_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_length_name[TRANSPILE_STATEMENT_TEXT_MAX];

                if (std::strncmp(substituted.source, item->cobol_name,
                        sizeof(item->cobol_name)) == 0)
                {
                    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name,
                        sizeof(buffer_name));
                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                }
                else
                {
                    size_t zero_based_index;

                    if (cblc_parse_occurs_reference_index(substituted.source, &zero_based_index)
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (std::strchr(substituted.source, '['))
                {
                    if (cblc_translate_expression_for_cobol(buffer_name, translated_buffer_name,
                            sizeof(translated_buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_translate_expression_for_cobol(length_name, translated_length_name,
                            sizeof(translated_length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    ft_strlcpy(translated_buffer_name, buffer_name, sizeof(translated_buffer_name));
                    ft_strlcpy(translated_length_name, length_name, sizeof(translated_length_name));
                }
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s(1:%s)",
                        translated_buffer_name, translated_length_name) < 0)
                    return (FT_FAILURE);
                return (append_statement_line(line));
            }
        }
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s", source_text) < 0)
                return (FT_FAILURE);
        }
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_COMPUTE)
    {
        char line[256];
        char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
        char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_translate_expression_for_cobol(substituted.source, source_text,
                sizeof(source_text)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_translate_expression_for_cobol(substituted.target, target_text,
                sizeof(target_text)) != FT_SUCCESS)
            return (FT_FAILURE);

        if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                target_text, source_text) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];
        const t_cblc_function *target_function;

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
        if (substituted.call_is_external)
        {
            if (std::strncmp(substituted.call_identifier, "CBLC-", 5) == 0
                && substituted.call_arguments[0] != '\0')
            {
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, substituted.call_arguments) < 0)
                    return (FT_FAILURE);
            }
            else if (substituted.call_arguments[0] != '\0')
            {
                char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_build_external_call_arguments(unit, &substituted,
                        using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, using_arguments) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           CALL '%s'", cobol_name) < 0)
                return (FT_FAILURE);
        }
        else
        {
            target_function = cblc_find_function_in_unit(unit,
                substituted.call_identifier);
            if (!target_function)
                return (FT_FAILURE);
            if (cblc_emit_local_call_argument_moves(unit, target_function,
                    &substituted, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           PERFORM %s", cobol_name) < 0)
                return (FT_FAILURE);
        }
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];
        const t_cblc_function *target_function;

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
        target_function = cblc_find_function_in_unit(unit, substituted.call_identifier);
        if (!target_function)
            return (FT_FAILURE);
        if (cblc_emit_local_call_argument_moves(unit, target_function, &substituted,
                builder) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           PERFORM %s", cobol_name) < 0)
            return (FT_FAILURE);
        if (append_statement_line(line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                substituted.source, substituted.target) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_RETURN)
        return (FT_SUCCESS);
    if (substituted.type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        || substituted.type == CBLC_STATEMENT_DESTRUCT)
        return (cblc_emit_lifecycle_statement(unit, &substituted, builder));
    if (substituted.type == CBLC_STATEMENT_METHOD_CALL
        || substituted.type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        return (cblc_emit_method_statement(unit, &substituted, builder));
    (void)function;
    return (FT_FAILURE);
}

static int cblc_extract_call_argument(const t_cblc_statement *statement,
    size_t argument_index, char *buffer, size_t buffer_size)
{
    const char *cursor;
    size_t current_index;
    size_t length;

    if (!statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    cursor = statement->call_arguments;
    current_index = 0;
    while (*cursor != '\0' && current_index < argument_index)
    {
        while (*cursor != '\0' && *cursor != ',')
            cursor += 1;
        if (*cursor == ',')
        {
            cursor += 1;
            current_index += 1;
        }
    }
    if (current_index != argument_index)
        return (FT_FAILURE);
    while (*cursor == ' ' || *cursor == '\t')
        cursor += 1;
    length = 0;
    while (cursor[length] != '\0' && cursor[length] != ',')
        length += 1;
    while (length > 0
        && (cursor[length - 1] == ' ' || cursor[length - 1] == '\t'))
        length -= 1;
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    std::memcpy(buffer, cursor, length);
    buffer[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_argument_matches_parameter(const t_cblc_translation_unit *unit,
    const char *argument, const t_cblc_parameter *parameter)
{
    const t_cblc_data_item *item;

    if (!unit || !argument || !parameter)
        return (0);
    item = cblc_find_data_item_const(unit, argument);
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_INT)
    {
        if (item && item->kind == CBLC_DATA_KIND_STRUCT)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
    {
        if (argument[0] == '"')
            return (1);
        if (!item || item->kind != CBLC_DATA_KIND_STRING)
            return (0);
        return (1);
    }
    if (parameter->kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
    {
        if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
            return (0);
        if (std::strncmp(item->struct_type_name, parameter->type_name,
                sizeof(item->struct_type_name)) != 0)
            return (0);
        return (1);
    }
    return (0);
}

static const t_cblc_constructor *cblc_find_constructor_for_arguments(
    const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *call_arguments, size_t call_argument_count)
{
    size_t constructor_index;

    if (!unit || !type)
        return (NULL);
    constructor_index = 0;
    while (constructor_index < type->constructor_count)
    {
        const t_cblc_constructor *constructor;
        t_cblc_statement statement;
        size_t parameter_index;
        int matches;

        constructor = &type->constructors[constructor_index];
        if (constructor->parameter_count != call_argument_count)
        {
            constructor_index += 1;
            continue ;
        }
        std::memset(&statement, 0, sizeof(statement));
        if (call_arguments)
            ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        parameter_index = 0;
        matches = 1;
        while (parameter_index < call_argument_count)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_extract_call_argument(&statement, parameter_index, argument,
                    sizeof(argument)) != FT_SUCCESS
                || !cblc_argument_matches_parameter(unit, argument,
                    &constructor->parameters[parameter_index]))
            {
                matches = 0;
                break ;
            }
            parameter_index += 1;
        }
        if (matches)
            return (constructor);
        constructor_index += 1;
    }
    return (NULL);
}

static int cblc_emit_parameter_argument_moves(const t_cblc_translation_unit *unit,
    const char *call_arguments,
    size_t call_argument_count, const t_cblc_parameter *parameters,
    size_t parameter_count, t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !call_arguments || !parameters || !builder)
        return (FT_FAILURE);
    if (parameter_count != call_argument_count)
        return (FT_FAILURE);
    index = 0;
    while (index < parameter_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char expression[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];
        t_cblc_statement statement;

        std::memset(&statement, 0, sizeof(statement));
        ft_strlcpy(statement.call_arguments, call_arguments, sizeof(statement.call_arguments));
        statement.call_argument_count = call_argument_count;
        if (cblc_extract_call_argument(&statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRUCT)
        {
            const t_cblc_data_item *item;

            item = cblc_find_data_item_const(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRUCT)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                    item->cobol_name, parameters[index].cobol_name) < 0)
                return (FT_FAILURE);
        }
        else if (parameters[index].kind == TRANSPILE_FUNCTION_PARAMETER_STRING)
        {
            const t_cblc_data_item *item;
            char line_length[256];
            char line_buffer[256];

            item = cblc_find_data_item_const(unit, argument);
            if (!item || item->kind != CBLC_DATA_KIND_STRING)
                return (FT_FAILURE);
            if (std::snprintf(line_buffer, sizeof(line_buffer), "           MOVE %s-BUF TO %s-BUF.",
                    item->cobol_name, parameters[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (std::snprintf(line_length, sizeof(line_length), "           COMPUTE %s-LEN = %s-LEN.",
                    parameters[index].cobol_name, item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line_buffer) != FT_SUCCESS)
                return (FT_FAILURE);
            ft_strlcpy(line, line_length, sizeof(line));
        }
        else
        {
            if (cblc_translate_expression_for_cobol(argument, expression,
                    sizeof(expression)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                    parameters[index].cobol_name, expression) < 0)
                return (FT_FAILURE);
        }
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_local_call_argument_moves(const t_cblc_translation_unit *unit,
    const t_cblc_function *target_function, const t_cblc_statement *statement,
    t_cobol_text_builder *builder)
{
    if (!unit || !target_function || !statement || !builder)
        return (FT_FAILURE);
    return (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
            statement->call_argument_count, target_function->parameters,
            target_function->parameter_count, builder));
}

static int cblc_append_external_call_using_argument(char *buffer, size_t buffer_size,
    const char *argument_text, int by_value)
{
    if (!buffer || buffer_size == 0 || !argument_text || argument_text[0] == '\0')
        return (FT_FAILURE);
    if (buffer[0] != '\0')
        ft_strlcat(buffer, " ", buffer_size);
    if (by_value)
        ft_strlcat(buffer, "BY VALUE ", buffer_size);
    else
        ft_strlcat(buffer, "BY REFERENCE ", buffer_size);
    ft_strlcat(buffer, argument_text, buffer_size);
    return (FT_SUCCESS);
}

static int cblc_argument_is_numeric_literal(const char *argument)
{
    size_t index;

    if (!argument || argument[0] == '\0')
        return (0);
    index = 0;
    if (argument[index] == '+' || argument[index] == '-')
        index += 1;
    if (argument[index] == '\0')
        return (0);
    while (argument[index] != '\0')
    {
        if (argument[index] < '0' || argument[index] > '9')
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_expression_is_simple_cobol_reference(const char *expression)
{
    size_t index;

    if (!expression || expression[0] == '\0')
        return (0);
    index = 0;
    while (expression[index] != '\0')
    {
        if ((expression[index] >= 'A' && expression[index] <= 'Z')
            || (expression[index] >= 'a' && expression[index] <= 'z')
            || (expression[index] >= '0' && expression[index] <= '9')
            || expression[index] == '-'
            || expression[index] == '_'
            || expression[index] == '('
            || expression[index] == ')'
            || expression[index] == ':')
        {
            index += 1;
            continue ;
        }
        return (0);
    }
    return (1);
}

static int cblc_build_external_call_arguments(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!unit || !statement || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    buffer[0] = '\0';
    index = 0;
    while (index < statement->call_argument_count)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];

        if (cblc_extract_call_argument(statement, index, argument,
                sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (index > 0)
            ft_strlcat(buffer, " ", buffer_size);
        if (cblc_argument_is_numeric_literal(argument))
        {
            ft_strlcat(buffer, "BY VALUE ", buffer_size);
            ft_strlcat(buffer, argument, buffer_size);
        }
        else
        {
            char cobol_reference[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_expression_for_cobol(argument, cobol_reference,
                    sizeof(cobol_reference)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (!cblc_expression_is_simple_cobol_reference(cobol_reference))
                return (FT_FAILURE);
            ft_strlcat(buffer, "BY REFERENCE ", buffer_size);
            ft_strlcat(buffer, cobol_reference, buffer_size);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_is_entry_parameter_item(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_function *entry_function;
    size_t index;

    if (!unit || !item || item->is_alias || item->is_function_local)
        return (0);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (std::strncmp(item->owner_function_name, entry_function->source_name,
            sizeof(item->owner_function_name)) != 0)
        return (0);
    index = 0;
    while (index < entry_function->parameter_count)
    {
        if (std::strncmp(item->source_name,
                entry_function->parameters[index].actual_source_name,
                sizeof(item->source_name)) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_is_external_entry_function(const t_cblc_translation_unit *unit)
{
    const t_cblc_function *entry_function;

    if (!unit || unit->function_count == 0)
        return (0);
    if (unit->entry_function_index == static_cast<size_t>(-1)
        || unit->entry_function_index >= unit->function_count)
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (std::strncmp(entry_function->source_name, "main",
            sizeof(entry_function->source_name)) == 0)
        return (0);
    return (1);
}

static int cblc_is_entry_return_item(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item)
{
    const t_cblc_function *entry_function;

    if (!unit || !item)
        return (0);
    if (!cblc_is_external_entry_function(unit))
        return (0);
    entry_function = &unit->functions[unit->entry_function_index];
    if (entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID)
        return (0);
    if (std::strncmp(item->source_name, entry_function->return_source_name,
            sizeof(item->source_name)) == 0)
        return (1);
    return (0);
}

static int cblc_emit_method_body(const t_cblc_translation_unit *unit, const t_cblc_data_item *receiver,
    const t_cblc_method *method, const char *assign_target, t_cobol_text_builder *builder)
{
    size_t index;

    if (!unit || !receiver || !method || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < method->statement_count)
    {
        const t_cblc_statement *body_statement;
        t_cblc_statement substituted;
        char line[256];

        body_statement = &method->statements[index];
        substituted = *body_statement;
        if (cblc_replace_this_prefix(body_statement->target, receiver->cobol_name, substituted.target,
                sizeof(substituted.target)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (cblc_replace_this_prefix(body_statement->source, receiver->cobol_name, substituted.source,
                sizeof(substituted.source)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (substituted.type == CBLC_STATEMENT_RETURN)
        {
            if (method->return_kind == CBLC_FUNCTION_RETURN_INT)
            {
                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s.",
                        assign_target, substituted.source) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else if (method->return_kind == CBLC_FUNCTION_RETURN_STRUCT)
            {
                if (!assign_target || assign_target[0] == '\0')
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s.",
                        substituted.source, assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (FT_SUCCESS);
        }
        if (cblc_emit_substituted_statement(unit, NULL, &substituted, receiver->cobol_name, builder)
            != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_method_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
    const t_cblc_data_item *receiver;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    const char *receiver_name;
    const char *assign_target;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_METHOD_CALL)
    {
        receiver_name = statement->target;
        assign_target = NULL;
    }
    else if (statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
    {
        receiver_name = statement->source;
        assign_target = statement->target;
    }
    else
        return (FT_FAILURE);
    receiver = cblc_find_data_item_by_cobol(unit, receiver_name);
    if (!receiver)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver);
    if (!receiver_type)
        return (FT_FAILURE);
    method = cblc_find_method_on_type(receiver_type, statement->call_identifier);
    if (!method)
        return (FT_FAILURE);
    if (receiver_type->is_builtin
        && std::strncmp(receiver_type->source_name, "string", TRANSPILE_IDENTIFIER_MAX) == 0)
    {
        char line[512];
        const t_cblc_data_item *status_item;

        if (std::strncmp(statement->call_identifier, "append",
                sizeof(statement->call_identifier)) == 0)
        {
            if (unit->helper_status_index < 0
                || static_cast<size_t>(unit->helper_status_index) >= unit->data_count)
                return (FT_FAILURE);
            status_item = &unit->data_items[unit->helper_status_index];
            if (!status_item)
                return (FT_FAILURE);
            if (statement->is_literal)
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE 0 %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source,
                        status_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCAT-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, receiver->cobol_name, statement->source,
                        status_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "len",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s-LEN.",
                    assign_target, receiver->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "capacity",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %zu.",
                    assign_target, receiver->length) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "clear",
                sizeof(statement->call_identifier)) == 0)
        {
            if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = 0.",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        if (std::strncmp(statement->call_identifier, "empty",
                sizeof(statement->call_identifier)) == 0)
        {
            if (!assign_target || assign_target[0] == '\0')
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           IF %s-LEN = 0",
                    receiver->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "equals",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                char literal_cobol[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text,
                        sizeof(literal_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                literal_cobol[0] = '"';
                literal_cobol[1] = '\0';
                ft_strlcat(literal_cobol, literal_text, sizeof(literal_cobol));
                ft_strlcat(literal_cobol, "\"", sizeof(literal_cobol));
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = %zu AND %s-BUF(1:%s-LEN) = %s",
                        receiver->cobol_name, literal_length, receiver->cobol_name,
                        receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = %s-LEN",
                        receiver->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "               IF %s-BUF(1:%s-LEN) = %s-BUF(1:%s-LEN)",
                        receiver->cobol_name, receiver->cobol_name,
                        argument_item->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] != '"')
            {
                if (cobol_text_builder_append_line(builder, "               END-IF") != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "compare",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                char literal_cobol[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                literal_cobol[0] = '"';
                literal_cobol[1] = '\0';
                ft_strlcat(literal_cobol, literal_text, sizeof(literal_cobol));
                ft_strlcat(literal_cobol, "\"", sizeof(literal_cobol));
                if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN < %zu",
                        receiver->cobol_name, literal_length) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = -1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           ELSE IF %s-LEN > %zu",
                        receiver->cobol_name, literal_length) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           ELSE IF %s-BUF(1:%s-LEN) < %s",
                        receiver->cobol_name, receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = -1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           ELSE IF %s-BUF(1:%s-LEN) > %s",
                        receiver->cobol_name, receiver->cobol_name, literal_cobol) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, "           END-IF."));
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           CALL 'CBLC-STRCMP-STRING' USING BY REFERENCE %s BY REFERENCE %s BY REFERENCE %s.",
                        receiver->cobol_name, argument_item->cobol_name, assign_target) < 0)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, line));
            }
        }
        if (std::strncmp(statement->call_identifier, "contains",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                if (literal_length == 0)
                {
                    if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 1.",
                            assign_target) < 0)
                        return (FT_FAILURE);
                    return (cobol_text_builder_append_line(builder, line));
                }
                if (std::snprintf(line, sizeof(line),
                        "           INSPECT %s-BUF(1:%s-LEN) TALLYING %s FOR ALL %s",
                        receiver->cobol_name, receiver->cobol_name, assign_target, argument) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "           IF %s-LEN = 0",
                        argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           ELSE") < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line),
                        "               INSPECT %s-BUF(1:%s-LEN) TALLYING %s FOR ALL %s-BUF(1:%s-LEN)",
                        receiver->cobol_name, receiver->cobol_name, assign_target,
                        argument_item->cobol_name, argument_item->cobol_name) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           END-IF.") < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           IF %s > 0",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                        assign_target) < 0)
                    return (FT_FAILURE);
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                return (cobol_text_builder_append_line(builder, "           END-IF."));
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           IF %s > 0",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
        if (std::strncmp(statement->call_identifier, "starts_with",
                sizeof(statement->call_identifier)) == 0
            || std::strncmp(statement->call_identifier, "ends_with",
                sizeof(statement->call_identifier)) == 0)
        {
            char argument[TRANSPILE_STATEMENT_TEXT_MAX];

            if (!assign_target || assign_target[0] == '\0'
                || statement->call_argument_count != 1)
                return (FT_FAILURE);
            if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = 0.", assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (argument[0] == '"')
            {
                char literal_text[TRANSPILE_STATEMENT_TEXT_MAX];
                const char *literal_cursor;
                size_t literal_length;

                literal_cursor = argument;
                if (cblc_parse_string_literal(&literal_cursor, literal_text, sizeof(literal_text))
                    != FT_SUCCESS || *literal_cursor != '\0')
                    return (FT_FAILURE);
                literal_length = std::strlen(literal_text);
                if (std::strncmp(statement->call_identifier, "starts_with",
                        sizeof(statement->call_identifier)) == 0)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %zu AND %s-BUF(1:%zu) = %s",
                            receiver->cobol_name, literal_length, receiver->cobol_name,
                            literal_length, argument) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %zu AND %s-BUF(%s-LEN - %zu + 1:%zu) = %s",
                            receiver->cobol_name, literal_length, receiver->cobol_name,
                            receiver->cobol_name, literal_length, literal_length, argument) < 0)
                        return (FT_FAILURE);
                }
            }
            else
            {
                const t_cblc_data_item *argument_item;

                argument_item = cblc_find_data_item_const(unit, argument);
                if (!argument_item || argument_item->kind != CBLC_DATA_KIND_STRING)
                    return (FT_FAILURE);
                if (std::strncmp(statement->call_identifier, "starts_with",
                        sizeof(statement->call_identifier)) == 0)
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %s-LEN AND %s-BUF(1:%s-LEN) = %s-BUF(1:%s-LEN)",
                            receiver->cobol_name, argument_item->cobol_name,
                            receiver->cobol_name, argument_item->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line),
                            "           IF %s-LEN >= %s-LEN AND %s-BUF(%s-LEN - %s-LEN + 1:%s-LEN) = %s-BUF(1:%s-LEN)",
                            receiver->cobol_name, argument_item->cobol_name,
                            receiver->cobol_name, receiver->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name,
                            argument_item->cobol_name, argument_item->cobol_name) < 0)
                        return (FT_FAILURE);
                }
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "               COMPUTE %s = 1",
                    assign_target) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, "           END-IF."));
        }
    }
    if (method->parameter_count > 0)
    {
        if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                statement->call_argument_count, method->parameters,
                method->parameter_count, builder) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (cblc_emit_method_body(unit, receiver, method, assign_target, builder));
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
    const t_cblc_constructor *constructor;
    const t_cblc_data_item *item;
    const t_cblc_struct_type *type;
    const t_cblc_statement *body;
    size_t count;
    size_t index;

    if (!unit || !statement || !builder)
        return (FT_FAILURE);
    item = cblc_find_data_item_by_cobol(unit, statement->target);
    if (!item)
        return (FT_FAILURE);
    if (item->kind == CBLC_DATA_KIND_STRING)
    {
        char argument[TRANSPILE_STATEMENT_TEXT_MAX];
        char line[256];
        const t_cblc_data_item *source_item;
        const char *literal_cursor;
        char literal_buffer[TRANSPILE_STATEMENT_TEXT_MAX];
        size_t assigned_length;

        if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-LEN.",
                item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (statement->type != CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->call_argument_count == 0)
            return (FT_SUCCESS);
        if (statement->call_argument_count != 1)
            return (FT_FAILURE);
        if (cblc_extract_call_argument(statement, 0, argument, sizeof(argument)) != FT_SUCCESS)
            return (FT_FAILURE);
        if (argument[0] == '"')
        {
            literal_cursor = argument;
            if (cblc_parse_string_literal(&literal_cursor, literal_buffer, sizeof(literal_buffer))
                != FT_SUCCESS || *literal_cursor != '\0')
                return (FT_FAILURE);
            assigned_length = std::strlen(literal_buffer);
            if (assigned_length > item->length)
                assigned_length = item->length;
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s-BUF.",
                    argument, item->cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %zu.",
                    item->cobol_name, assigned_length) < 0)
                return (FT_FAILURE);
            return (cobol_text_builder_append_line(builder, line));
        }
        source_item = cblc_find_data_item_const(unit, argument);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE %s-BUF TO %s-BUF.",
                source_item->cobol_name, item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           COMPUTE %s-LEN = %s-LEN.",
                item->cobol_name, source_item->cobol_name) < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && ((statement->call_argument_count == 0 && type->has_default_constructor)
            || statement->call_argument_count > 0))
    {
        constructor = cblc_find_constructor_for_arguments(unit, type,
                statement->call_arguments, statement->call_argument_count);
        if (!constructor)
        {
            if (statement->call_argument_count == 0)
                return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder));
            return (FT_FAILURE);
        }
        if (constructor->statement_count == 0)
            return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder));
        if (constructor->parameter_count > 0)
        {
            if (cblc_emit_parameter_argument_moves(unit, statement->call_arguments,
                    statement->call_argument_count, constructor->parameters,
                    constructor->parameter_count, builder) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        body = constructor->statements;
        count = constructor->statement_count;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &body[index], item->cobol_name, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    if (statement->type == CBLC_STATEMENT_DESTRUCT && type->destructor_statement_count > 0)
    {
        body = type->destructor_statements;
        count = type->destructor_statement_count;
        index = 0;
        while (index < count)
        {
            if (cblc_emit_substituted_statement(unit, NULL, &body[index], item->cobol_name, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
            index += 1;
        }
        return (FT_SUCCESS);
    }
    return (cblc_emit_lifecycle_recursive(unit, type, item->cobol_name, builder));
}

static int cblc_emit_function(const t_cblc_translation_unit *unit, const t_cblc_function *function,
    t_cobol_text_builder *builder, int append_stop_run)
{
    char line[256];
    size_t index;

    if (!unit || !function || !builder)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "%s.", function->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < function->statement_count)
    {
        const t_cblc_statement *statement;
        int skip_append;

        statement = &function->statements[index];
        skip_append = 0;
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (statement->is_literal)
                ft_strlcpy(source_text, statement->source, sizeof(source_text));
            else if (cblc_translate_expression_for_cobol(statement->source, source_text,
                        sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(statement->target, target_text,
                    sizeof(target_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    source_text, target_text) < 0)
                return (FT_FAILURE);
        }
        else if (statement->type == CBLC_STATEMENT_DISPLAY)
        {
            char base_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_data_item *item;

            if (statement->is_literal)
            {
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                        statement->source) < 0)
                    return (FT_FAILURE);
            }
            else
            {
            item = NULL;
            if (cblc_parse_cobol_reference_base(statement->source, base_name, sizeof(base_name))
                == FT_SUCCESS)
                item = cblc_find_data_item_by_cobol(unit, base_name);
            if (item && item->kind == CBLC_DATA_KIND_STRING)
            {
                char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                char length_name[TRANSPILE_IDENTIFIER_MAX];
                char translated_buffer_name[TRANSPILE_STATEMENT_TEXT_MAX];
                char translated_length_name[TRANSPILE_STATEMENT_TEXT_MAX];

                if (std::strncmp(statement->source, item->cobol_name,
                        sizeof(item->cobol_name)) == 0)
                {
                    cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name,
                        sizeof(buffer_name));
                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                }
                else if (std::strchr(statement->source, '['))
                {
                    char index_expression[TRANSPILE_STATEMENT_TEXT_MAX];
                    const char *open_bracket;
                    size_t index_length;

                    open_bracket = std::strchr(statement->source, '[');
                    if (!open_bracket)
                        return (FT_FAILURE);
                    open_bracket += 1;
                    index_length = std::strcspn(open_bracket, "]");
                    if (index_length + 1 >= sizeof(index_expression)
                        || open_bracket[index_length] != ']')
                        return (FT_FAILURE);
                    std::memcpy(index_expression, open_bracket, index_length);
                    index_expression[index_length] = '\0';
                    if (cblc_format_string_component_expression_reference(item->cobol_name,
                            index_expression, "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_expression_reference(item->cobol_name,
                            index_expression, "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    size_t zero_based_index;

                    if (cblc_parse_occurs_reference_index(statement->source, &zero_based_index)
                        != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-BUF", buffer_name, sizeof(buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_format_string_component_reference(item->cobol_name, 1, zero_based_index,
                            "-LEN", length_name, sizeof(length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (std::strchr(statement->source, '['))
                {
                    if (cblc_translate_expression_for_cobol(buffer_name, translated_buffer_name,
                            sizeof(translated_buffer_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (cblc_translate_expression_for_cobol(length_name, translated_length_name,
                            sizeof(translated_length_name)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                else
                {
                    ft_strlcpy(translated_buffer_name, buffer_name, sizeof(translated_buffer_name));
                    ft_strlcpy(translated_length_name, length_name, sizeof(translated_length_name));
                }
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s(1:%s)",
                        translated_buffer_name, translated_length_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                char source_text[TRANSPILE_STATEMENT_TEXT_MAX];

                if (cblc_translate_expression_for_cobol(statement->source, source_text,
                        sizeof(source_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                        source_text) < 0)
                    return (FT_FAILURE);
            }
            }
        }
        else if (statement->type == CBLC_STATEMENT_COMPUTE)
        {
            char source_text[TRANSPILE_STATEMENT_TEXT_MAX];
            char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

            if (cblc_translate_expression_for_cobol(statement->source, source_text,
                    sizeof(source_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (cblc_translate_expression_for_cobol(statement->target, target_text,
                    sizeof(target_text)) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                    target_text, source_text) < 0)
                return (FT_FAILURE);
        }
        else if (statement->type == CBLC_STATEMENT_CALL)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_function *target_function;

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (statement->call_is_external)
            {
                if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0
                    && statement->call_arguments[0] != '\0')
                {
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            statement->call_arguments) < 0)
                        return (FT_FAILURE);
                }
                else if (statement->call_arguments[0] != '\0')
                {
                    char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];

                    if (cblc_build_external_call_arguments(unit, statement,
                            using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            using_arguments) < 0)
                        return (FT_FAILURE);
                }
                else
                {
                    if (std::snprintf(line, sizeof(line), "           CALL '%s'",
                            cobol_name) < 0)
                        return (FT_FAILURE);
                }
            }
            else
            {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (!target_function)
                    return (FT_FAILURE);
                if (cblc_emit_local_call_argument_moves(unit, target_function,
                        statement, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];
            const t_cblc_function *target_function;

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (statement->call_is_external)
            {
                char using_arguments[TRANSPILE_STATEMENT_TEXT_MAX];
                char target_text[TRANSPILE_STATEMENT_TEXT_MAX];

                if (statement->target[0] == '\0')
                    return (FT_FAILURE);
                using_arguments[0] = '\0';
                if (statement->call_arguments[0] != '\0')
                {
                    if (cblc_build_external_call_arguments(unit, statement,
                            using_arguments, sizeof(using_arguments)) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
                if (cblc_translate_expression_for_cobol(statement->target, target_text,
                        sizeof(target_text)) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (cblc_append_external_call_using_argument(using_arguments,
                        sizeof(using_arguments), target_text, 0) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, using_arguments) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                target_function = cblc_find_function_in_unit(unit,
                    statement->call_identifier);
                if (!target_function)
                    return (FT_FAILURE);
                if (cblc_emit_local_call_argument_moves(unit, target_function,
                        statement, builder) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
                size_t perform_length;

                perform_length = std::strlen(line);
                if (perform_length + 1 >= sizeof(line))
                    return (FT_FAILURE);
                line[perform_length] = '.';
                line[perform_length + 1] = '\0';
                if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                    return (FT_FAILURE);
                if (statement->source[0] == '\0' || statement->target[0] == '\0')
                    return (FT_FAILURE);
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                        statement->source, statement->target) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_RETURN)
        {
            if (statement->target[0] == '\0' || statement->source[0] == '\0')
            {
                skip_append = 1;
            }
            else
            {
                if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                        statement->source, statement->target) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
            || statement->type == CBLC_STATEMENT_DESTRUCT)
        {
            if (cblc_emit_lifecycle_statement(unit, statement, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else if (statement->type == CBLC_STATEMENT_METHOD_CALL
            || statement->type == CBLC_STATEMENT_METHOD_CALL_ASSIGN)
        {
            if (cblc_emit_method_statement(unit, statement, builder) != FT_SUCCESS)
                return (FT_FAILURE);
            skip_append = 1;
        }
        else
            return (FT_FAILURE);
        if (!skip_append)
        {
            size_t length;

            length = std::strlen(line);
            if (length == 0 || line[length - 1] != '.')
            {
                if (length + 1 >= sizeof(line))
                    return (FT_FAILURE);
                line[length] = '.';
                line[length + 1] = '\0';
            }
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    if (append_stop_run)
    {
        if (std::strncmp(function->source_name, "main", sizeof(function->source_name)) == 0)
        {
            if (cobol_text_builder_append_line(builder, "           STOP RUN.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
        {
            if (cobol_text_builder_append_line(builder, "           GOBACK.") != FT_SUCCESS)
                return (FT_FAILURE);
        }
    }
    if (cobol_text_builder_append_line(builder, "") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_emit_struct_fields(const t_cblc_translation_unit *unit, const t_cblc_struct_type *type,
    const char *prefix, size_t level, t_cobol_text_builder *builder)
{
    char line[256];
    char nested_prefix[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!unit || !type || !prefix || !builder)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if (type->fields[index].kind == CBLC_DATA_KIND_STRING)
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s-LEN PIC 9(4) COMP VALUE 0.",
                    level + 5, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s-BUF PIC X(%zu).",
                    level + 5, prefix, type->fields[index].cobol_name,
                    type->fields[index].length) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC X(%zu).",
                    level, prefix, type->fields[index].cobol_name,
                    type->fields[index].length) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_INT)
        {
            if (type->fields[index].array_count > 0)
            {
                if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9) OCCURS %zu TIMES.",
                        level, prefix, type->fields[index].cobol_name,
                        type->fields[index].array_count) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9).",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else if (type->fields[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            const t_cblc_struct_type *field_type;

            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
                    level, prefix, type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
                return (FT_FAILURE);
            if (std::snprintf(nested_prefix, sizeof(nested_prefix), "%s-%s", prefix,
                    type->fields[index].cobol_name) < 0)
                return (FT_FAILURE);
            field_type = cblc_find_struct_type(unit, type->fields[index].struct_type_name);
            if (!field_type)
                return (FT_FAILURE);
            if (cblc_emit_struct_fields(unit, field_type, nested_prefix, level + 5, builder)
                != FT_SUCCESS)
                return (FT_FAILURE);
        }
        else
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_emit_struct_instance(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item, t_cobol_text_builder *builder)
{
    const t_cblc_struct_type *type;
    char line[256];

    if (!unit || !item || !builder)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (std::snprintf(line, sizeof(line), "       01 %s.", item->cobol_name) < 0)
        return (FT_FAILURE);
    if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
        return (FT_FAILURE);
    return (cblc_emit_struct_fields(unit, type, item->cobol_name, 5, builder));
}

static int cblc_translation_unit_calls_are_resolved(const t_cblc_translation_unit *unit)
{
    size_t function_index;

    if (!unit)
        return (FT_FAILURE);
    function_index = 0;
    while (function_index < unit->function_count)
    {
        const t_cblc_function *function;
        size_t statement_index;

        function = &unit->functions[function_index];
        statement_index = 0;
        while (statement_index < function->statement_count)
        {
            const t_cblc_statement *statement;
            const t_cblc_function *local_target;

            statement = &function->statements[statement_index];
            if (statement->type != CBLC_STATEMENT_CALL
                && statement->type != CBLC_STATEMENT_CALL_ASSIGN)
            {
                statement_index += 1;
                continue ;
            }
            if (std::strncmp(statement->call_identifier, "CBLC-", 5) == 0)
            {
                statement_index += 1;
                continue ;
            }
            local_target = cblc_find_function_in_unit(unit, statement->call_identifier);
            if (local_target)
            {
                statement_index += 1;
                continue ;
            }
            if (!statement->call_is_external)
                return (FT_FAILURE);
            statement_index += 1;
        }
        function_index += 1;
    }
    return (FT_SUCCESS);
}

int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;
    size_t entry_index;
    const t_cblc_function *entry_function;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    if (cblc_translation_unit_calls_are_resolved(unit) != FT_SUCCESS)
        return (FT_FAILURE);
    entry_index = unit->entry_function_index;
    if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
        entry_index = 0;
    entry_function = NULL;
    if (unit->function_count > 0 && entry_index < unit->function_count)
        entry_function = &unit->functions[entry_index];
    cobol_text_builder_init(&builder);
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (std::snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       ENVIRONMENT DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
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
    index = 0;
    while (index < unit->data_count)
    {
        if (unit->data_items[index].is_alias || std::strchr(unit->data_items[index].source_name, '.')
            || cblc_is_entry_parameter_item(unit, &unit->data_items[index])
            || cblc_is_entry_return_item(unit, &unit->data_items[index]))
        {
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
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) OCCURS %zu TIMES.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].array_count) < 0)
                    goto cleanup;
            }
            else if (unit->data_items[index].is_const
                && unit->data_items[index].has_initializer)
            {
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9) VALUE %s.",
                        unit->data_items[index].cobol_name,
                        unit->data_items[index].initializer_text) < 0)
                    goto cleanup;
            }
            else if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                    unit->data_items[index].cobol_name) < 0)
                goto cleanup;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRING)
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
                        : unit->data_items[index].length) < 0)
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
            }
            else if (std::snprintf(line, sizeof(line), "          05 %s-BUF PIC X(%zu).",
                    unit->data_items[index].cobol_name,
                    unit->data_items[index].length) < 0)
                goto cleanup;
            if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                goto cleanup;
            index += 1;
            continue ;
        }
        else if (unit->data_items[index].kind == CBLC_DATA_KIND_STRUCT)
        {
            if (cblc_emit_struct_instance(unit, &unit->data_items[index], &builder) != FT_SUCCESS)
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
    if (entry_function && cblc_is_external_entry_function(unit)
        && (entry_function->parameter_count > 0
            || entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID))
    {
        if (cobol_text_builder_append_line(&builder, "       LINKAGE SECTION.") != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (entry_function->parameters[index].kind != TRANSPILE_FUNCTION_PARAMETER_INT)
                goto cleanup;
            if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                    entry_function->parameters[index].cobol_name) < 0)
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
                if (std::snprintf(line, sizeof(line), "       01 %s PIC S9(9).",
                        entry_function->return_cobol_name) < 0)
                    goto cleanup;
                if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
                    goto cleanup;
            }
            else if (return_item->kind == CBLC_DATA_KIND_STRUCT)
            {
                if (cblc_emit_struct_instance(unit, return_item, &builder) != FT_SUCCESS)
                    goto cleanup;
            }
            else
                goto cleanup;
        }
        ft_strlcpy(line, "       PROCEDURE DIVISION USING", sizeof(line));
        if (entry_function->parameter_count == 0
            && entry_function->return_kind == CBLC_FUNCTION_RETURN_VOID)
            goto cleanup;
        index = 0;
        while (index < entry_function->parameter_count)
        {
            if (std::strlen(line) + 1
                + std::strlen(entry_function->parameters[index].cobol_name)
                + 1 >= sizeof(line))
                goto cleanup;
            ft_strlcat(line, " ", sizeof(line));
            ft_strlcat(line, entry_function->parameters[index].cobol_name, sizeof(line));
            index += 1;
        }
        if (entry_function->return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            if (std::strlen(line) + 1
                + std::strlen(entry_function->return_cobol_name) + 1 >= sizeof(line))
                goto cleanup;
            ft_strlcat(line, " ", sizeof(line));
            ft_strlcat(line, entry_function->return_cobol_name, sizeof(line));
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
    if (unit->function_count > 0)
    {
        if (cblc_emit_function(unit, &unit->functions[entry_index], &builder, 1) != FT_SUCCESS)
            goto cleanup;
        index = 0;
        while (index < unit->function_count)
        {
            if (index != entry_index)
            {
                if (cblc_emit_function(unit, &unit->functions[index], &builder, 0) != FT_SUCCESS)
                    goto cleanup;
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
    cobol_text_builder_dispose(&builder);
    if (!*out_text)
        return (FT_FAILURE);
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
