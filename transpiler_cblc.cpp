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
static int cblc_parse_call(const char **cursor, t_cblc_function *function);
static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function);
static int cblc_capture_lifecycle_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity);
static int cblc_capture_constructor_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity);
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
static const t_cblc_struct_type *cblc_find_receiver_type(const t_cblc_translation_unit *unit,
    const t_cblc_data_item *item);

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
    if (cblc_struct_type_ensure_method_capacity(type, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "append", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_VOID;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
    type->method_count += 1;
    method = &type->methods[type->method_count];
    std::memset(method, 0, sizeof(*method));
    ft_strlcpy(method->source_name, "len", sizeof(method->source_name));
    cblc_identifier_to_cobol(method->source_name, method->cobol_name, sizeof(method->cobol_name));
    method->return_kind = CBLC_FUNCTION_RETURN_INT;
    method->visibility = CBLC_MEMBER_VISIBILITY_PUBLIC;
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

static int cblc_finalize_data_item(t_cblc_translation_unit *unit, const char *identifier,
    size_t length, t_cblc_data_kind kind, int is_const, int has_initializer,
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
    return (cblc_finalize_data_item(unit, identifier, length, CBLC_DATA_KIND_CHAR,
            is_const, is_const, initializer_text, initializer_length));
}

static int cblc_parse_string_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];
    size_t length;
    size_t initializer_length;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "string"))
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
        if (cblc_parse_string_const_initializer(cursor, initializer_text,
                sizeof(initializer_text), length, &initializer_length) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
    }
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_finalize_data_item(unit, identifier, length, CBLC_DATA_KIND_STRING,
            is_const, is_const, initializer_text, initializer_length));
}

static int cblc_parse_int_declaration(const char **cursor, t_cblc_translation_unit *unit,
    int is_const)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char initializer_text[TRANSPILE_STATEMENT_TEXT_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "int"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    initializer_text[0] = '\0';
    if (is_const)
    {
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
    return (cblc_finalize_data_item(unit, identifier, 0, CBLC_DATA_KIND_INT,
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

static int cblc_parse_data_reference(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_data_item **out_item, int *out_len_reference)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit || !out_item)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    item = cblc_find_data_item(unit, identifier);
    if (!item)
        return (FT_FAILURE);
    while (**cursor == '.')
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
    }
    if (out_len_reference)
        *out_len_reference = 0;
    *out_item = item;
    return (FT_SUCCESS);
}

static int cblc_struct_type_append_field(t_cblc_struct_type *type, const char *identifier,
    size_t length, t_cblc_data_kind kind, const char *struct_type_name, int is_const,
    t_cblc_member_visibility visibility)
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
    int is_const;

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
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_struct_type_append_field(type, identifier, 0, CBLC_DATA_KIND_INT, NULL,
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
        return (cblc_struct_type_append_field(type, identifier, length, CBLC_DATA_KIND_CHAR, NULL,
                is_const, visibility));
    }
    if (cblc_match_keyword(cursor, "string"))
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
        return (cblc_struct_type_append_field(type, identifier, length, CBLC_DATA_KIND_STRING, NULL,
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
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    return (cblc_struct_type_append_field(type, identifier, 0, CBLC_DATA_KIND_STRUCT,
        field_type->source_name, is_const, visibility));
}

static int cblc_parse_lifecycle_declaration(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    int is_destructor;

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    is_destructor = 0;
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
            type->has_default_constructor = 1;
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
                : cblc_capture_constructor_body(cursor, unit, type, &statements, &statement_count,
                    &statement_capacity)) != FT_SUCCESS)
        {
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
            if (type->constructor_statements)
                cma_free(type->constructor_statements);
            type->constructor_statements = statements;
            type->constructor_statement_count = statement_count;
            type->constructor_statement_capacity = statement_capacity;
            type->has_default_constructor = 1;
        }
        return (FT_SUCCESS);
    }
    *cursor = start;
    return (FT_FAILURE);
}

static int cblc_capture_method_body(const char **cursor, t_cblc_translation_unit *unit,
    const t_cblc_struct_type *type, t_cblc_function_return_kind return_kind,
    t_cblc_statement **out_statements, size_t *out_count, size_t *out_capacity)
{
    t_cblc_function method_function;
    size_t saved_data_count;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    std::memset(&method_function, 0, sizeof(method_function));
    method_function.return_kind = return_kind;
    if (return_kind == CBLC_FUNCTION_RETURN_INT)
        ft_strlcpy(method_function.return_cobol_name, "CBLC-THIS-RET",
            sizeof(method_function.return_cobol_name));
    saved_data_count = unit->data_count;
    saved_access_type = g_cblc_member_access_type;
    if (cblc_bind_lifecycle_scope(unit, type, &saved_data_count) != FT_SUCCESS)
        return (FT_FAILURE);
    g_cblc_member_access_type = type;
    status = cblc_parse_statement_block(cursor, unit, &method_function, 1);
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    g_cblc_member_access_type = saved_access_type;
    if (status != FT_SUCCESS || (return_kind == CBLC_FUNCTION_RETURN_INT && !method_function.saw_return))
    {
        if (method_function.statements)
            cma_free(method_function.statements);
        if (method_function.local_destructor_targets)
            cma_free(method_function.local_destructor_targets);
        return (FT_FAILURE);
    }
    *out_statements = method_function.statements;
    *out_count = method_function.statement_count;
    *out_capacity = method_function.statement_capacity;
    if (method_function.local_destructor_targets)
        cma_free(method_function.local_destructor_targets);
    return (FT_SUCCESS);
}

static int cblc_parse_method_definition(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_struct_type *type, t_cblc_member_visibility visibility)
{
    const char *start;
    t_cblc_method *method;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_function_return_kind return_kind;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;

    if (!cursor || !*cursor || !unit || !type || !type->is_class)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_match_keyword(cursor, "void"))
        return_kind = CBLC_FUNCTION_RETURN_VOID;
    else if (cblc_match_keyword(cursor, "int"))
        return_kind = CBLC_FUNCTION_RETURN_INT;
    else
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
    if (cblc_capture_method_body(cursor, unit, type, return_kind, &statements,
            &statement_count, &statement_capacity) != FT_SUCCESS)
    {
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
    method->visibility = visibility;
    method->statements = statements;
    method->statement_count = statement_count;
    method->statement_capacity = statement_capacity;
    type->method_count += 1;
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
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char instance_identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    start = *cursor;
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
    if (**cursor != ';')
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
    const char *function_identifier)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!unit || !function_identifier)
        return (NULL);
    if (std::snprintf(identifier, sizeof(identifier), "cblc_return_%s",
            function_identifier) < 0)
        return (NULL);
    return (cblc_add_generated_int_item(unit, identifier));
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
        ft_strlcpy(statement->source, arguments, sizeof(statement->source));
    else
        statement->source[0] = '\0';
    return (FT_SUCCESS);
}

static int cblc_add_temp_alias_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, const char *struct_type_name,
    int is_const)
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
    item->kind = kind;
    item->is_const = is_const;
    item->is_active = 1;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_add_named_data_item(t_cblc_translation_unit *unit, const char *source_name,
    const char *cobol_name, t_cblc_data_kind kind, size_t length, const char *struct_type_name,
    const char *owner_function_name, int is_function_local, int is_alias, int is_const)
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
        return (cblc_find_struct_type(unit, item->declared_type_name));
    if (item->kind == CBLC_DATA_KIND_STRUCT)
        return (cblc_find_struct_type(unit, item->struct_type_name));
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
    size_t index;

    if (!unit || !type || !saved_data_count)
        return (FT_FAILURE);
    *saved_data_count = unit->data_count;
    root_source = "this";
    root_cobol = "CBLC-THIS";
    if (cblc_add_temp_alias_item(unit, root_source, root_cobol, CBLC_DATA_KIND_STRUCT, 0,
            type->source_name, 0) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_add_struct_instance_field_items(unit, type, root_source, root_cobol,
            NULL, 0, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    alias_start = *saved_data_count + 1;
    index = alias_start;
    while (index < unit->data_count)
    {
        const char *source_name;

        source_name = unit->data_items[index].source_name;
        if (std::strncmp(source_name, "this.", 5) == 0)
        {
            if (cblc_add_temp_alias_item(unit, source_name + 5, unit->data_items[index].cobol_name,
                    unit->data_items[index].kind, unit->data_items[index].length,
                    unit->data_items[index].struct_type_name[0] != '\0'
                        ? unit->data_items[index].struct_type_name : NULL,
                    unit->data_items[index].is_const) != FT_SUCCESS)
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
        if (cblc_parse_method_call(cursor, unit, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_call(cursor, function) == FT_SUCCESS)
            continue ;
        if (cblc_parse_std_strcpy(cursor, unit, function) == FT_SUCCESS)
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
    const t_cblc_struct_type *type, t_cblc_statement **out_statements, size_t *out_count,
    size_t *out_capacity)
{
    t_cblc_function constructor_function;
    t_cblc_function *field_functions;
    int *seen_fields;
    size_t saved_data_count;
    size_t index;
    const t_cblc_struct_type *saved_access_type;
    int status;

    if (!cursor || !*cursor || !unit || !type || !out_statements || !out_count || !out_capacity)
        return (FT_FAILURE);
    std::memset(&constructor_function, 0, sizeof(constructor_function));
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
                if (cblc_append_statement(&constructor_function, CBLC_STATEMENT_DEFAULT_CONSTRUCT,
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
            if (cblc_append_existing_statement(&constructor_function,
                    &field_functions[index].statements[statement_index]) != FT_SUCCESS)
            {
                status = FT_FAILURE;
                goto cleanup;
            }
            statement_index += 1;
        }
        index += 1;
    }
    status = cblc_parse_statement_block(cursor, unit, &constructor_function, 0);
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
    *out_statements = constructor_function.statements;
    *out_count = constructor_function.statement_count;
    *out_capacity = constructor_function.statement_capacity;
    constructor_function.statements = NULL;
    status = FT_SUCCESS;
cleanup:
    g_cblc_constructor_parse_state.type = NULL;
    g_cblc_constructor_parse_state.initialized_fields = NULL;
    g_cblc_constructor_parse_state.active = 0;
    g_cblc_member_access_type = saved_access_type;
    cblc_unbind_lifecycle_scope(unit, saved_data_count);
    if (constructor_function.statements)
        cma_free(constructor_function.statements);
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
    destruct_targets = static_cast<char (*)[TRANSPILE_IDENTIFIER_MAX]>(cma_calloc(index,
            sizeof(*destruct_targets)));
    if (!construct_targets || !destruct_targets)
    {
        if (construct_targets)
            cma_free(construct_targets);
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
            cma_free(destruct_targets);
            return (FT_FAILURE);
        }
        if (cblc_item_requires_default_construct(unit, item))
        {
            ft_strlcpy(construct_targets[construct_count], item->cobol_name, TRANSPILE_IDENTIFIER_MAX);
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
        cma_free(destruct_targets);
        return (FT_SUCCESS);
    }
    new_statements = static_cast<t_cblc_statement *>(cma_calloc(new_count, sizeof(*new_statements)));
    if (!new_statements)
    {
        cma_free(construct_targets);
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
        cma_free(destruct_targets);
        return (FT_FAILURE);
    }
    if (function->statements)
        cma_free(function->statements);
    function->statements = new_statements;
    function->statement_count = new_count;
    function->statement_capacity = new_count;
    cma_free(construct_targets);
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
    if (cblc_add_named_data_item(unit, alias_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0,
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
    size_t length;

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
    if (**cursor == '[')
    {
        *cursor += 1;
        cblc_skip_whitespace(cursor);
        if (cblc_parse_numeric_literal(cursor, &length) != FT_SUCCESS)
        {
            *cursor = start;
            return (FT_FAILURE);
        }
        cblc_skip_whitespace(cursor);
        if (**cursor != ']')
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
            length, NULL, function->source_name, 1, 0, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_add_named_data_item(unit, local_identifier, actual_cobol_name, CBLC_DATA_KIND_STRING,
            length, NULL, NULL, 0, 1, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
    if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
            NULL, 0) != FT_SUCCESS)
        return (FT_FAILURE);
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
    char type_identifier[TRANSPILE_IDENTIFIER_MAX];
    char local_identifier[TRANSPILE_IDENTIFIER_MAX];
    char actual_source_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_name[TRANSPILE_IDENTIFIER_MAX];
    char actual_cobol_source[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
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
    if (cblc_add_named_data_item(unit, actual_source_name, actual_cobol_name, CBLC_DATA_KIND_STRUCT, 0,
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
    if (cblc_type_requires_default_construct(unit, type))
    {
        if (cblc_append_statement(function, CBLC_STATEMENT_DEFAULT_CONSTRUCT, actual_cobol_name,
                NULL, 0) != FT_SUCCESS)
            return (FT_FAILURE);
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

                if (cblc_parse_data_reference(cursor, unit, &item, &is_length_reference)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    char length_name[TRANSPILE_IDENTIFIER_MAX];

                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                    if (cblc_expression_append(buffer, buffer_size, length_name) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, item->cobol_name) != FT_SUCCESS)
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
    while (**cursor != '\0' && **cursor != ')')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '\0' || **cursor == ')')
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

                if (cblc_parse_data_reference(cursor, unit, &item, &is_length_reference)
                    != FT_SUCCESS)
                    return (FT_FAILURE);
                if (is_length_reference)
                {
                    char length_name[TRANSPILE_IDENTIFIER_MAX];

                    cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                        sizeof(length_name));
                    if (cblc_expression_append(buffer, buffer_size, length_name) != FT_SUCCESS)
                        return (FT_FAILURE);
                    expect_operand = 0;
                    continue ;
                }
                if (item->kind != CBLC_DATA_KIND_INT)
                    return (FT_FAILURE);
                if (cblc_expression_append(buffer, buffer_size, item->cobol_name) != FT_SUCCESS)
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
    const t_cblc_function *called_function;
    t_cblc_statement *statement;

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
    called_function = cblc_find_function_in_unit(unit, identifier);
    if (!called_function || called_function->return_kind != CBLC_FUNCTION_RETURN_INT)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (called_function->return_cobol_name[0] == '\0')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    if (cblc_append_statement(function, CBLC_STATEMENT_CALL_ASSIGN,
            target_item->cobol_name, called_function->return_cobol_name, 0)
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
    statement->call_is_external = 0;
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
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;

    if (!cursor || !*cursor || !unit || !function || !target_item)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_method_receiver(cursor, unit, &receiver_item, method_name,
            sizeof(method_name)) != FT_SUCCESS)
        return (FT_FAILURE);
    receiver_type = cblc_find_receiver_type(unit, receiver_item);
    method = cblc_find_method_on_type(receiver_type, method_name);
    if (!method || method->return_kind != CBLC_FUNCTION_RETURN_INT || target_item->kind != CBLC_DATA_KIND_INT)
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
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL_ASSIGN,
            target_item->cobol_name, receiver_item->cobol_name, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
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
    t_cblc_data_item *target_item;
    int target_is_length_reference;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    if (cblc_parse_data_reference(cursor, unit, &target_item, &target_is_length_reference)
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

        cblc_build_string_component_name(target_item->cobol_name, "-BUF", buffer_target,
            sizeof(buffer_target));
        cblc_build_string_component_name(target_item->cobol_name, "-LEN", length_target,
            sizeof(length_target));
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
        if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_STRING)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_item->cobol_name, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_item->cobol_name, cobol_source, 0));
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
                    target_item->cobol_name, cobol_source, 1));
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
                    target_item->cobol_name, cobol_source, 1));
        }
        if (cblc_parse_data_reference(cursor, unit, &source_item, NULL) != FT_SUCCESS)
            return (FT_FAILURE);
        if (!source_item || source_item->kind != CBLC_DATA_KIND_CHAR)
            return (FT_FAILURE);
        ft_strlcpy(cobol_source, source_item->cobol_name, sizeof(cobol_source));
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_ASSIGNMENT,
                target_item->cobol_name, cobol_source, 0));
    }
    if (target_item->kind == CBLC_DATA_KIND_INT)
    {
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
        if (cblc_parse_std_strlen_assignment(cursor, unit, function, target_item)
            == FT_SUCCESS)
            return (FT_SUCCESS);
        if (cblc_parse_numeric_expression(cursor, unit, expression_buffer,
                sizeof(expression_buffer)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_skip_whitespace(cursor);
        if (**cursor != ';')
            return (FT_FAILURE);
        *cursor += 1;
        return (cblc_append_statement(function, CBLC_STATEMENT_COMPUTE,
                target_item->cobol_name, expression_buffer, 0));
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

        if (cblc_parse_data_reference(cursor, unit, &item, &is_length_reference) != FT_SUCCESS)
            return (FT_FAILURE);
        if (is_length_reference)
        {
            cblc_build_string_component_name(item->cobol_name, "-LEN", cobol_argument,
                sizeof(cobol_argument));
            is_literal = 0;
        }
        else
        {
            if (item->kind == CBLC_DATA_KIND_STRING)
            {
                char buffer_name[TRANSPILE_IDENTIFIER_MAX];
                char length_name[TRANSPILE_IDENTIFIER_MAX];

                cblc_build_string_component_name(item->cobol_name, "-BUF", buffer_name,
                    sizeof(buffer_name));
                cblc_build_string_component_name(item->cobol_name, "-LEN", length_name,
                    sizeof(length_name));
                if (std::snprintf(cobol_argument, sizeof(cobol_argument), "%s(1:%s)",
                        buffer_name, length_name) < 0)
                    return (FT_FAILURE);
            }
            else
                ft_strlcpy(cobol_argument, item->cobol_name, sizeof(cobol_argument));
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

static int cblc_parse_call(const char **cursor, t_cblc_function *function)
{
    const char *start;
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;

    if (!cursor || !*cursor || !function)
        return (FT_FAILURE);
    start = *cursor;
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '(')
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    *cursor += 1;
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
    return (FT_SUCCESS);
}

static int cblc_parse_method_call(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    const char *start;
    t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *receiver_type;
    const t_cblc_method *method;
    char method_name[TRANSPILE_IDENTIFIER_MAX];
    t_cblc_statement *statement;

    if (!cursor || !*cursor || !unit || !function)
        return (FT_FAILURE);
    start = *cursor;
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
    if (cblc_append_statement(function, CBLC_STATEMENT_METHOD_CALL,
            receiver_item->cobol_name, NULL, 0) != FT_SUCCESS)
    {
        *cursor = start;
        return (FT_FAILURE);
    }
    statement = &function->statements[function->statement_count - 1];
    ft_strlcpy(statement->call_identifier, method_name, sizeof(statement->call_identifier));
    return (FT_SUCCESS);
}

static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit,
    t_cblc_function *function)
{
    char expression[TRANSPILE_STATEMENT_TEXT_MAX];

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
    if (cblc_parse_numeric_expression(cursor, unit, expression,
            sizeof(expression)) != FT_SUCCESS)
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
    size_t index;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "function"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_match_keyword(cursor, "void"))
        return_kind = CBLC_FUNCTION_RETURN_VOID;
    else if (cblc_match_keyword(cursor, "int"))
        return_kind = CBLC_FUNCTION_RETURN_INT;
    else
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
    function->saw_return = 0;
    function->return_kind = return_kind;
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
    return_item = NULL;
    if (return_kind != CBLC_FUNCTION_RETURN_VOID)
    {
        return_item = cblc_create_return_item(unit, identifier);
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
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != '{')
        return (FT_FAILURE);
    *cursor += 1;
    *cursor -= 1;
    return (cblc_parse_statement_block(cursor, unit, function, 1));
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
            if (unit->struct_types[index].constructor_statements)
                cma_free(unit->struct_types[index].constructor_statements);
            if (unit->struct_types[index].destructor_statements)
                cma_free(unit->struct_types[index].destructor_statements);
            unit->struct_types[index].fields = NULL;
            unit->struct_types[index].field_count = 0;
            unit->struct_types[index].field_capacity = 0;
            unit->struct_types[index].methods = NULL;
            unit->struct_types[index].method_count = 0;
            unit->struct_types[index].method_capacity = 0;
            unit->struct_types[index].constructor_statements = NULL;
            unit->struct_types[index].constructor_statement_count = 0;
            unit->struct_types[index].constructor_statement_capacity = 0;
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
        if (cblc_match_keyword(&cursor, "function"))
        {
            cursor -= std::strlen("function");
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

        if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                substituted.source, substituted.target) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_DISPLAY)
    {
        char line[256];

        if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                substituted.source) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_COMPUTE)
    {
        char line[256];

        if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                substituted.target, substituted.source) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
        if (substituted.call_is_external)
        {
            if (substituted.source[0] != '\0')
            {
                if (std::snprintf(line, sizeof(line), "           CALL '%s' USING %s",
                        cobol_name, substituted.source) < 0)
                    return (FT_FAILURE);
            }
            else if (std::snprintf(line, sizeof(line), "           CALL '%s'", cobol_name) < 0)
                return (FT_FAILURE);
        }
        else if (std::snprintf(line, sizeof(line), "           PERFORM %s", cobol_name) < 0)
            return (FT_FAILURE);
        return (append_statement_line(line));
    }
    if (substituted.type == CBLC_STATEMENT_CALL_ASSIGN)
    {
        char cobol_name[TRANSPILE_IDENTIFIER_MAX];
        char line[256];

        cblc_identifier_to_cobol(substituted.call_identifier, cobol_name, sizeof(cobol_name));
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
    }
    return (cblc_emit_method_body(unit, receiver, method, assign_target, builder));
}

static int cblc_emit_lifecycle_statement(const t_cblc_translation_unit *unit,
    const t_cblc_statement *statement, t_cobol_text_builder *builder)
{
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
        char line[256];

        if (std::snprintf(line, sizeof(line), "           MOVE 0 TO %s-LEN.",
                item->cobol_name) < 0)
            return (FT_FAILURE);
        if (cobol_text_builder_append_line(builder, line) != FT_SUCCESS)
            return (FT_FAILURE);
        if (std::snprintf(line, sizeof(line), "           MOVE SPACES TO %s-BUF.",
                item->cobol_name) < 0)
            return (FT_FAILURE);
        return (cobol_text_builder_append_line(builder, line));
    }
    if (item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_FAILURE);
    type = cblc_find_struct_type(unit, item->struct_type_name);
    if (!type)
        return (FT_FAILURE);
    if (statement->type == CBLC_STATEMENT_DEFAULT_CONSTRUCT
        && type->constructor_statement_count > 0)
    {
        body = type->constructor_statements;
        count = type->constructor_statement_count;
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
            if (std::snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    statement->source, statement->target) < 0)
                return (FT_FAILURE);
        }
        else if (statement->type == CBLC_STATEMENT_DISPLAY)
        {
            if (std::snprintf(line, sizeof(line), "           DISPLAY %s",
                    statement->source) < 0)
                return (FT_FAILURE);
        }
        else if (statement->type == CBLC_STATEMENT_COMPUTE)
        {
            if (std::snprintf(line, sizeof(line), "           COMPUTE %s = %s",
                    statement->target, statement->source) < 0)
                return (FT_FAILURE);
        }
        else if (statement->type == CBLC_STATEMENT_CALL)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
            if (statement->call_is_external)
            {
                if (statement->source[0] != '\0')
                {
                    if (std::snprintf(line, sizeof(line),
                            "           CALL '%s' USING %s", cobol_name,
                            statement->source) < 0)
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
                if (std::snprintf(line, sizeof(line), "           PERFORM %s",
                        cobol_name) < 0)
                    return (FT_FAILURE);
            }
        }
        else if (statement->type == CBLC_STATEMENT_CALL_ASSIGN)
        {
            char cobol_name[TRANSPILE_IDENTIFIER_MAX];

            cblc_identifier_to_cobol(statement->call_identifier, cobol_name,
                sizeof(cobol_name));
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
        if (cobol_text_builder_append_line(builder, "           STOP RUN.") != FT_SUCCESS)
            return (FT_FAILURE);
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
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s.",
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
            if (std::snprintf(line, sizeof(line), "       %02zu %s-%s PIC S9(9).",
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

int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;
    size_t entry_index;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
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
        if (unit->data_items[index].is_alias || std::strchr(unit->data_items[index].source_name, '.'))
        {
            index += 1;
            continue ;
        }
        if (unit->data_items[index].kind == CBLC_DATA_KIND_CHAR)
        {
            if (unit->data_items[index].is_const
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
            if (unit->data_items[index].is_const
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
            if (std::snprintf(line, sizeof(line), "       01 %s.",
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
    if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
        goto cleanup;
    entry_index = unit->entry_function_index;
    if (unit->function_count > 0)
    {
        if (entry_index == static_cast<size_t>(-1) || entry_index >= unit->function_count)
            entry_index = 0;
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
                    statement->call_is_external = 0;
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

            return_mode = TRANSPILE_FUNCTION_RETURN_VOID;
            if (unit->functions[index].return_kind != CBLC_FUNCTION_RETURN_VOID)
                return_mode = TRANSPILE_FUNCTION_RETURN_VALUE;
            if (transpiler_context_register_function(context, module_name,
                    unit->functions[index].source_name, return_mode,
                    TRANSPILE_SYMBOL_PUBLIC) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}
