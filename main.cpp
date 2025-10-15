#include <climits>
#include <filesystem>
#include <system_error>

#include "cblc_formatter.hpp"
#include "parser.hpp"
#include "runtime_file.hpp"
#include "transpiler_cli.hpp"
#include "transpiler_cobol_reverse.hpp"
#include "transpiler_logging.hpp"
#include "transpiler_pipeline.hpp"
#include "transpiler_semantics.hpp"
#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int pipeline_emit_error(t_transpiler_context *context, const char *message)
{
    if (!context || !message)
        return (FT_FAILURE);
    if (transpiler_logging_emit(context, TRANSPILE_SEVERITY_ERROR, FT_FAILURE, message) != FT_SUCCESS)
        return (FT_FAILURE);
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int pipeline_read_file(const char *path, char **out_text)
{
    t_runtime_file file;
    char stack_buffer[1024];
    char *buffer;
    size_t capacity;
    size_t length;
    size_t bytes_read;
    int status;

    if (!path || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    runtime_file_init(&file);
    if (runtime_file_open_read(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    capacity = 1024;
    buffer = static_cast<char *>(cma_calloc(capacity, sizeof(char)));
    if (!buffer)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    length = 0;
    status = FT_FAILURE;
    while (1)
    {
        if (runtime_file_read(&file, stack_buffer, sizeof(stack_buffer), &bytes_read) != FT_SUCCESS)
            break ;
        if (bytes_read == 0)
        {
            status = FT_SUCCESS;
            break ;
        }
        while (length + bytes_read + 1 > capacity)
        {
            size_t new_capacity;
            char *new_buffer;

            if (capacity >= SIZE_MAX / 2)
                goto cleanup;
            new_capacity = capacity * 2;
            new_buffer = static_cast<char *>(cma_calloc(new_capacity, sizeof(char)));
            if (!new_buffer)
                goto cleanup;
            if (length > 0)
                ft_memcpy(new_buffer, buffer, length);
            cma_free(buffer);
            buffer = new_buffer;
            capacity = new_capacity;
        }
        if (bytes_read > 0)
        {
            ft_memcpy(buffer + length, stack_buffer, bytes_read);
            length += bytes_read;
            buffer[length] = '\0';
        }
    }
    if (status == FT_SUCCESS)
    {
        *out_text = buffer;
        buffer = NULL;
    }
cleanup:
    runtime_file_close(&file);
    if (buffer)
        cma_free(buffer);
    return (status);
}

static int pipeline_prepare_output_directory(const char *path)
{
    std::error_code error;
    std::filesystem::path output_path;
    std::filesystem::path parent_directory;

    if (!path)
        return (FT_FAILURE);
    output_path = std::filesystem::path(path);
    parent_directory = output_path.parent_path();
    if (parent_directory.empty())
        return (FT_SUCCESS);
    std::filesystem::create_directories(parent_directory, error);
    if (error)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_write_file(const char *path, const char *text)
{
    t_runtime_file file;
    size_t length;

    if (!path || !text)
        return (FT_FAILURE);
    if (pipeline_prepare_output_directory(path) != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_file_init(&file);
    if (runtime_file_open_write(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (runtime_file_write(&file, text, length) != FT_SUCCESS)
    {
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (runtime_file_close(&file) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int pipeline_resolve_output_path(const t_transpiler_context *context, const char *target_path,
    char *buffer, size_t buffer_size)
{
    const char *directory;
    const char *filename;
    const char *separator;
    size_t length;

    if (!context || !target_path || !buffer || buffer_size == 0)
        return (FT_FAILURE);
    directory = context->output_directory;
    if (directory && directory[0] != '\0')
    {
        filename = target_path;
        separator = ft_strrchr(target_path, '/');
        if (!separator)
            separator = ft_strrchr(target_path, '\\');
        if (separator && separator[1] != '\0')
            filename = separator + 1;
        if (pf_snprintf(buffer, buffer_size, "%s/%s", directory, filename) < 0)
            return (FT_FAILURE);
        length = ft_strlen(buffer);
        if (length + 1 > buffer_size)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    length = ft_strlen(target_path);
    if (length + 1 > buffer_size)
        return (FT_FAILURE);
    ft_strlcpy(buffer, target_path, buffer_size);
    return (FT_SUCCESS);
}

static int pipeline_format_cblc(const char *input, t_transpiler_format_mode mode, char **out_text)
{
    if (!input || !out_text)
        return (FT_FAILURE);
    return (cblc_formatter_format(input, mode, out_text));
}

typedef struct s_cblc_data_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
}   t_cblc_data_item;

typedef enum e_cblc_statement_type
{
    CBLC_STATEMENT_ASSIGNMENT,
    CBLC_STATEMENT_DISPLAY
}   t_cblc_statement_type;

typedef struct s_cblc_statement
{
    t_cblc_statement_type type;
    char target[TRANSPILE_IDENTIFIER_MAX];
    char source[TRANSPILE_IDENTIFIER_MAX];
    int is_literal;
}   t_cblc_statement;

typedef struct s_cblc_translation_unit
{
    t_cblc_data_item *data_items;
    size_t data_count;
    size_t data_capacity;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    char program_name[TRANSPILE_IDENTIFIER_MAX];
    int saw_return;
}   t_cblc_translation_unit;

typedef struct s_cobol_text_builder
{
    char *data;
    size_t length;
    size_t capacity;
}   t_cobol_text_builder;

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
        ft_memcpy(new_data, builder->data, builder->length);
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
    ft_memcpy(builder->data + builder->length, text, length);
    builder->length += length;
    builder->data[builder->length] = '\0';
    return (FT_SUCCESS);
}

static int cobol_text_builder_append_string(t_cobol_text_builder *builder, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (cobol_text_builder_append_span(builder, text, ft_strlen(text)));
}

static int cobol_text_builder_append_line(t_cobol_text_builder *builder, const char *text)
{
    if (cobol_text_builder_append_string(builder, text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cobol_text_builder_append_string(builder, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static void cblc_translation_unit_init(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->statements = NULL;
    unit->statement_count = 0;
    unit->statement_capacity = 0;
    unit->program_name[0] = '\0';
    unit->saw_return = 0;
}

static void cblc_translation_unit_dispose(t_cblc_translation_unit *unit)
{
    if (!unit)
        return ;
    if (unit->data_items)
        cma_free(unit->data_items);
    if (unit->statements)
        cma_free(unit->statements);
    unit->data_items = NULL;
    unit->data_count = 0;
    unit->data_capacity = 0;
    unit->statements = NULL;
    unit->statement_count = 0;
    unit->statement_capacity = 0;
    unit->program_name[0] = '\0';
    unit->saw_return = 0;
}

static int cblc_translation_unit_ensure_data_capacity(t_cblc_translation_unit *unit, size_t desired_capacity)
{
    t_cblc_data_item *items;

    if (!unit)
        return (FT_FAILURE);
    if (unit->data_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    items = static_cast<t_cblc_data_item *>(cma_calloc(desired_capacity, sizeof(t_cblc_data_item)));
    if (!items)
        return (FT_FAILURE);
    if (unit->data_items && unit->data_count > 0)
        ft_memcpy(items, unit->data_items, unit->data_count * sizeof(t_cblc_data_item));
    if (unit->data_items)
        cma_free(unit->data_items);
    unit->data_items = items;
    unit->data_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_translation_unit_ensure_statement_capacity(t_cblc_translation_unit *unit, size_t desired_capacity)
{
    t_cblc_statement *statements;

    if (!unit)
        return (FT_FAILURE);
    if (unit->statement_capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    statements = static_cast<t_cblc_statement *>(cma_calloc(desired_capacity, sizeof(t_cblc_statement)));
    if (!statements)
        return (FT_FAILURE);
    if (unit->statements && unit->statement_count > 0)
        ft_memcpy(statements, unit->statements, unit->statement_count * sizeof(t_cblc_statement));
    if (unit->statements)
        cma_free(unit->statements);
    unit->statements = statements;
    unit->statement_capacity = desired_capacity;
    return (FT_SUCCESS);
}

static void cblc_identifier_to_cobol(const char *identifier, char *buffer, size_t buffer_size)
{
    size_t index;
    size_t write_index;

    if (!buffer || buffer_size == 0)
        return ;
    buffer[0] = '\0';
    if (!identifier)
        return ;
    index = 0;
    write_index = 0;
    while (identifier[index] != '\0' && write_index + 1 < buffer_size)
    {
        char character;

        character = identifier[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        else if (character == '_')
            character = '-';
        buffer[write_index] = character;
        write_index += 1;
        index += 1;
    }
    buffer[write_index] = '\0';
}

static void cblc_skip_whitespace(const char **cursor)
{
    if (!cursor || !*cursor)
        return ;
    while (**cursor == ' ' || **cursor == '\t' || **cursor == '\n' || **cursor == '\r')
        *cursor += 1;
}

static int cblc_is_identifier_start(char character)
{
    if ((character >= 'a' && character <= 'z')
        || (character >= 'A' && character <= 'Z') || character == '_')
        return (1);
    return (0);
}

static int cblc_is_identifier_part(char character)
{
    if (cblc_is_identifier_start(character))
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
    if (!cblc_is_identifier_start(**cursor))
        return (FT_FAILURE);
    length = 0;
    while (cblc_is_identifier_part((*cursor)[length]) && length + 1 < buffer_size)
    {
        buffer[length] = (*cursor)[length];
        length += 1;
    }
    buffer[length] = '\0';
    *cursor += length;
    return (FT_SUCCESS);
}

static int cblc_match_keyword(const char **cursor, const char *keyword)
{
    size_t length;

    if (!cursor || !*cursor || !keyword)
        return (0);
    length = ft_strlen(keyword);
    if (ft_strncmp(*cursor, keyword, length) != 0)
        return (0);
    if (cblc_is_identifier_part((*cursor)[length]))
        return (0);
    *cursor += length;
    return (1);
}

static int cblc_parse_number(const char **cursor, size_t *out_value)
{
    size_t value;
    int saw_digit;

    if (!cursor || !*cursor || !out_value)
        return (FT_FAILURE);
    value = 0;
    saw_digit = 0;
    while (**cursor >= '0' && **cursor <= '9')
    {
        value = value * 10 + static_cast<size_t>(**cursor - '0');
        *cursor += 1;
        saw_digit = 1;
    }
    if (!saw_digit)
        return (FT_FAILURE);
    *out_value = value;
    return (FT_SUCCESS);
}

static int cblc_parse_char_declaration(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
    t_cblc_data_item *item;

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "char"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '[')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (cblc_parse_number(cursor, &length) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ']')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    if (unit->data_count >= unit->data_capacity)
    {
        if (cblc_translation_unit_ensure_data_capacity(unit,
                unit->data_capacity == 0 ? 4 : unit->data_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &unit->data_items[unit->data_count];
    cblc_identifier_to_cobol(identifier, item->name, sizeof(item->name));
    item->length = length;
    unit->data_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_string_literal(const char **cursor, char *buffer, size_t buffer_size)
{
    size_t index;

    if (!cursor || !*cursor || !buffer || buffer_size < 3)
        return (FT_FAILURE);
    if (**cursor != '"')
        return (FT_FAILURE);
    *cursor += 1;
    index = 0;
    while (**cursor != '\0' && **cursor != '"')
    {
        char character;

        character = **cursor;
        if (character == '\\')
        {
            *cursor += 1;
            if (**cursor == '\0')
                return (FT_FAILURE);
            character = **cursor;
        }
        if (index + 2 >= buffer_size)
            return (FT_FAILURE);
        buffer[index] = character;
        index += 1;
        *cursor += 1;
    }
    if (**cursor != '"')
        return (FT_FAILURE);
    *cursor += 1;
    buffer[index] = '\0';
    return (FT_SUCCESS);
}

static int cblc_append_statement(t_cblc_translation_unit *unit, t_cblc_statement_type type,
    const char *target, const char *source, int is_literal)
{
    t_cblc_statement *statement;

    if (!unit || !source)
        return (FT_FAILURE);
    if (type == CBLC_STATEMENT_ASSIGNMENT && !target)
        return (FT_FAILURE);
    if (unit->statement_count >= unit->statement_capacity)
    {
        if (cblc_translation_unit_ensure_statement_capacity(unit,
                unit->statement_capacity == 0 ? 4 : unit->statement_capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    statement = &unit->statements[unit->statement_count];
    statement->type = type;
    statement->target[0] = '\0';
    if (target)
        ft_strlcpy(statement->target, target, sizeof(statement->target));
    ft_strlcpy(statement->source, source, sizeof(statement->source));
    statement->is_literal = is_literal;
    unit->statement_count += 1;
    return (FT_SUCCESS);
}

static int cblc_parse_assignment(const char **cursor, t_cblc_translation_unit *unit)
{
    char target_identifier[TRANSPILE_IDENTIFIER_MAX];
    char source_identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char cobol_target[TRANSPILE_IDENTIFIER_MAX];
    char cobol_source[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (cblc_parse_identifier(cursor, target_identifier, sizeof(target_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != '=')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
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
        cblc_identifier_to_cobol(target_identifier, cobol_target, sizeof(cobol_target));
        return (cblc_append_statement(unit, CBLC_STATEMENT_ASSIGNMENT, cobol_target,
                cobol_source, 1));
    }
    if (cblc_parse_identifier(cursor, source_identifier, sizeof(source_identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_identifier_to_cobol(target_identifier, cobol_target, sizeof(cobol_target));
    cblc_identifier_to_cobol(source_identifier, cobol_source, sizeof(cobol_source));
    return (cblc_append_statement(unit, CBLC_STATEMENT_ASSIGNMENT, cobol_target,
            cobol_source, 0));
}

static int cblc_parse_display(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    char literal_buffer[TRANSPILE_IDENTIFIER_MAX];
    char cobol_argument[TRANSPILE_IDENTIFIER_MAX];
    int is_literal;

    if (!cursor || !*cursor || !unit)
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
        if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
            return (FT_FAILURE);
        cblc_identifier_to_cobol(identifier, cobol_argument, sizeof(cobol_argument));
        is_literal = 0;
    }
    cblc_skip_whitespace(cursor);
    if (**cursor != ')')
        return (FT_FAILURE);
    *cursor += 1;
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    return (cblc_append_statement(unit, CBLC_STATEMENT_DISPLAY, NULL, cobol_argument, is_literal));
}

static int cblc_parse_return(const char **cursor, t_cblc_translation_unit *unit)
{
    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "return"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (**cursor != ';')
        return (FT_FAILURE);
    *cursor += 1;
    unit->saw_return = 1;
    return (FT_SUCCESS);
}

static int cblc_parse_function(const char **cursor, t_cblc_translation_unit *unit)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!cursor || !*cursor || !unit)
        return (FT_FAILURE);
    if (!cblc_match_keyword(cursor, "function"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (!cblc_match_keyword(cursor, "void"))
        return (FT_FAILURE);
    cblc_skip_whitespace(cursor);
    if (cblc_parse_identifier(cursor, identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (unit->program_name[0] == '\0')
        cblc_identifier_to_cobol(identifier, unit->program_name, sizeof(unit->program_name));
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
    while (**cursor != '\0')
    {
        cblc_skip_whitespace(cursor);
        if (**cursor == '}')
        {
            *cursor += 1;
            return (FT_SUCCESS);
        }
        if (cblc_match_keyword(cursor, "return"))
        {
            (*cursor) -= ft_strlen("return");
            if (cblc_parse_return(cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(cursor, "display"))
        {
            (*cursor) -= ft_strlen("display");
            if (cblc_parse_display(cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_parse_assignment(cursor, unit) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_FAILURE);
}

static int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit)
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
        if (cblc_match_keyword(&cursor, "function"))
        {
            cursor -= ft_strlen("function");
            if (cblc_parse_function(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        if (cblc_match_keyword(&cursor, "char"))
        {
            cursor -= ft_strlen("char");
            if (cblc_parse_char_declaration(&cursor, unit) != FT_SUCCESS)
                return (FT_FAILURE);
            continue ;
        }
        return (FT_FAILURE);
    }
    if (unit->program_name[0] == '\0')
        ft_strlcpy(unit->program_name, "MAIN", sizeof(unit->program_name));
    return (FT_SUCCESS);
}

static int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text)
{
    t_cobol_text_builder builder;
    char line[256];
    size_t index;

    if (!unit || !out_text)
        return (FT_FAILURE);
    *out_text = NULL;
    cobol_text_builder_init(&builder);
    if (cobol_text_builder_append_line(&builder, "       IDENTIFICATION DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (pf_snprintf(line, sizeof(line), "       PROGRAM-ID. %s.", unit->program_name) < 0)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       DATA DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "       WORKING-STORAGE SECTION.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->data_count)
    {
        if (pf_snprintf(line, sizeof(line), "       01 %s PIC X(%zu).",
                unit->data_items[index].name, unit->data_items[index].length) < 0)
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cobol_text_builder_append_line(&builder, "       PROCEDURE DIVISION.") != FT_SUCCESS)
        goto cleanup;
    if (cobol_text_builder_append_line(&builder, "MAIN.") != FT_SUCCESS)
        goto cleanup;
    index = 0;
    while (index < unit->statement_count)
    {
        const t_cblc_statement *statement;

        statement = &unit->statements[index];
        if (statement->type == CBLC_STATEMENT_ASSIGNMENT)
        {
            if (pf_snprintf(line, sizeof(line), "           MOVE %s TO %s",
                    statement->source, statement->target) < 0)
                goto cleanup;
        }
        else if (statement->type == CBLC_STATEMENT_DISPLAY)
        {
            if (pf_snprintf(line, sizeof(line), "           DISPLAY %s",
                    statement->source) < 0)
                goto cleanup;
        }
        else
            goto cleanup;
        if (cobol_text_builder_append_line(&builder, line) != FT_SUCCESS)
            goto cleanup;
        index += 1;
    }
    if (cobol_text_builder_append_line(&builder, "           STOP RUN.") != FT_SUCCESS)
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

static int pipeline_convert_cobol_to_cblc(t_transpiler_context *context, const char *input_path, const char *output_path)
{
    char resolved_path[TRANSPILE_FILE_PATH_MAX];
    char *source_text;
    char *cblc_text;
    char *formatted_text;
    t_parser parser;
    t_ast_node *program;
    int status;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];

    if (!context || !input_path || !output_path)
        return (FT_FAILURE);
    source_text = NULL;
    cblc_text = NULL;
    formatted_text = NULL;
    program = NULL;
    status = FT_FAILURE;
    if (pipeline_read_file(input_path, &source_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_init(&parser, source_text);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        if (pf_snprintf(message, sizeof(message), "Failed to parse COBOL source '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    parser_dispose(&parser);
    transpiler_context_reset_unit_state(context);
    if (transpiler_semantics_analyze_program(context, program) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Semantic analysis failed for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (transpiler_cobol_program_to_cblc(context, program, &cblc_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to generate CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_format_cblc(cblc_text, context->format_mode, &formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Failed to format generated CBL-C for '%s'", input_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_resolve_output_path(context, output_path, resolved_path, sizeof(resolved_path)) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'", output_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    if (pipeline_write_file(resolved_path, formatted_text) != FT_SUCCESS)
    {
        if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'", resolved_path) >= 0)
            (void)pipeline_emit_error(context, message);
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    if (program)
        ast_node_destroy(program);
    if (formatted_text)
        cma_free(formatted_text);
    if (cblc_text)
        cma_free(cblc_text);
    if (source_text)
        cma_free(source_text);
    return (status);
}

static int pipeline_stage_cobol_to_cblc(t_transpiler_context *context, void *user_data)
{
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    index = 0;
    while (index < context->source_count)
    {
        if (pipeline_convert_cobol_to_cblc(context, context->source_paths[index], context->target_paths[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static int pipeline_stage_cblc_to_cobol(t_transpiler_context *context, void *user_data)
{
    size_t index;

    (void)user_data;
    if (!context)
        return (FT_FAILURE);
    index = 0;
    while (index < context->source_count)
    {
        char resolved_path[TRANSPILE_FILE_PATH_MAX];
        char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
        char *source_text;
        char *cobol_text;
        t_cblc_translation_unit unit;

        source_text = NULL;
        cobol_text = NULL;
        cblc_translation_unit_init(&unit);
        if (pipeline_read_file(context->source_paths[index], &source_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to read input file '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (cblc_parse_translation_unit(source_text, &unit) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to parse CBL-C source '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (!unit.saw_return)
        {
            if (pf_snprintf(message, sizeof(message),
                    "CBL-C source '%s' is missing a terminating return;", context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (cblc_generate_cobol(&unit, &cobol_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to generate COBOL for '%s'",
                    context->source_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (pipeline_resolve_output_path(context, context->target_paths[index], resolved_path,
                sizeof(resolved_path)) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Unable to resolve output path for '%s'",
                    context->target_paths[index]) >= 0)
                (void)pipeline_emit_error(context, message);
            if (cobol_text)
                cma_free(cobol_text);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (pipeline_write_file(resolved_path, cobol_text) != FT_SUCCESS)
        {
            if (pf_snprintf(message, sizeof(message), "Failed to write output file '%s'",
                    resolved_path) >= 0)
                (void)pipeline_emit_error(context, message);
            if (cobol_text)
                cma_free(cobol_text);
            cblc_translation_unit_dispose(&unit);
            if (source_text)
                cma_free(source_text);
            return (FT_FAILURE);
        }
        if (cobol_text)
            cma_free(cobol_text);
        cblc_translation_unit_dispose(&unit);
        if (source_text)
            cma_free(source_text);
        index += 1;
    }
    return (FT_SUCCESS);
}

int main(int argc, const char **argv)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    t_transpiler_cli_options options;
    int status;

    if (transpiler_cli_parse(&options, argc, argv) != FT_SUCCESS)
    {
        transpiler_cli_print_usage();
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (options.show_help)
    {
        transpiler_cli_print_usage();
        transpiler_cli_options_dispose(&options);
        return (0);
    }
    if (transpiler_pipeline_init(&pipeline) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    if (transpiler_context_init(&context) != FT_SUCCESS)
    {
        transpiler_cli_options_dispose(&options);
        transpiler_pipeline_dispose(&pipeline);
        return (1);
    }
    if (transpiler_cli_apply(&options, &context) != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        transpiler_cli_options_dispose(&options);
        return (1);
    }
    status = FT_FAILURE;
    if (context.source_language == TRANSPILE_LANGUAGE_COBOL
        && context.target_language == TRANSPILE_LANGUAGE_CBL_C)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cobol-to-cblc",
                pipeline_stage_cobol_to_cblc, NULL) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
    }
    else if (context.source_language == TRANSPILE_LANGUAGE_CBL_C
        && context.target_language == TRANSPILE_LANGUAGE_COBOL)
    {
        if (transpiler_pipeline_add_stage(&pipeline, "cblc-to-cobol",
                pipeline_stage_cblc_to_cobol, NULL) != FT_SUCCESS)
        {
            status = FT_FAILURE;
            goto cleanup;
        }
    }
    else
    {
        (void)pipeline_emit_error(&context, "Unsupported translation direction");
        status = FT_FAILURE;
        goto cleanup;
    }
    status = transpiler_pipeline_execute(&pipeline, &context);
cleanup:
    transpiler_logging_flush(&context);
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    transpiler_cli_options_dispose(&options);
    if (status != FT_SUCCESS)
        return (1);
    return (0);
}
