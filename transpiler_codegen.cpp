#include "transpiler_codegen.hpp"

#include <cstdarg>

#include "libft/CMA/CMA.hpp"
#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

typedef struct s_transpiler_codegen_buffer
{
    char *data;
    size_t length;
    size_t capacity;
}   t_transpiler_codegen_buffer;

static void transpiler_codegen_buffer_init(t_transpiler_codegen_buffer *buffer)
{
    if (!buffer)
        return ;
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static void transpiler_codegen_buffer_dispose(t_transpiler_codegen_buffer *buffer)
{
    if (!buffer)
        return ;
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = NULL;
    buffer->length = 0;
    buffer->capacity = 0;
}

static int transpiler_codegen_buffer_reserve(t_transpiler_codegen_buffer *buffer, size_t desired_capacity)
{
    char *new_data;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 64)
        desired_capacity = 64;
    new_data = static_cast<char *>(cma_calloc(desired_capacity, sizeof(char)));
    if (!new_data)
        return (FT_FAILURE);
    if (buffer->data && buffer->length > 0)
        ft_memcpy(new_data, buffer->data, buffer->length);
    if (buffer->data)
        cma_free(buffer->data);
    buffer->data = new_data;
    buffer->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int transpiler_codegen_buffer_append_span(t_transpiler_codegen_buffer *buffer, const char *text, size_t length)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!text && length > 0)
        return (FT_FAILURE);
    if (length == 0)
        return (FT_SUCCESS);
    if (transpiler_codegen_buffer_reserve(buffer, buffer->length + length + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    ft_memcpy(buffer->data + buffer->length, text, length);
    buffer->length += length;
    buffer->data[buffer->length] = '\0';
    return (FT_SUCCESS);
}

static int transpiler_codegen_buffer_append_string(t_transpiler_codegen_buffer *buffer, const char *text)
{
    if (!text)
        return (FT_SUCCESS);
    return (transpiler_codegen_buffer_append_span(buffer, text, ft_strlen(text)));
}

static int transpiler_codegen_buffer_append_format(t_transpiler_codegen_buffer *buffer, const char *format, ...)
{
    va_list args;
    va_list copy;
    char stack_buffer[256];
    char *heap_buffer;
    int required_length;
    int status;

    if (!buffer)
        return (FT_FAILURE);
    if (!format)
        return (FT_FAILURE);
    va_start(args, format);
    va_copy(copy, args);
    required_length = pf_vsnprintf(stack_buffer, sizeof(stack_buffer), format, copy);
    va_end(copy);
    if (required_length < 0)
    {
        va_end(args);
        return (FT_FAILURE);
    }
    if (static_cast<size_t>(required_length) < sizeof(stack_buffer))
    {
        status = transpiler_codegen_buffer_append_span(buffer, stack_buffer,
            static_cast<size_t>(required_length));
        va_end(args);
        return (status);
    }
    heap_buffer = static_cast<char *>(cma_calloc(static_cast<size_t>(required_length) + 1, sizeof(char)));
    if (!heap_buffer)
    {
        va_end(args);
        return (FT_FAILURE);
    }
    if (pf_vsnprintf(heap_buffer, static_cast<size_t>(required_length) + 1, format, args) < 0)
    {
        va_end(args);
        cma_free(heap_buffer);
        return (FT_FAILURE);
    }
    va_end(args);
    status = transpiler_codegen_buffer_append_span(buffer, heap_buffer,
        static_cast<size_t>(required_length));
    cma_free(heap_buffer);
    return (status);
}

static void transpiler_codegen_uppercase_copy(const char *source, char *destination, size_t destination_size)
{
    size_t index;

    if (!destination || destination_size == 0)
        return ;
    destination[0] = '\0';
    if (!source)
        return ;
    index = 0;
    while (source[index] != '\0' && index + 1 < destination_size)
    {
        char character;

        character = source[index];
        if (character >= 'a' && character <= 'z')
            character = static_cast<char>(character - 'a' + 'A');
        destination[index] = character;
        index += 1;
    }
    destination[index] = '\0';
}

static size_t transpiler_codegen_determine_record_length(const t_transpiler_file_declaration *file)
{
    if (!file)
        return (256);
    if (file->explicit_record_length > 0)
        return (file->explicit_record_length);
    if (file->inferred_record_length > 0)
        return (file->inferred_record_length);
    return (256);
}

static const char *transpiler_codegen_organization_for_role(t_transpiler_file_role role)
{
    if (role == TRANSPILE_FILE_ROLE_DATA)
        return ("SEQUENTIAL");
    return ("LINE SEQUENTIAL");
}

static int transpiler_codegen_buffer_append_indent(t_transpiler_codegen_buffer *buffer, size_t indentation)
{
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    index = 0;
    while (index < indentation)
    {
        if (transpiler_codegen_buffer_append_string(buffer, "    ") != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

static const char *transpiler_codegen_condition_operator_text(t_transpiler_cobol_comparison_operator op)
{
    if (op == TRANSPILE_COBOL_COMPARISON_EQUALS)
        return ("=");
    if (op == TRANSPILE_COBOL_COMPARISON_NOT_EQUALS)
        return ("<>");
    if (op == TRANSPILE_COBOL_COMPARISON_LESS_THAN)
        return ("<");
    if (op == TRANSPILE_COBOL_COMPARISON_LESS_OR_EQUAL)
        return ("<=");
    if (op == TRANSPILE_COBOL_COMPARISON_GREATER_THAN)
        return (">");
    if (op == TRANSPILE_COBOL_COMPARISON_GREATER_OR_EQUAL)
        return (">=");
    return (NULL);
}

static int transpiler_codegen_append_condition(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_condition *condition)
{
    const char *operator_text;

    if (!buffer)
        return (FT_FAILURE);
    if (!condition)
        return (FT_FAILURE);
    if (!condition->left || !condition->right)
        return (FT_FAILURE);
    operator_text = transpiler_codegen_condition_operator_text(condition->op);
    if (!operator_text)
        return (FT_FAILURE);
    if (condition->negated)
    {
        if (transpiler_codegen_buffer_append_string(buffer, "NOT ") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (transpiler_codegen_buffer_append_string(buffer, condition->left) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, " ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, operator_text) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, " ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, condition->right) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_codegen_append_statement_block(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_statement_block *block, size_t indentation);

static int transpiler_codegen_append_if_statement(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_if_statement *if_statement, size_t indentation)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!if_statement)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "IF ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_condition(buffer, &if_statement->condition) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_statement_block(buffer, &if_statement->then_branch,
            indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (if_statement->else_branch.count > 0)
    {
        if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, "ELSE\n") != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_append_statement_block(buffer, &if_statement->else_branch,
                indentation + 1) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "END-IF\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_codegen_append_perform_until(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_perform_until *perform_until, size_t indentation)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!perform_until)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "PERFORM UNTIL ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_condition(buffer, &perform_until->condition) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_statement_block(buffer, &perform_until->body,
            indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "END-PERFORM\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_codegen_append_perform_varying(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_perform_varying *perform_varying, size_t indentation)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!perform_varying)
        return (FT_FAILURE);
    if (!perform_varying->counter || !perform_varying->initial || !perform_varying->step)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "PERFORM VARYING ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, perform_varying->counter) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, " FROM ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, perform_varying->initial) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, " BY ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, perform_varying->step) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, " UNTIL ") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_condition(buffer, &perform_varying->condition) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "\n") != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_append_statement_block(buffer, &perform_varying->body,
            indentation + 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_codegen_buffer_append_string(buffer, "END-PERFORM\n") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_codegen_append_statement(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_statement *statement, size_t indentation)
{
    if (!buffer)
        return (FT_FAILURE);
    if (!statement)
        return (FT_FAILURE);
    if (statement->kind == TRANSPILE_COBOL_STATEMENT_MOVE)
    {
        if (!statement->move.source || !statement->move.target)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_indent(buffer, indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, "MOVE ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, statement->move.source) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, " TO ") != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, statement->move.target) != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_codegen_buffer_append_string(buffer, "\n") != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_SUCCESS);
    }
    if (statement->kind == TRANSPILE_COBOL_STATEMENT_IF)
        return (transpiler_codegen_append_if_statement(buffer, &statement->if_statement, indentation));
    if (statement->kind == TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL)
        return (transpiler_codegen_append_perform_until(buffer, &statement->perform_until, indentation));
    if (statement->kind == TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING)
        return (transpiler_codegen_append_perform_varying(buffer, &statement->perform_varying, indentation));
    return (FT_FAILURE);
}

static int transpiler_codegen_append_statement_block(t_transpiler_codegen_buffer *buffer,
    const t_transpiler_cobol_statement_block *block, size_t indentation)
{
    size_t index;

    if (!buffer)
        return (FT_FAILURE);
    if (!block)
        return (FT_SUCCESS);
    index = 0;
    while (index < block->count)
    {
        if (transpiler_codegen_append_statement(buffer, block->statements[index], indentation) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

void transpiler_codegen_file_sections_init(t_transpiler_cobol_file_sections *sections)
{
    if (!sections)
        return ;
    sections->environment_division = NULL;
    sections->data_division = NULL;
}

void transpiler_codegen_file_sections_dispose(t_transpiler_cobol_file_sections *sections)
{
    if (!sections)
        return ;
    if (sections->environment_division)
        cma_free(sections->environment_division);
    if (sections->data_division)
        cma_free(sections->data_division);
    sections->environment_division = NULL;
    sections->data_division = NULL;
}

static int transpiler_codegen_copy_buffer(const t_transpiler_codegen_buffer *buffer, char **out)
{
    char *copy;

    if (!buffer || !out)
        return (FT_FAILURE);
    copy = static_cast<char *>(cma_calloc(buffer->length + 1, sizeof(char)));
    if (!copy)
        return (FT_FAILURE);
    if (buffer->length > 0)
        ft_memcpy(copy, buffer->data, buffer->length);
    copy[buffer->length] = '\0';
    *out = copy;
    return (FT_SUCCESS);
}

int transpiler_codegen_build_file_sections(const t_transpiler_context *context,
    t_transpiler_cobol_file_sections *sections)
{
    t_transpiler_codegen_buffer environment_buffer;
    t_transpiler_codegen_buffer data_buffer;
    const t_transpiler_file_declaration *files;
    size_t file_count;
    size_t index;
    int status;

    if (!context)
        return (FT_FAILURE);
    if (!sections)
        return (FT_FAILURE);
    transpiler_codegen_file_sections_dispose(sections);
    transpiler_codegen_file_sections_init(sections);
    transpiler_codegen_buffer_init(&environment_buffer);
    transpiler_codegen_buffer_init(&data_buffer);
    status = FT_FAILURE;
    if (transpiler_codegen_buffer_append_string(&environment_buffer, "ENVIRONMENT DIVISION.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_buffer_append_string(&environment_buffer, "    INPUT-OUTPUT SECTION.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_buffer_append_string(&environment_buffer, "    FILE-CONTROL.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_buffer_append_string(&data_buffer, "DATA DIVISION.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_buffer_append_string(&data_buffer, "    FILE SECTION.\n") != FT_SUCCESS)
        goto cleanup;
    files = transpiler_context_get_files(context, &file_count);
    index = 0;
    while (index < file_count)
    {
        char upper_name[TRANSPILE_IDENTIFIER_MAX];
        size_t record_length;

        transpiler_codegen_uppercase_copy(files[index].name, upper_name, sizeof(upper_name));
        if (transpiler_codegen_buffer_append_format(&environment_buffer,
            "        SELECT %s ASSIGN TO \"%s\"\n",
            upper_name, files[index].path) != FT_SUCCESS)
            goto cleanup;
        if (transpiler_codegen_buffer_append_format(&environment_buffer,
            "            ORGANIZATION IS %s.\n",
            transpiler_codegen_organization_for_role(files[index].role)) != FT_SUCCESS)
            goto cleanup;
        record_length = transpiler_codegen_determine_record_length(&files[index]);
        if (transpiler_codegen_buffer_append_format(&data_buffer, "    FD %s.\n", upper_name) != FT_SUCCESS)
            goto cleanup;
        if (transpiler_codegen_buffer_append_format(&data_buffer,
            "        01 %s-REC PIC X(%zu).\n", upper_name, record_length) != FT_SUCCESS)
            goto cleanup;
        if (index + 1 < file_count)
        {
            if (transpiler_codegen_buffer_append_string(&data_buffer, "\n") != FT_SUCCESS)
                goto cleanup;
        }
        index += 1;
    }
    if (transpiler_codegen_copy_buffer(&environment_buffer, &sections->environment_division) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_copy_buffer(&data_buffer, &sections->data_division) != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_buffer_dispose(&environment_buffer);
    transpiler_codegen_buffer_dispose(&data_buffer);
    if (status != FT_SUCCESS)
        transpiler_codegen_file_sections_dispose(sections);
    return (status);
}

int transpiler_codegen_build_procedure_division(const t_transpiler_cobol_statement_block *statements,
    char **out)
{
    t_transpiler_codegen_buffer procedure_buffer;
    int status;

    if (!out)
        return (FT_FAILURE);
    *out = NULL;
    transpiler_codegen_buffer_init(&procedure_buffer);
    status = FT_FAILURE;
    if (transpiler_codegen_buffer_append_string(&procedure_buffer, "PROCEDURE DIVISION.\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_append_statement_block(&procedure_buffer, statements, 1) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_codegen_copy_buffer(&procedure_buffer, out) != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    transpiler_codegen_buffer_dispose(&procedure_buffer);
    if (status != FT_SUCCESS)
        *out = NULL;
    return (status);
}
