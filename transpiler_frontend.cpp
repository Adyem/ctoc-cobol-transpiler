#include "cblc_transpiler.hpp"

#include <cctype>

#include "compatibility/libft_compat.hpp"
#include "compatibility/memory_compat.hpp"

typedef struct s_cblc_completion_keyword
{
    const char *label;
}   t_cblc_completion_keyword;

typedef enum e_cblc_completion_context_kind
{
    CBLC_COMPLETION_CONTEXT_GENERAL = 0,
    CBLC_COMPLETION_CONTEXT_MEMBER,
    CBLC_COMPLETION_CONTEXT_STD_NAMESPACE
}   t_cblc_completion_context_kind;

typedef struct s_cblc_completion_context
{
    t_cblc_completion_context_kind kind;
    char prefix[TRANSPILE_IDENTIFIER_MAX];
    char receiver[TRANSPILE_IDENTIFIER_MAX];
}   t_cblc_completion_context;

static const t_cblc_completion_keyword g_cblc_completion_keywords[] = {
    {"import"},
    {"copy"},
    {"function"},
    {"void"},
    {"int"},
    {"char"},
    {"string"},
    {"struct"},
    {"class"},
    {"public"},
    {"private"},
    {"const"},
    {"display"},
    {"return"}
};

static const char *cblc_frontend_data_declaration_token(const t_cblc_data_item *item);

static void transpiler_source_span_reset(t_transpiler_source_span *span)
{
    if (!span)
        return ;
    ft_bzero(span, sizeof(*span));
}

static int transpiler_source_is_identifier_char(char value)
{
    if ((value >= 'A' && value <= 'Z') || (value >= 'a' && value <= 'z'))
        return (1);
    if (value >= '0' && value <= '9')
        return (1);
    if (value == '_' || value == ':')
        return (1);
    return (0);
}

static int transpiler_source_is_identifier_boundary(const char *text, size_t index)
{
    if (!text)
        return (0);
    if (text[index] == '\0')
        return (1);
    if (!transpiler_source_is_identifier_char(text[index]))
        return (1);
    return (0);
}

static int transpiler_source_offset_to_position(const char *text, size_t offset,
    size_t *line, size_t *column)
{
    size_t current_line;
    size_t current_column;
    size_t index;

    if (!text || !line || !column)
        return (FT_FAILURE);
    current_line = 1;
    current_column = 1;
    index = 0;
    while (text[index] != '\0' && index < offset)
    {
        if (text[index] == '\n')
        {
            current_line += 1;
            current_column = 1;
        }
        else
            current_column += 1;
        index += 1;
    }
    *line = current_line;
    *column = current_column;
    return (FT_SUCCESS);
}

static int transpiler_source_position_to_offset(const char *text, size_t line, size_t column,
    size_t *offset)
{
    size_t current_line;
    size_t current_column;
    size_t index;

    if (!text || !offset || line == 0 || column == 0)
        return (FT_FAILURE);
    current_line = 1;
    current_column = 1;
    index = 0;
    while (text[index] != '\0')
    {
        if (current_line == line && current_column == column)
        {
            *offset = index;
            return (FT_SUCCESS);
        }
        if (text[index] == '\n')
        {
            current_line += 1;
            current_column = 1;
        }
        else
            current_column += 1;
        index += 1;
    }
    if (current_line == line && current_column == column)
    {
        *offset = index;
        return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static int transpiler_source_make_span(const char *path, const char *text, size_t start_offset,
    size_t end_offset, t_transpiler_source_span *span)
{
    if (!text || !span || end_offset < start_offset)
        return (FT_FAILURE);
    transpiler_source_span_reset(span);
    if (path)
        ft_strlcpy(span->path, path, sizeof(span->path));
    if (transpiler_source_offset_to_position(text, start_offset, &span->start_line,
            &span->start_column) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_source_offset_to_position(text, end_offset, &span->end_line,
            &span->end_column) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_source_find_identifier(const char *text, const char *name,
    size_t search_start, size_t *match_offset)
{
    size_t name_length;
    const char *match;
    size_t offset;

    if (!text || !name || !match_offset)
        return (FT_FAILURE);
    name_length = std::strlen(name);
    offset = search_start;
    while (text[offset] != '\0')
    {
        match = std::strstr(text + offset, name);
        if (!match)
            return (FT_FAILURE);
        offset = static_cast<size_t>(match - text);
        if ((offset == 0 || transpiler_source_is_identifier_boundary(text, offset - 1))
            && transpiler_source_is_identifier_boundary(text, offset + name_length))
        {
            *match_offset = offset;
            return (FT_SUCCESS);
        }
        offset += 1;
    }
    return (FT_FAILURE);
}

static int transpiler_source_find_line_bounds(const char *text, size_t offset, size_t *line_start,
    size_t *line_end)
{
    size_t start;
    size_t end;

    if (!text || !line_start || !line_end)
        return (FT_FAILURE);
    start = offset;
    while (start > 0 && text[start - 1] != '\n')
        start -= 1;
    end = offset;
    while (text[end] != '\0' && text[end] != '\n')
        end += 1;
    *line_start = start;
    *line_end = end;
    return (FT_SUCCESS);
}

static int transpiler_source_line_contains_token_before_offset(const char *text,
    size_t line_start, size_t name_offset, const char *token)
{
    size_t token_offset;
    size_t token_length;

    if (!text || !token)
        return (0);
    token_length = std::strlen(token);
    token_offset = line_start;
    while (token_offset + token_length <= name_offset)
    {
        if (std::strncmp(text + token_offset, token, token_length) == 0
            && (token_offset == 0
                || transpiler_source_is_identifier_boundary(text, token_offset - 1))
            && transpiler_source_is_identifier_boundary(text, token_offset + token_length))
            return (1);
        token_offset += 1;
    }
    return (0);
}

static int transpiler_source_find_function_name_span(const char *path, const char *text,
    const char *name, t_transpiler_source_span *span)
{
    size_t name_offset;
    size_t line_start;
    size_t line_end;
    size_t cursor;

    if (!path || !text || !name || !span)
        return (FT_FAILURE);
    cursor = 0;
    while (transpiler_source_find_identifier(text, name, cursor, &name_offset) == FT_SUCCESS)
    {
        if (transpiler_source_find_line_bounds(text, name_offset, &line_start, &line_end)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_source_line_contains_token_before_offset(text, line_start, name_offset,
                "function")
            && name_offset + std::strlen(name) < line_end
            && text[name_offset + std::strlen(name)] == '(')
            return (transpiler_source_make_span(path, text, name_offset,
                    name_offset + std::strlen(name), span));
        cursor = name_offset + 1;
    }
    return (FT_FAILURE);
}

static int transpiler_source_find_type_name_span(const char *path, const char *text,
    const char *keyword, const char *name, t_transpiler_source_span *span)
{
    size_t name_offset;
    size_t line_start;
    size_t line_end;
    size_t cursor;

    if (!path || !text || !keyword || !name || !span)
        return (FT_FAILURE);
    cursor = 0;
    while (transpiler_source_find_identifier(text, name, cursor, &name_offset) == FT_SUCCESS)
    {
        if (transpiler_source_find_line_bounds(text, name_offset, &line_start, &line_end)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_source_line_contains_token_before_offset(text, line_start, name_offset,
                keyword))
            return (transpiler_source_make_span(path, text, name_offset,
                    name_offset + std::strlen(name), span));
        cursor = name_offset + 1;
    }
    return (FT_FAILURE);
}

static int transpiler_source_find_data_item_name_span(const char *path, const char *text,
    const t_cblc_data_item *item, t_transpiler_source_span *span)
{
    size_t name_offset;
    size_t line_start;
    size_t line_end;
    size_t cursor;
    const char *type_name;

    if (!path || !text || !item || !span || item->source_name[0] == '\0')
        return (FT_FAILURE);
    type_name = cblc_frontend_data_declaration_token(item);
    if (type_name[0] == '\0')
        return (FT_FAILURE);
    cursor = 0;
    while (transpiler_source_find_identifier(text, item->source_name, cursor, &name_offset)
        == FT_SUCCESS)
    {
        if (transpiler_source_find_line_bounds(text, name_offset, &line_start, &line_end)
            != FT_SUCCESS)
            return (FT_FAILURE);
        if (transpiler_source_line_contains_token_before_offset(text, line_start, name_offset,
                type_name))
        {
            if (item->owner_function_name[0] == '\0'
                || transpiler_source_find_function_name_span(path, text, item->owner_function_name,
                    span) == FT_SUCCESS)
            {
                return (transpiler_source_make_span(path, text, name_offset,
                        name_offset + std::strlen(item->source_name), span));
            }
        }
        cursor = name_offset + 1;
    }
    return (FT_FAILURE);
}

int lexer_token_get_span(const t_lexer_token *token, const char *path,
    t_transpiler_source_span *span)
{
    size_t index;
    size_t end_line;
    size_t end_column;

    if (!token || !span)
        return (FT_FAILURE);
    transpiler_source_span_reset(span);
    if (path)
        ft_strlcpy(span->path, path, sizeof(span->path));
    span->start_line = token->line;
    span->start_column = token->column;
    end_line = token->line;
    end_column = token->column;
    index = 0;
    while (token->lexeme && index < token->length)
    {
        if (token->lexeme[index] == '\n')
        {
            end_line += 1;
            end_column = 1;
        }
        else
            end_column += 1;
        index += 1;
    }
    span->end_line = end_line;
    span->end_column = end_column;
    return (FT_SUCCESS);
}

int ast_node_get_span(const t_ast_node *node, const char *path, t_transpiler_source_span *span)
{
    if (!node || !span)
        return (FT_FAILURE);
    return (lexer_token_get_span(&node->token, path, span));
}

static int cblc_document_symbol_list_reserve(t_cblc_document_symbol_list *list,
    size_t desired_capacity)
{
    t_cblc_document_symbol *new_items;
    size_t index;

    if (!list)
        return (FT_FAILURE);
    if (list->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_items = static_cast<t_cblc_document_symbol *>(cma_calloc(desired_capacity,
            sizeof(*new_items)));
    if (!new_items)
        return (FT_FAILURE);
    index = 0;
    while (index < list->count)
    {
        new_items[index] = list->items[index];
        index += 1;
    }
    if (list->items)
        cma_free(list->items);
    list->items = new_items;
    list->capacity = desired_capacity;
    return (FT_SUCCESS);
}

static int cblc_document_symbol_list_append(t_cblc_document_symbol_list *list,
    t_cblc_document_symbol_kind kind, const char *name, const t_transpiler_source_span *span)
{
    t_cblc_document_symbol *item;

    if (!list || !name || !span)
        return (FT_FAILURE);
    if (list->count >= list->capacity)
    {
        if (cblc_document_symbol_list_reserve(list,
                list->capacity == 0 ? 4 : list->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &list->items[list->count];
    ft_bzero(item, sizeof(*item));
    item->kind = kind;
    ft_strlcpy(item->name, name, sizeof(item->name));
    item->span = *span;
    list->count += 1;
    return (FT_SUCCESS);
}

int cblc_frontend_analysis_init(t_cblc_frontend_analysis *analysis)
{
    if (!analysis)
        return (FT_FAILURE);
    ft_bzero(analysis, sizeof(*analysis));
    analysis->unit = static_cast<t_cblc_translation_unit *>(cma_calloc(1, sizeof(*analysis->unit)));
    if (!analysis->unit)
        return (FT_FAILURE);
    cblc_translation_unit_init(analysis->unit);
    if (transpiler_diagnostics_init(&analysis->diagnostics) != FT_SUCCESS)
        return (FT_FAILURE);
    analysis->parse_status = FT_FAILURE;
    return (FT_SUCCESS);
}

void cblc_frontend_analysis_dispose(t_cblc_frontend_analysis *analysis)
{
    if (!analysis)
        return ;
    if (analysis->source_text)
        cma_free(analysis->source_text);
    analysis->source_text = NULL;
    if (analysis->unit)
    {
        cblc_translation_unit_dispose(analysis->unit);
        cma_free(analysis->unit);
    }
    analysis->unit = NULL;
    transpiler_diagnostics_dispose(&analysis->diagnostics);
    ft_bzero(analysis, sizeof(*analysis));
}

int cblc_document_symbol_list_init(t_cblc_document_symbol_list *list)
{
    if (!list)
        return (FT_FAILURE);
    ft_bzero(list, sizeof(*list));
    return (FT_SUCCESS);
}

void cblc_document_symbol_list_dispose(t_cblc_document_symbol_list *list)
{
    if (!list)
        return ;
    if (list->items)
        cma_free(list->items);
    ft_bzero(list, sizeof(*list));
}

static int cblc_completion_list_reserve(t_cblc_completion_list *list, size_t desired_capacity)
{
    t_cblc_completion_item *new_items;
    size_t index;

    if (!list)
        return (FT_FAILURE);
    if (list->capacity >= desired_capacity)
        return (FT_SUCCESS);
    if (desired_capacity < 4)
        desired_capacity = 4;
    new_items = static_cast<t_cblc_completion_item *>(cma_calloc(desired_capacity,
            sizeof(*new_items)));
    if (!new_items)
        return (FT_FAILURE);
    index = 0;
    while (index < list->count)
    {
        new_items[index] = list->items[index];
        index += 1;
    }
    if (list->items)
        cma_free(list->items);
    list->items = new_items;
    list->capacity = desired_capacity;
    return (FT_SUCCESS);
}

int cblc_completion_list_init(t_cblc_completion_list *list)
{
    if (!list)
        return (FT_FAILURE);
    ft_bzero(list, sizeof(*list));
    return (FT_SUCCESS);
}

void cblc_completion_list_dispose(t_cblc_completion_list *list)
{
    if (!list)
        return ;
    if (list->items)
        cma_free(list->items);
    ft_bzero(list, sizeof(*list));
}

static int cblc_completion_label_equals(const char *left, const char *right)
{
    size_t index;

    if (!left || !right)
        return (0);
    index = 0;
    while (left[index] != '\0' && right[index] != '\0')
    {
        if (std::tolower(static_cast<unsigned char>(left[index]))
            != std::tolower(static_cast<unsigned char>(right[index])))
            return (0);
        index += 1;
    }
    if (left[index] != '\0' || right[index] != '\0')
        return (0);
    return (1);
}

static int cblc_completion_matches_prefix(const char *label, const char *prefix)
{
    size_t index;

    if (!label || !prefix)
        return (0);
    if (prefix[0] == '\0')
        return (1);
    index = 0;
    while (prefix[index] != '\0')
    {
        if (label[index] == '\0')
            return (0);
        if (std::tolower(static_cast<unsigned char>(label[index]))
            != std::tolower(static_cast<unsigned char>(prefix[index])))
            return (0);
        index += 1;
    }
    return (1);
}

static int cblc_completion_list_contains(const t_cblc_completion_list *list, const char *label)
{
    size_t index;

    if (!list || !label)
        return (0);
    index = 0;
    while (index < list->count)
    {
        if (cblc_completion_label_equals(list->items[index].label, label))
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_completion_list_append(t_cblc_completion_list *list,
    t_cblc_completion_item_kind kind, const char *label)
{
    t_cblc_completion_item *item;

    if (!list || !label)
        return (FT_FAILURE);
    if (cblc_completion_list_contains(list, label))
        return (FT_SUCCESS);
    if (list->count >= list->capacity)
    {
        if (cblc_completion_list_reserve(list,
                list->capacity == 0 ? 4 : list->capacity * 2) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    item = &list->items[list->count];
    ft_bzero(item, sizeof(*item));
    item->kind = kind;
    ft_strlcpy(item->label, label, sizeof(item->label));
    list->count += 1;
    return (FT_SUCCESS);
}

int cblc_frontend_analyze_document(t_cblc_frontend_analysis *analysis, const char *path,
    const char *text)
{
    size_t text_length;
    t_transpiler_source_span span;

    if (!analysis || !text)
        return (FT_FAILURE);
    if (analysis->source_text)
    {
        cma_free(analysis->source_text);
        analysis->source_text = NULL;
    }
    if (!analysis->unit)
        return (FT_FAILURE);
    cblc_translation_unit_dispose(analysis->unit);
    cblc_translation_unit_init(analysis->unit);
    transpiler_diagnostics_dispose(&analysis->diagnostics);
    if (transpiler_diagnostics_init(&analysis->diagnostics) != FT_SUCCESS)
        return (FT_FAILURE);
    analysis->parse_status = FT_FAILURE;
    analysis->path[0] = '\0';
    if (path)
        ft_strlcpy(analysis->path, path, sizeof(analysis->path));
    text_length = std::strlen(text);
    analysis->source_text = static_cast<char *>(cma_calloc(text_length + 1, sizeof(char)));
    if (!analysis->source_text)
        return (FT_FAILURE);
    ft_strlcpy(analysis->source_text, text, text_length + 1);
    if (cblc_parse_translation_unit(text, analysis->unit) != FT_SUCCESS)
    {
        transpiler_source_span_reset(&span);
        if (path)
            ft_strlcpy(span.path, path, sizeof(span.path));
        span.start_line = 1;
        span.start_column = 1;
        span.end_line = 1;
        span.end_column = 1;
        if (transpiler_diagnostics_push_with_details(&analysis->diagnostics,
                TRANSPILE_SEVERITY_ERROR, FT_FAILURE, "Parse error", &span, NULL, NULL)
            != FT_SUCCESS)
            return (FT_FAILURE);
        return (FT_FAILURE);
    }
    analysis->parse_status = FT_SUCCESS;
    return (FT_SUCCESS);
}

static int cblc_frontend_collect_type_symbols(const t_cblc_frontend_analysis *analysis,
    t_cblc_document_symbol_list *symbols)
{
    size_t index;
    t_transpiler_source_span span;

    index = 0;
    while (index < analysis->unit->struct_type_count)
    {
        if (!analysis->unit->struct_types[index].is_builtin)
        {
            if (analysis->unit->struct_types[index].is_class)
            {
                if (transpiler_source_find_type_name_span(analysis->path, analysis->source_text,
                        "class", analysis->unit->struct_types[index].source_name, &span)
                    == FT_SUCCESS)
                {
                    if (cblc_document_symbol_list_append(symbols, CBLC_DOCUMENT_SYMBOL_CLASS,
                            analysis->unit->struct_types[index].source_name, &span) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
            else
            {
                if (transpiler_source_find_type_name_span(analysis->path, analysis->source_text,
                        "struct", analysis->unit->struct_types[index].source_name, &span)
                    == FT_SUCCESS)
                {
                    if (cblc_document_symbol_list_append(symbols, CBLC_DOCUMENT_SYMBOL_STRUCT,
                            analysis->unit->struct_types[index].source_name, &span) != FT_SUCCESS)
                        return (FT_FAILURE);
                }
            }
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_collect_function_symbols(const t_cblc_frontend_analysis *analysis,
    t_cblc_document_symbol_list *symbols)
{
    size_t index;
    t_transpiler_source_span span;

    index = 0;
    while (index < analysis->unit->function_count)
    {
        if (transpiler_source_find_function_name_span(analysis->path, analysis->source_text,
                analysis->unit->functions[index].source_name, &span) == FT_SUCCESS)
        {
            if (cblc_document_symbol_list_append(symbols, CBLC_DOCUMENT_SYMBOL_FUNCTION,
                    analysis->unit->functions[index].source_name, &span) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_collect_data_symbols(const t_cblc_frontend_analysis *analysis,
    t_cblc_document_symbol_list *symbols)
{
    size_t index;
    t_transpiler_source_span span;

    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias && analysis->unit->data_items[index].is_active
            && transpiler_source_find_data_item_name_span(analysis->path, analysis->source_text,
                &analysis->unit->data_items[index], &span) == FT_SUCCESS)
        {
            if (cblc_document_symbol_list_append(symbols, CBLC_DOCUMENT_SYMBOL_DATA_ITEM,
                    analysis->unit->data_items[index].source_name, &span) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

int cblc_frontend_collect_document_symbols(const t_cblc_frontend_analysis *analysis,
    t_cblc_document_symbol_list *symbols)
{
    if (!analysis || !symbols)
        return (FT_FAILURE);
    cblc_document_symbol_list_dispose(symbols);
    if (cblc_document_symbol_list_init(symbols) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!analysis->source_text || analysis->parse_status != FT_SUCCESS)
        return (FT_SUCCESS);
    if (cblc_frontend_collect_type_symbols(analysis, symbols) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_collect_function_symbols(analysis, symbols) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_collect_data_symbols(analysis, symbols) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int cblc_frontend_extract_identifier_at_position(const char *text, size_t line,
    size_t column, char *identifier, size_t identifier_size)
{
    size_t offset;
    size_t start;
    size_t end;
    size_t length;

    if (!text || !identifier || identifier_size == 0)
        return (FT_FAILURE);
    identifier[0] = '\0';
    if (transpiler_source_position_to_offset(text, line, column, &offset) != FT_SUCCESS)
        return (FT_FAILURE);
    start = offset;
    while (start > 0 && transpiler_source_is_identifier_char(text[start - 1]))
        start -= 1;
    end = offset;
    while (transpiler_source_is_identifier_char(text[end]))
        end += 1;
    if (end == start && offset > 0 && transpiler_source_is_identifier_char(text[offset - 1]))
    {
        end = offset;
        start = offset - 1;
        while (start > 0 && transpiler_source_is_identifier_char(text[start - 1]))
            start -= 1;
    }
    if (end <= start)
        return (FT_FAILURE);
    length = end - start;
    if (length >= identifier_size)
        length = identifier_size - 1;
    std::memcpy(identifier, text + start, length);
    identifier[length] = '\0';
    return (FT_SUCCESS);
}

static int cblc_frontend_find_data_definition(const t_cblc_frontend_analysis *analysis,
    const char *identifier, t_transpiler_source_span *span)
{
    size_t index;

    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias
            && analysis->unit->data_items[index].is_active
            && std::strncmp(analysis->unit->data_items[index].source_name, identifier,
                sizeof(analysis->unit->data_items[index].source_name)) == 0
            && transpiler_source_find_data_item_name_span(analysis->path, analysis->source_text,
                &analysis->unit->data_items[index], span) == FT_SUCCESS)
            return (FT_SUCCESS);
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_frontend_find_function_definition(const t_cblc_frontend_analysis *analysis,
    const char *identifier, t_transpiler_source_span *span)
{
    size_t index;

    index = 0;
    while (index < analysis->unit->function_count)
    {
        if (std::strncmp(analysis->unit->functions[index].source_name, identifier,
                sizeof(analysis->unit->functions[index].source_name)) == 0
            && transpiler_source_find_function_name_span(analysis->path, analysis->source_text,
                analysis->unit->functions[index].source_name, span) == FT_SUCCESS)
            return (FT_SUCCESS);
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_frontend_find_type_definition(const t_cblc_frontend_analysis *analysis,
    const char *identifier, t_transpiler_source_span *span)
{
    size_t index;

    index = 0;
    while (index < analysis->unit->struct_type_count)
    {
        if (!analysis->unit->struct_types[index].is_builtin
            && std::strncmp(analysis->unit->struct_types[index].source_name, identifier,
                sizeof(analysis->unit->struct_types[index].source_name)) == 0)
        {
            if (analysis->unit->struct_types[index].is_class)
            {
                return (transpiler_source_find_type_name_span(analysis->path,
                        analysis->source_text, "class",
                        analysis->unit->struct_types[index].source_name, span));
            }
            return (transpiler_source_find_type_name_span(analysis->path,
                    analysis->source_text, "struct",
                    analysis->unit->struct_types[index].source_name, span));
        }
        index += 1;
    }
    return (FT_FAILURE);
}

int cblc_frontend_find_definition(const t_cblc_frontend_analysis *analysis, size_t line,
    size_t column, t_transpiler_source_span *definition_span)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];

    if (!analysis || !definition_span || !analysis->source_text)
        return (FT_FAILURE);
    transpiler_source_span_reset(definition_span);
    if (analysis->parse_status != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_extract_identifier_at_position(analysis->source_text, line, column,
            identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_find_data_definition(analysis, identifier, definition_span) == FT_SUCCESS)
        return (FT_SUCCESS);
    if (cblc_frontend_find_function_definition(analysis, identifier, definition_span)
        == FT_SUCCESS)
        return (FT_SUCCESS);
    if (cblc_frontend_find_type_definition(analysis, identifier, definition_span) == FT_SUCCESS)
        return (FT_SUCCESS);
    return (FT_FAILURE);
}

static const char *cblc_frontend_data_kind_name(const t_cblc_data_item *item)
{
    if (!item)
        return ("unknown");
    if (item->declared_type_name[0] != '\0')
        return (item->declared_type_name);
    if (item->kind == CBLC_DATA_KIND_CHAR)
        return ("char");
    if (item->kind == CBLC_DATA_KIND_INT)
        return ("int");
    if (item->kind == CBLC_DATA_KIND_STRING)
        return ("string");
    if (item->kind == CBLC_DATA_KIND_STRUCT)
        return (item->struct_type_name);
    return ("unknown");
}

static const char *cblc_frontend_data_declaration_token(const t_cblc_data_item *item)
{
    if (!item)
        return ("");
    if (item->declared_type_name[0] != '\0')
        return (item->declared_type_name);
    if (item->kind == CBLC_DATA_KIND_CHAR)
        return ("char");
    if (item->kind == CBLC_DATA_KIND_INT)
        return ("int");
    if (item->kind == CBLC_DATA_KIND_STRING)
        return ("string");
    if (item->kind == CBLC_DATA_KIND_STRUCT)
        return (item->struct_type_name);
    return ("");
}

static const char *cblc_frontend_return_kind_name(t_cblc_function_return_kind kind)
{
    if (kind == CBLC_FUNCTION_RETURN_INT)
        return ("int");
    return ("void");
}

int cblc_frontend_get_hover(const t_cblc_frontend_analysis *analysis, size_t line,
    size_t column, char *buffer, size_t buffer_size)
{
    char identifier[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!analysis || !buffer || buffer_size == 0 || !analysis->source_text)
        return (FT_FAILURE);
    buffer[0] = '\0';
    if (analysis->parse_status != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_extract_identifier_at_position(analysis->source_text, line, column,
            identifier, sizeof(identifier)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias
            && analysis->unit->data_items[index].is_active
            && std::strncmp(analysis->unit->data_items[index].source_name, identifier,
                sizeof(analysis->unit->data_items[index].source_name)) == 0)
        {
            if (std::snprintf(buffer, buffer_size, "Variable %s : %s",
                    analysis->unit->data_items[index].source_name,
                    cblc_frontend_data_kind_name(&analysis->unit->data_items[index])) < 0)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        index += 1;
    }
    index = 0;
    while (index < analysis->unit->function_count)
    {
        if (std::strncmp(analysis->unit->functions[index].source_name, identifier,
                sizeof(analysis->unit->functions[index].source_name)) == 0)
        {
            if (std::snprintf(buffer, buffer_size, "Function %s : %s",
                    analysis->unit->functions[index].source_name,
                    cblc_frontend_return_kind_name(analysis->unit->functions[index].return_kind))
                < 0)
                return (FT_FAILURE);
            return (FT_SUCCESS);
        }
        index += 1;
    }
    index = 0;
    while (index < analysis->unit->struct_type_count)
    {
        if (!analysis->unit->struct_types[index].is_builtin
            && std::strncmp(analysis->unit->struct_types[index].source_name, identifier,
                sizeof(analysis->unit->struct_types[index].source_name)) == 0)
        {
            if (analysis->unit->struct_types[index].is_class)
            {
                if (std::snprintf(buffer, buffer_size, "Class %s",
                        analysis->unit->struct_types[index].source_name) < 0)
                    return (FT_FAILURE);
            }
            else
            {
                if (std::snprintf(buffer, buffer_size, "Struct %s",
                        analysis->unit->struct_types[index].source_name) < 0)
                    return (FT_FAILURE);
            }
            return (FT_SUCCESS);
        }
        index += 1;
    }
    return (FT_FAILURE);
}

static int cblc_frontend_extract_completion_context(const char *text, size_t line, size_t column,
    t_cblc_completion_context *context)
{
    size_t offset;
    size_t start;
    size_t receiver_end;
    size_t receiver_start;
    size_t length;

    if (!text || !context)
        return (FT_FAILURE);
    ft_bzero(context, sizeof(*context));
    context->kind = CBLC_COMPLETION_CONTEXT_GENERAL;
    if (transpiler_source_position_to_offset(text, line, column, &offset) != FT_SUCCESS)
        return (FT_FAILURE);
    start = offset;
    while (start > 0 && transpiler_source_is_identifier_char(text[start - 1]))
        start -= 1;
    length = offset - start;
    if (length >= sizeof(context->prefix))
        length = sizeof(context->prefix) - 1;
    if (length > 0)
        std::memcpy(context->prefix, text + start, length);
    context->prefix[length] = '\0';
    if (std::strncmp(context->prefix, "std::", std::strlen("std::")) == 0)
    {
        context->kind = CBLC_COMPLETION_CONTEXT_STD_NAMESPACE;
        return (FT_SUCCESS);
    }
    if (start == 0 || text[start - 1] != '.')
        return (FT_SUCCESS);
    context->kind = CBLC_COMPLETION_CONTEXT_MEMBER;
    receiver_end = start - 1;
    receiver_start = receiver_end;
    while (receiver_start > 0
        && transpiler_source_is_identifier_char(text[receiver_start - 1]))
        receiver_start -= 1;
    length = receiver_end - receiver_start;
    if (length >= sizeof(context->receiver))
        length = sizeof(context->receiver) - 1;
    if (length > 0)
        std::memcpy(context->receiver, text + receiver_start, length);
    context->receiver[length] = '\0';
    return (FT_SUCCESS);
}

static int transpiler_source_find_function_body_offsets(const char *path, const char *text,
    const char *name, size_t *body_start, size_t *body_end)
{
    t_transpiler_source_span span;
    size_t scan_offset;
    size_t open_brace_offset;
    size_t depth;

    if (!path || !text || !name || !body_start || !body_end)
        return (FT_FAILURE);
    if (transpiler_source_find_function_name_span(path, text, name, &span) != FT_SUCCESS)
        return (FT_FAILURE);
    if (transpiler_source_position_to_offset(text, span.end_line, span.end_column,
            &scan_offset) != FT_SUCCESS)
        return (FT_FAILURE);
    while (text[scan_offset] != '\0' && text[scan_offset] != '{')
        scan_offset += 1;
    if (text[scan_offset] != '{')
        return (FT_FAILURE);
    open_brace_offset = scan_offset;
    depth = 1;
    scan_offset += 1;
    while (text[scan_offset] != '\0')
    {
        if (text[scan_offset] == '{')
            depth += 1;
        else if (text[scan_offset] == '}')
        {
            depth -= 1;
            if (depth == 0)
            {
                *body_start = open_brace_offset + 1;
                *body_end = scan_offset;
                return (FT_SUCCESS);
            }
        }
        scan_offset += 1;
    }
    return (FT_FAILURE);
}

static int cblc_frontend_find_enclosing_function(const t_cblc_frontend_analysis *analysis,
    size_t line, size_t column, char *function_name, size_t function_name_size)
{
    size_t cursor_offset;
    size_t body_start;
    size_t body_end;
    size_t index;
    size_t scan_offset;
    size_t best_offset;
    size_t name_start;
    size_t name_length;
    const char *match;

    if (!analysis || !analysis->source_text || !function_name || function_name_size == 0)
        return (FT_FAILURE);
    function_name[0] = '\0';
    if (transpiler_source_position_to_offset(analysis->source_text, line, column, &cursor_offset)
        != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    while (index < analysis->unit->function_count)
    {
        if (transpiler_source_find_function_body_offsets(analysis->path, analysis->source_text,
                analysis->unit->functions[index].source_name, &body_start, &body_end)
            == FT_SUCCESS)
        {
            if (cursor_offset >= body_start && cursor_offset <= body_end)
            {
                ft_strlcpy(function_name, analysis->unit->functions[index].source_name,
                    function_name_size);
                return (FT_SUCCESS);
            }
        }
        index += 1;
    }
    best_offset = static_cast<size_t>(-1);
    scan_offset = 0;
    while (analysis->source_text[scan_offset] != '\0')
    {
        match = std::strstr(analysis->source_text + scan_offset, "function");
        if (!match)
            break ;
        scan_offset = static_cast<size_t>(match - analysis->source_text);
        if (scan_offset >= cursor_offset)
            break ;
        if ((scan_offset == 0
                || transpiler_source_is_identifier_boundary(analysis->source_text,
                    scan_offset - 1))
            && transpiler_source_is_identifier_boundary(analysis->source_text,
                scan_offset + std::strlen("function")))
            best_offset = scan_offset;
        scan_offset += 1;
    }
    if (best_offset == static_cast<size_t>(-1))
        return (FT_FAILURE);
    scan_offset = best_offset + std::strlen("function");
    while (analysis->source_text[scan_offset] != '\0'
        && std::isspace(static_cast<unsigned char>(analysis->source_text[scan_offset])))
        scan_offset += 1;
    if (std::strncmp(analysis->source_text + scan_offset, "void", std::strlen("void")) == 0)
        scan_offset += std::strlen("void");
    else if (std::strncmp(analysis->source_text + scan_offset, "int", std::strlen("int")) == 0)
        scan_offset += std::strlen("int");
    while (analysis->source_text[scan_offset] != '\0'
        && std::isspace(static_cast<unsigned char>(analysis->source_text[scan_offset])))
        scan_offset += 1;
    name_start = scan_offset;
    while (transpiler_source_is_identifier_char(analysis->source_text[scan_offset]))
        scan_offset += 1;
    if (scan_offset <= name_start)
        return (FT_FAILURE);
    name_length = scan_offset - name_start;
    if (name_length >= function_name_size)
        name_length = function_name_size - 1;
    std::memcpy(function_name, analysis->source_text + name_start, name_length);
    function_name[name_length] = '\0';
    if (function_name[0] == '\0')
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static const t_cblc_data_item *cblc_frontend_find_data_item_in_scope(
    const t_cblc_frontend_analysis *analysis, const char *identifier, const char *function_name)
{
    size_t index;
    char alias_source_name[TRANSPILE_IDENTIFIER_MAX];

    if (!analysis || !identifier)
        return (NULL);
    alias_source_name[0] = '\0';
    if (function_name && function_name[0] != '\0')
    {
        if (std::snprintf(alias_source_name, sizeof(alias_source_name), "%s__%s", function_name,
                identifier) < 0)
            alias_source_name[0] = '\0';
    }
    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias
            && analysis->unit->data_items[index].is_active
            && std::strncmp(analysis->unit->data_items[index].source_name, identifier,
                sizeof(analysis->unit->data_items[index].source_name)) == 0
            && function_name && function_name[0] != '\0'
            && std::strncmp(analysis->unit->data_items[index].owner_function_name, function_name,
                sizeof(analysis->unit->data_items[index].owner_function_name)) == 0)
            return (&analysis->unit->data_items[index]);
        index += 1;
    }
    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (analysis->unit->data_items[index].is_active
            && analysis->unit->data_items[index].owner_function_name[0] == '\0')
        {
            if (!analysis->unit->data_items[index].is_alias
                && std::strncmp(analysis->unit->data_items[index].source_name, identifier,
                    sizeof(analysis->unit->data_items[index].source_name)) == 0)
                return (&analysis->unit->data_items[index]);
            if (analysis->unit->data_items[index].is_alias
                && alias_source_name[0] != '\0'
                && std::strncmp(analysis->unit->data_items[index].source_name, identifier,
                    sizeof(analysis->unit->data_items[index].source_name)) == 0)
                return (&analysis->unit->data_items[index]);
        }
        index += 1;
    }
    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias
            && analysis->unit->data_items[index].is_active
            && alias_source_name[0] != '\0'
            && std::strncmp(analysis->unit->data_items[index].source_name, alias_source_name,
                sizeof(analysis->unit->data_items[index].source_name)) == 0)
            return (&analysis->unit->data_items[index]);
        index += 1;
    }
    return (NULL);
}

static const t_cblc_struct_type *cblc_frontend_find_struct_type(
    const t_cblc_frontend_analysis *analysis, const char *type_name)
{
    size_t index;

    if (!analysis || !type_name || type_name[0] == '\0')
        return (NULL);
    index = 0;
    while (index < analysis->unit->struct_type_count)
    {
        if (std::strncmp(analysis->unit->struct_types[index].source_name, type_name,
                sizeof(analysis->unit->struct_types[index].source_name)) == 0)
            return (&analysis->unit->struct_types[index]);
        index += 1;
    }
    return (NULL);
}

static int cblc_frontend_append_keyword_completions(t_cblc_completion_list *completions,
    const char *prefix)
{
    size_t index;

    index = 0;
    while (index < sizeof(g_cblc_completion_keywords) / sizeof(g_cblc_completion_keywords[0]))
    {
        if (cblc_completion_matches_prefix(g_cblc_completion_keywords[index].label, prefix))
        {
            if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_KEYWORD,
                    g_cblc_completion_keywords[index].label) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_alias_is_local_to_function(const t_cblc_frontend_analysis *analysis,
    const t_cblc_data_item *item, const char *function_name)
{
    char alias_source_name[TRANSPILE_IDENTIFIER_MAX];
    size_t index;

    if (!analysis || !item || !item->is_alias || !function_name || function_name[0] == '\0')
        return (0);
    if (std::snprintf(alias_source_name, sizeof(alias_source_name), "%s__%s", function_name,
            item->source_name) < 0)
        return (0);
    index = 0;
    while (index < analysis->unit->data_count)
    {
        if (!analysis->unit->data_items[index].is_alias
            && analysis->unit->data_items[index].is_active
            && std::strncmp(analysis->unit->data_items[index].source_name, alias_source_name,
                sizeof(analysis->unit->data_items[index].source_name)) == 0)
            return (1);
        index += 1;
    }
    return (0);
}

static int cblc_frontend_append_data_completions(const t_cblc_frontend_analysis *analysis,
    t_cblc_completion_list *completions, const char *prefix, const char *function_name)
{
    size_t index;
    char local_prefix[TRANSPILE_IDENTIFIER_MAX];
    const char *label;
    size_t local_prefix_length;

    local_prefix[0] = '\0';
    if (function_name && function_name[0] != '\0')
    {
        if (std::snprintf(local_prefix, sizeof(local_prefix), "%s__", function_name) < 0)
            local_prefix[0] = '\0';
    }
    local_prefix_length = std::strlen(local_prefix);
    index = 0;
    while (index < analysis->unit->data_count)
    {
        label = analysis->unit->data_items[index].source_name;
        if (analysis->unit->data_items[index].is_active)
        {
            if (analysis->unit->data_items[index].is_alias)
            {
                if (!cblc_frontend_alias_is_local_to_function(analysis,
                        &analysis->unit->data_items[index], function_name))
                {
                    index += 1;
                    continue ;
                }
            }
            else if (!(analysis->unit->data_items[index].owner_function_name[0] == '\0'
                    || (function_name && function_name[0] != '\0'
                        && std::strncmp(analysis->unit->data_items[index].owner_function_name,
                            function_name,
                            sizeof(analysis->unit->data_items[index].owner_function_name)) == 0)))
            {
                index += 1;
                continue ;
            }
            if (!analysis->unit->data_items[index].is_alias
                && function_name && function_name[0] != '\0'
                && std::strncmp(analysis->unit->data_items[index].owner_function_name,
                    function_name,
                    sizeof(analysis->unit->data_items[index].owner_function_name)) == 0
                && local_prefix_length > 0
                && std::strncmp(analysis->unit->data_items[index].source_name, local_prefix,
                    local_prefix_length) == 0)
                label = analysis->unit->data_items[index].source_name + local_prefix_length;
            if (cblc_completion_matches_prefix(label, prefix))
            {
                if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_DATA_ITEM,
                        label) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_append_type_and_function_completions(
    const t_cblc_frontend_analysis *analysis, t_cblc_completion_list *completions,
    const char *prefix)
{
    size_t index;

    index = 0;
    while (index < analysis->unit->struct_type_count)
    {
        if (!analysis->unit->struct_types[index].is_builtin
            && cblc_completion_matches_prefix(analysis->unit->struct_types[index].source_name,
                prefix))
        {
            if (analysis->unit->struct_types[index].is_class)
            {
                if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_CLASS,
                        analysis->unit->struct_types[index].source_name) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
            else
            {
                if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_STRUCT,
                        analysis->unit->struct_types[index].source_name) != FT_SUCCESS)
                    return (FT_FAILURE);
            }
        }
        index += 1;
    }
    index = 0;
    while (index < analysis->unit->function_count)
    {
        if (cblc_completion_matches_prefix(analysis->unit->functions[index].source_name, prefix))
        {
            if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_FUNCTION,
                    analysis->unit->functions[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_append_stdlib_completions(t_cblc_completion_list *completions,
    const char *prefix)
{
    const t_transpiler_standard_library_entry *entries;
    size_t entry_count;
    size_t index;

    entries = transpiler_standard_library_get_entries(&entry_count);
    index = 0;
    while (index < entry_count)
    {
        if (cblc_completion_matches_prefix(entries[index].qualified_name, prefix))
        {
            if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_FUNCTION,
                    entries[index].qualified_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_append_string_member_completions(t_cblc_completion_list *completions,
    const char *prefix)
{
    if (cblc_completion_matches_prefix("append", prefix))
    {
        if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_FUNCTION,
                "append") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    if (cblc_completion_matches_prefix("len", prefix))
    {
        if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_FUNCTION,
                "len") != FT_SUCCESS)
            return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_append_struct_member_completions(
    const t_cblc_struct_type *type, t_cblc_completion_list *completions, const char *prefix)
{
    size_t index;

    if (!type)
        return (FT_FAILURE);
    index = 0;
    while (index < type->field_count)
    {
        if ((!type->is_class
                || type->fields[index].visibility == CBLC_MEMBER_VISIBILITY_PUBLIC)
            && cblc_completion_matches_prefix(type->fields[index].source_name, prefix))
        {
            if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_DATA_ITEM,
                    type->fields[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    index = 0;
    while (index < type->method_count)
    {
        if ((!type->is_class
                || type->methods[index].visibility == CBLC_MEMBER_VISIBILITY_PUBLIC)
            && cblc_completion_matches_prefix(type->methods[index].source_name, prefix))
        {
            if (cblc_completion_list_append(completions, CBLC_COMPLETION_ITEM_FUNCTION,
                    type->methods[index].source_name) != FT_SUCCESS)
                return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

static int cblc_frontend_append_member_completions(const t_cblc_frontend_analysis *analysis,
    t_cblc_completion_list *completions, const t_cblc_completion_context *context,
    const char *function_name)
{
    const t_cblc_data_item *receiver_item;
    const t_cblc_struct_type *type;

    if (!analysis || !completions || !context)
        return (FT_FAILURE);
    receiver_item = cblc_frontend_find_data_item_in_scope(analysis, context->receiver,
            function_name);
    if (!receiver_item)
        return (FT_SUCCESS);
    if (receiver_item->kind == CBLC_DATA_KIND_STRING)
        return (cblc_frontend_append_string_member_completions(completions, context->prefix));
    if (receiver_item->kind != CBLC_DATA_KIND_STRUCT)
        return (FT_SUCCESS);
    type = cblc_frontend_find_struct_type(analysis, receiver_item->struct_type_name);
    if (!type)
        return (FT_SUCCESS);
    return (cblc_frontend_append_struct_member_completions(type, completions, context->prefix));
}

int cblc_frontend_complete(const t_cblc_frontend_analysis *analysis, size_t line,
    size_t column, t_cblc_completion_list *completions)
{
    t_cblc_completion_context context;
    char function_name[TRANSPILE_IDENTIFIER_MAX];

    if (!analysis || !completions || !analysis->source_text)
        return (FT_FAILURE);
    cblc_completion_list_dispose(completions);
    if (cblc_completion_list_init(completions) != FT_SUCCESS)
        return (FT_FAILURE);
    if (analysis->parse_status != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_extract_completion_context(analysis->source_text, line, column, &context)
        != FT_SUCCESS)
        return (FT_FAILURE);
    function_name[0] = '\0';
    cblc_frontend_find_enclosing_function(analysis, line, column, function_name,
        sizeof(function_name));
    if (context.kind == CBLC_COMPLETION_CONTEXT_STD_NAMESPACE)
        return (cblc_frontend_append_stdlib_completions(completions, context.prefix));
    if (context.kind == CBLC_COMPLETION_CONTEXT_MEMBER)
    {
        return (cblc_frontend_append_member_completions(analysis, completions, &context,
                function_name));
    }
    if (cblc_frontend_append_keyword_completions(completions, context.prefix) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_append_data_completions(analysis, completions, context.prefix,
            function_name) != FT_SUCCESS)
        return (FT_FAILURE);
    if (cblc_frontend_append_type_and_function_completions(analysis, completions, context.prefix)
        != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
