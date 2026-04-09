#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include "cblc_transpiler.hpp"

typedef struct s_lsp_document
{
    std::string uri;
    std::string path;
    std::string text;
    t_cblc_frontend_analysis analysis;
    int analysis_initialized;
}   t_lsp_document;

static int lsp_reanalyze_document(t_lsp_document *document);

static int lsp_skip_whitespace(const std::string &text, size_t *index)
{
    if (!index)
        return (FT_FAILURE);
    while (*index < text.size() && std::isspace(static_cast<unsigned char>(text[*index])) != 0)
        *index += 1;
    return (FT_SUCCESS);
}

static int lsp_parse_json_string(const std::string &text, size_t *index, std::string *value)
{
    std::string result;

    if (!index || !value || *index >= text.size() || text[*index] != '"')
        return (FT_FAILURE);
    *index += 1;
    while (*index < text.size())
    {
        char current;

        current = text[*index];
        if (current == '"')
        {
            *index += 1;
            *value = result;
            return (FT_SUCCESS);
        }
        if (current == '\\')
        {
            *index += 1;
            if (*index >= text.size())
                return (FT_FAILURE);
            current = text[*index];
            if (current == 'n')
                result.push_back('\n');
            else if (current == 'r')
                result.push_back('\r');
            else if (current == 't')
                result.push_back('\t');
            else
                result.push_back(current);
        }
        else
            result.push_back(current);
        *index += 1;
    }
    return (FT_FAILURE);
}

static int lsp_find_json_key(const std::string &text, const char *key, size_t search_start,
    size_t *value_index)
{
    std::string pattern;
    size_t key_index;

    if (!key || !value_index)
        return (FT_FAILURE);
    pattern = "\"";
    pattern += key;
    pattern += "\"";
    key_index = text.find(pattern, search_start);
    if (key_index == std::string::npos)
        return (FT_FAILURE);
    key_index = text.find(':', key_index + pattern.size());
    if (key_index == std::string::npos)
        return (FT_FAILURE);
    key_index += 1;
    if (lsp_skip_whitespace(text, &key_index) != FT_SUCCESS)
        return (FT_FAILURE);
    *value_index = key_index;
    return (FT_SUCCESS);
}

static int lsp_find_json_string_value(const std::string &text, const char *key,
    size_t search_start, std::string *value)
{
    size_t index;

    if (!value)
        return (FT_FAILURE);
    if (lsp_find_json_key(text, key, search_start, &index) != FT_SUCCESS)
        return (FT_FAILURE);
    return (lsp_parse_json_string(text, &index, value));
}

static int lsp_find_json_integer_value(const std::string &text, const char *key,
    size_t search_start, int *value)
{
    size_t index;
    size_t end_index;
    std::string number_text;

    if (!value)
        return (FT_FAILURE);
    if (lsp_find_json_key(text, key, search_start, &index) != FT_SUCCESS)
        return (FT_FAILURE);
    end_index = index;
    while (end_index < text.size() && (text[end_index] == '-' || std::isdigit(
                static_cast<unsigned char>(text[end_index])) != 0))
        end_index += 1;
    if (end_index == index)
        return (FT_FAILURE);
    number_text = text.substr(index, end_index - index);
    *value = std::atoi(number_text.c_str());
    return (FT_SUCCESS);
}

static std::string lsp_json_escape(const std::string &text)
{
    std::string escaped;
    size_t index;

    index = 0;
    while (index < text.size())
    {
        if (text[index] == '\\')
            escaped += "\\\\";
        else if (text[index] == '"')
            escaped += "\\\"";
        else if (text[index] == '\n')
            escaped += "\\n";
        else if (text[index] == '\r')
            escaped += "\\r";
        else if (text[index] == '\t')
            escaped += "\\t";
        else
            escaped.push_back(text[index]);
        index += 1;
    }
    return (escaped);
}

static std::string lsp_path_from_uri(const std::string &uri)
{
    std::string path;
    size_t index;

    if (uri.rfind("file://", 0) == 0)
        path = uri.substr(7);
    else
        path = uri;
    index = 0;
    while (index < path.size())
    {
        if (path[index] == '%' && index + 2 < path.size())
        {
            char buffer[3];
            char *end;
            long value;

            buffer[0] = path[index + 1];
            buffer[1] = path[index + 2];
            buffer[2] = '\0';
            value = std::strtol(buffer, &end, 16);
            if (end && *end == '\0')
            {
                path.replace(index, 3, 1, static_cast<char>(value));
            }
        }
        index += 1;
    }
    return (path);
}

static void lsp_send_message(const std::string &payload)
{
    std::cout << "Content-Length: " << payload.size() << "\r\n\r\n";
    std::cout << payload;
    std::cout.flush();
}

static void lsp_send_response(const std::string &id, const std::string &result_json)
{
    std::string payload;

    payload = "{\"jsonrpc\":\"2.0\",\"id\":";
    payload += id;
    payload += ",\"result\":";
    payload += result_json;
    payload += "}";
    lsp_send_message(payload);
}

static void lsp_send_notification(const char *method, const std::string &params_json)
{
    std::string payload;

    payload = "{\"jsonrpc\":\"2.0\",\"method\":\"";
    payload += method;
    payload += "\",\"params\":";
    payload += params_json;
    payload += "}";
    lsp_send_message(payload);
}

static int lsp_extract_request_id(const std::string &payload, std::string *id)
{
    size_t index;
    size_t end_index;
    std::string parsed_string;

    if (!id)
        return (FT_FAILURE);
    if (lsp_find_json_key(payload, "id", 0, &index) != FT_SUCCESS)
        return (FT_FAILURE);
    if (payload[index] == '"')
    {
        if (lsp_parse_json_string(payload, &index, &parsed_string) != FT_SUCCESS)
            return (FT_FAILURE);
        *id = "\"";
        *id += lsp_json_escape(parsed_string);
        *id += "\"";
        return (FT_SUCCESS);
    }
    end_index = index;
    while (end_index < payload.size() && payload[end_index] != ',' && payload[end_index] != '}'
        && std::isspace(static_cast<unsigned char>(payload[end_index])) == 0)
        end_index += 1;
    if (end_index == index)
        return (FT_FAILURE);
    *id = payload.substr(index, end_index - index);
    return (FT_SUCCESS);
}

static t_lsp_document *lsp_find_document(std::vector<t_lsp_document *> *documents,
    const std::string &uri)
{
    size_t index;

    if (!documents)
        return (NULL);
    index = 0;
    while (index < documents->size())
    {
        if ((*documents)[index] && (*documents)[index]->uri == uri)
            return ((*documents)[index]);
        index += 1;
    }
    return (NULL);
}

static t_lsp_document *lsp_find_document_by_path(std::vector<t_lsp_document *> *documents,
    const std::string &path)
{
    size_t index;

    if (!documents)
        return (NULL);
    index = 0;
    while (index < documents->size())
    {
        if ((*documents)[index] && (*documents)[index]->path == path)
            return ((*documents)[index]);
        index += 1;
    }
    return (NULL);
}

static std::string lsp_uri_from_path(const std::string &path)
{
    return ("file://" + path);
}

static std::string lsp_parent_directory(const std::string &path)
{
    size_t slash_index;

    slash_index = path.find_last_of('/');
    if (slash_index == std::string::npos)
        return (".");
    if (slash_index == 0)
        return ("/");
    return (path.substr(0, slash_index));
}

static std::string lsp_resolve_import_path(const std::string &base_path,
    const char *import_path)
{
    if (!import_path || import_path[0] == '\0')
        return ("");
    if (import_path[0] == '/')
        return (import_path);
    return (lsp_parent_directory(base_path) + "/" + import_path);
}

static int lsp_read_file_text(const std::string &path, std::string *text)
{
    std::ifstream file;
    std::string line;
    std::string content;

    if (!text)
        return (FT_FAILURE);
    file.open(path.c_str(), std::ios::in | std::ios::binary);
    if (!file.is_open())
        return (FT_FAILURE);
    while (std::getline(file, line))
    {
        content += line;
        if (!file.eof())
            content += "\n";
    }
    *text = content;
    return (FT_SUCCESS);
}

static void lsp_dispose_document(t_lsp_document *document)
{
    if (!document)
        return ;
    if (document->analysis_initialized)
        cblc_frontend_analysis_dispose(&document->analysis);
    delete document;
}

static t_lsp_document *lsp_require_document(std::vector<t_lsp_document *> *documents,
    const std::string &uri, const std::string &path)
{
    t_lsp_document *document;

    document = lsp_find_document(documents, uri);
    if (document)
        return (document);
    document = new t_lsp_document();
    document->uri = uri;
    document->path = path;
    document->text.clear();
    document->analysis_initialized = 0;
    if (cblc_frontend_analysis_init(&document->analysis) != FT_SUCCESS)
    {
        delete document;
        return (NULL);
    }
    document->analysis_initialized = 1;
    documents->push_back(document);
    return (document);
}

static t_lsp_document *lsp_require_document_from_path(std::vector<t_lsp_document *> *documents,
    const std::string &path)
{
    t_lsp_document *document;
    std::string text;

    document = lsp_find_document_by_path(documents, path);
    if (document)
        return (document);
    document = lsp_require_document(documents, lsp_uri_from_path(path), path);
    if (!document)
        return (NULL);
    if (lsp_read_file_text(path, &text) != FT_SUCCESS)
        return (document);
    document->text = text;
    (void)lsp_reanalyze_document(document);
    return (document);
}

static std::string lsp_range_from_span(const t_transpiler_source_span *span)
{
    size_t start_line;
    size_t start_column;
    size_t end_line;
    size_t end_column;

    if (!span)
        return ("{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":0}}");
    start_line = span->start_line == 0 ? 0 : span->start_line - 1;
    start_column = span->start_column == 0 ? 0 : span->start_column - 1;
    end_line = span->end_line == 0 ? start_line : span->end_line - 1;
    end_column = span->end_column == 0 ? start_column : span->end_column - 1;
    return ("{\"start\":{\"line\":" + std::to_string(start_line)
        + ",\"character\":" + std::to_string(start_column)
        + "},\"end\":{\"line\":" + std::to_string(end_line)
        + ",\"character\":" + std::to_string(end_column) + "}}");
}

static std::string lsp_diagnostics_json(const t_lsp_document *document)
{
    std::string diagnostics;
    size_t index;

    diagnostics = "{\"uri\":\"";
    diagnostics += lsp_json_escape(document->uri);
    diagnostics += "\",\"diagnostics\":[";
    index = 0;
    while (index < document->analysis.diagnostics.count)
    {
        if (index > 0)
            diagnostics += ",";
        diagnostics += "{\"range\":";
        diagnostics += lsp_range_from_span(&document->analysis.diagnostics.items[index].span);
        diagnostics += ",\"severity\":1,\"source\":\"cblc\",\"message\":\"";
        diagnostics += lsp_json_escape(document->analysis.diagnostics.items[index].message);
        diagnostics += "\"}";
        index += 1;
    }
    diagnostics += "]}";
    return (diagnostics);
}

static void lsp_publish_diagnostics(const t_lsp_document *document)
{
    if (!document)
        return ;
    lsp_send_notification("textDocument/publishDiagnostics",
        lsp_diagnostics_json(document));
}

static int lsp_reanalyze_document(t_lsp_document *document)
{
    if (!document)
        return (FT_FAILURE);
    if (cblc_frontend_analyze_document(&document->analysis, document->path.c_str(),
            document->text.c_str()) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int lsp_label_equals(const std::string &left, const std::string &right)
{
    size_t index;

    index = 0;
    while (index < left.size() && index < right.size())
    {
        if (std::tolower(static_cast<unsigned char>(left[index]))
            != std::tolower(static_cast<unsigned char>(right[index])))
            return (0);
        index += 1;
    }
    if (index != left.size() || index != right.size())
        return (0);
    return (1);
}

static int lsp_label_matches_prefix(const std::string &label, const std::string &prefix)
{
    size_t index;

    if (prefix.empty())
        return (1);
    if (prefix.size() > label.size())
        return (0);
    index = 0;
    while (index < prefix.size())
    {
        if (std::tolower(static_cast<unsigned char>(label[index]))
            != std::tolower(static_cast<unsigned char>(prefix[index])))
            return (0);
        index += 1;
    }
    return (1);
}

static int lsp_extract_identifier_at_position(const std::string &text, int line, int character,
    std::string *identifier)
{
    size_t current_line;
    size_t current_column;
    size_t offset;
    size_t start;
    size_t end;

    if (!identifier || line < 0 || character < 0)
        return (FT_FAILURE);
    current_line = 0;
    current_column = 0;
    offset = 0;
    while (offset < text.size())
    {
        if (current_line == static_cast<size_t>(line)
            && current_column == static_cast<size_t>(character))
            break ;
        if (text[offset] == '\n')
        {
            current_line += 1;
            current_column = 0;
        }
        else
            current_column += 1;
        offset += 1;
    }
    start = offset;
    while (start > 0 && (std::isalnum(static_cast<unsigned char>(text[start - 1])) != 0
            || text[start - 1] == '_' || text[start - 1] == ':'))
        start -= 1;
    end = offset;
    while (end < text.size() && (std::isalnum(static_cast<unsigned char>(text[end])) != 0
            || text[end] == '_' || text[end] == ':'))
        end += 1;
    if (end <= start)
        return (FT_FAILURE);
    *identifier = text.substr(start, end - start);
    return (FT_SUCCESS);
}

static int lsp_extract_completion_prefix(const std::string &text, int line, int character,
    std::string *prefix, int *is_general)
{
    size_t current_line;
    size_t current_column;
    size_t offset;
    size_t start;

    if (!prefix || !is_general || line < 0 || character < 0)
        return (FT_FAILURE);
    current_line = 0;
    current_column = 0;
    offset = 0;
    while (offset < text.size())
    {
        if (current_line == static_cast<size_t>(line)
            && current_column == static_cast<size_t>(character))
            break ;
        if (text[offset] == '\n')
        {
            current_line += 1;
            current_column = 0;
        }
        else
            current_column += 1;
        offset += 1;
    }
    start = offset;
    while (start > 0 && (std::isalnum(static_cast<unsigned char>(text[start - 1])) != 0
            || text[start - 1] == '_' || text[start - 1] == ':'))
        start -= 1;
    *prefix = text.substr(start, offset - start);
    *is_general = 1;
    if (start > 0 && text[start - 1] == '.')
        *is_general = 0;
    if (prefix->rfind("std::", 0) == 0)
        *is_general = 0;
    return (FT_SUCCESS);
}

static t_lsp_document *lsp_find_imported_definition_document(
    std::vector<t_lsp_document *> *documents, const t_lsp_document *document,
    const std::string &identifier, t_transpiler_source_span *span)
{
    size_t index;
    t_lsp_document *imported_document;
    t_cblc_document_symbol_list symbols;
    size_t symbol_index;
    std::string import_path;

    if (!documents || !document || !span)
        return (NULL);
    index = 0;
    while (index < document->analysis.unit->import_count)
    {
        import_path = lsp_resolve_import_path(document->path,
                document->analysis.unit->imports[index].path);
        imported_document = lsp_require_document_from_path(documents, import_path);
        if (imported_document && cblc_document_symbol_list_init(&symbols) == FT_SUCCESS)
        {
            if (cblc_frontend_collect_document_symbols(&imported_document->analysis, &symbols)
                == FT_SUCCESS)
            {
                symbol_index = 0;
                while (symbol_index < symbols.count)
                {
                    if ((symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_FUNCTION
                            || symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_CLASS
                            || symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_STRUCT)
                        && lsp_label_equals(symbols.items[symbol_index].name, identifier))
                    {
                        *span = symbols.items[symbol_index].span;
                        cblc_document_symbol_list_dispose(&symbols);
                        return (imported_document);
                    }
                    symbol_index += 1;
                }
            }
            cblc_document_symbol_list_dispose(&symbols);
        }
        index += 1;
    }
    return (NULL);
}

static int lsp_completion_item_kind(t_cblc_completion_item_kind kind)
{
    if (kind == CBLC_COMPLETION_ITEM_KEYWORD)
        return (14);
    if (kind == CBLC_COMPLETION_ITEM_CLASS)
        return (7);
    if (kind == CBLC_COMPLETION_ITEM_STRUCT)
        return (22);
    if (kind == CBLC_COMPLETION_ITEM_FUNCTION)
        return (3);
    if (kind == CBLC_COMPLETION_ITEM_DATA_ITEM)
        return (6);
    return (1);
}

static int lsp_document_symbol_kind(t_cblc_document_symbol_kind kind)
{
    if (kind == CBLC_DOCUMENT_SYMBOL_CLASS)
        return (5);
    if (kind == CBLC_DOCUMENT_SYMBOL_STRUCT)
        return (23);
    if (kind == CBLC_DOCUMENT_SYMBOL_FUNCTION)
        return (12);
    if (kind == CBLC_DOCUMENT_SYMBOL_DATA_ITEM)
        return (13);
    return (13);
}

static int lsp_semantic_token_kind(t_cblc_semantic_token_kind kind)
{
    if (kind == CBLC_SEMANTIC_TOKEN_KEYWORD)
        return (0);
    if (kind == CBLC_SEMANTIC_TOKEN_TYPE)
        return (1);
    if (kind == CBLC_SEMANTIC_TOKEN_FUNCTION)
        return (2);
    if (kind == CBLC_SEMANTIC_TOKEN_VARIABLE)
        return (3);
    if (kind == CBLC_SEMANTIC_TOKEN_STRING)
        return (4);
    if (kind == CBLC_SEMANTIC_TOKEN_NUMBER)
        return (5);
    if (kind == CBLC_SEMANTIC_TOKEN_COMMENT)
        return (6);
    return (3);
}

static std::string lsp_hover_result_json(const t_lsp_document *document, int line, int character)
{
    char hover[256];

    if (!document)
        return ("null");
    if (cblc_frontend_get_hover(&document->analysis, static_cast<size_t>(line + 1),
            static_cast<size_t>(character + 1), hover, sizeof(hover)) != FT_SUCCESS)
        return ("null");
    return ("{\"contents\":{\"kind\":\"plaintext\",\"value\":\""
        + lsp_json_escape(hover) + "\"}}");
}

static std::string lsp_definition_result_json(std::vector<t_lsp_document *> *documents,
    const t_lsp_document *document, int line, int character)
{
    t_transpiler_source_span span;
    t_lsp_document *imported_document;
    std::string identifier;

    if (!document)
        return ("[]");
    ft_bzero(&span, sizeof(span));
    if (cblc_frontend_find_definition(&document->analysis, static_cast<size_t>(line + 1),
            static_cast<size_t>(character + 1), &span) == FT_SUCCESS)
    {
        return ("[{\"uri\":\"" + lsp_json_escape(document->uri) + "\",\"range\":"
        + lsp_range_from_span(&span) + "}]");
    }
    if (lsp_extract_identifier_at_position(document->text, line, character, &identifier)
        != FT_SUCCESS)
        return ("[]");
    imported_document = lsp_find_imported_definition_document(documents, document, identifier, &span);
    if (!imported_document)
        return ("[]");
    return ("[{\"uri\":\"" + lsp_json_escape(imported_document->uri) + "\",\"range\":"
        + lsp_range_from_span(&span) + "}]");
}

static std::string lsp_completion_result_json(const t_lsp_document *document, int line,
    int character, std::vector<t_lsp_document *> *documents)
{
    t_cblc_completion_list completions;
    std::string result;
    size_t index;
    std::vector<std::string> imported_labels;
    std::vector<int> imported_kinds;
    size_t import_index;
    t_lsp_document *imported_document;
    t_cblc_document_symbol_list symbols;
    size_t symbol_index;
    std::string import_path;
    std::string prefix;
    int is_general;
    size_t existing_index;
    int already_present;

    if (!document)
        return ("[]");
    if (cblc_completion_list_init(&completions) != FT_SUCCESS)
        return ("[]");
    if (cblc_frontend_complete(&document->analysis, static_cast<size_t>(line + 1),
            static_cast<size_t>(character + 1), &completions) != FT_SUCCESS)
    {
        cblc_completion_list_dispose(&completions);
        return ("[]");
    }
    prefix.clear();
    is_general = 0;
    if (documents && lsp_extract_completion_prefix(document->text, line, character, &prefix,
            &is_general) == FT_SUCCESS && is_general)
    {
        import_index = 0;
        while (import_index < document->analysis.unit->import_count)
        {
            import_path = lsp_resolve_import_path(document->path,
                    document->analysis.unit->imports[import_index].path);
            imported_document = lsp_require_document_from_path(documents, import_path);
            if (imported_document && cblc_document_symbol_list_init(&symbols) == FT_SUCCESS)
            {
                if (cblc_frontend_collect_document_symbols(&imported_document->analysis, &symbols)
                    == FT_SUCCESS)
                {
                    symbol_index = 0;
                    while (symbol_index < symbols.count)
                    {
                        if ((symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_FUNCTION
                                || symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_CLASS
                                || symbols.items[symbol_index].kind == CBLC_DOCUMENT_SYMBOL_STRUCT)
                            && lsp_label_matches_prefix(symbols.items[symbol_index].name, prefix))
                        {
                            already_present = 0;
                            existing_index = 0;
                            while (existing_index < completions.count)
                            {
                                if (lsp_label_equals(completions.items[existing_index].label,
                                        symbols.items[symbol_index].name))
                                {
                                    already_present = 1;
                                    break ;
                                }
                                existing_index += 1;
                            }
                            existing_index = 0;
                            while (!already_present && existing_index < imported_labels.size())
                            {
                                if (lsp_label_equals(imported_labels[existing_index],
                                        symbols.items[symbol_index].name))
                                {
                                    already_present = 1;
                                    break ;
                                }
                                existing_index += 1;
                            }
                            if (!already_present)
                            {
                                imported_labels.push_back(symbols.items[symbol_index].name);
                                imported_kinds.push_back(lsp_document_symbol_kind(
                                        symbols.items[symbol_index].kind));
                            }
                        }
                        symbol_index += 1;
                    }
                }
                cblc_document_symbol_list_dispose(&symbols);
            }
            import_index += 1;
        }
    }
    result = "[";
    index = 0;
    while (index < completions.count)
    {
        if (index > 0)
            result += ",";
        result += "{\"label\":\"";
        result += lsp_json_escape(completions.items[index].label);
        result += "\",\"kind\":";
        result += std::to_string(lsp_completion_item_kind(completions.items[index].kind));
        result += "}";
        index += 1;
    }
    index = 0;
    while (index < imported_labels.size())
    {
        if (result.size() > 1)
            result += ",";
        result += "{\"label\":\"";
        result += lsp_json_escape(imported_labels[index]);
        result += "\",\"kind\":";
        result += std::to_string(imported_kinds[index]);
        result += "}";
        index += 1;
    }
    result += "]";
    cblc_completion_list_dispose(&completions);
    return (result);
}

static std::string lsp_document_symbols_result_json(const t_lsp_document *document)
{
    t_cblc_document_symbol_list symbols;
    std::string result;
    size_t index;

    if (!document)
        return ("[]");
    if (cblc_document_symbol_list_init(&symbols) != FT_SUCCESS)
        return ("[]");
    if (cblc_frontend_collect_document_symbols(&document->analysis, &symbols) != FT_SUCCESS)
    {
        cblc_document_symbol_list_dispose(&symbols);
        return ("[]");
    }
    result = "[";
    index = 0;
    while (index < symbols.count)
    {
        if (index > 0)
            result += ",";
        result += "{\"name\":\"";
        result += lsp_json_escape(symbols.items[index].name);
        result += "\",\"kind\":";
        result += std::to_string(lsp_document_symbol_kind(symbols.items[index].kind));
        result += ",\"range\":";
        result += lsp_range_from_span(&symbols.items[index].span);
        result += ",\"selectionRange\":";
        result += lsp_range_from_span(&symbols.items[index].span);
        result += "}";
        index += 1;
    }
    result += "]";
    cblc_document_symbol_list_dispose(&symbols);
    return (result);
}

static std::string lsp_references_result_json(const t_lsp_document *document, int line,
    int character)
{
    t_cblc_source_span_list references;
    std::string result;
    size_t index;

    if (!document)
        return ("[]");
    if (cblc_source_span_list_init(&references) != FT_SUCCESS)
        return ("[]");
    if (cblc_frontend_find_references(&document->analysis, static_cast<size_t>(line + 1),
            static_cast<size_t>(character + 1), &references) != FT_SUCCESS)
    {
        cblc_source_span_list_dispose(&references);
        return ("[]");
    }
    result = "[";
    index = 0;
    while (index < references.count)
    {
        if (index > 0)
            result += ",";
        result += "{\"uri\":\"";
        result += lsp_json_escape(document->uri);
        result += "\",\"range\":";
        result += lsp_range_from_span(&references.items[index]);
        result += "}";
        index += 1;
    }
    result += "]";
    cblc_source_span_list_dispose(&references);
    return (result);
}

static std::string lsp_rename_result_json(const t_lsp_document *document, int line, int character,
    const std::string &new_name)
{
    t_cblc_source_span_list edits;
    std::string result;
    size_t index;

    if (!document)
        return ("null");
    if (cblc_source_span_list_init(&edits) != FT_SUCCESS)
        return ("null");
    if (cblc_frontend_prepare_rename(&document->analysis, static_cast<size_t>(line + 1),
            static_cast<size_t>(character + 1), new_name.c_str(), &edits) != FT_SUCCESS)
    {
        cblc_source_span_list_dispose(&edits);
        return ("null");
    }
    result = "{\"changes\":{\"";
    result += lsp_json_escape(document->uri);
    result += "\":[";
    index = 0;
    while (index < edits.count)
    {
        if (index > 0)
            result += ",";
        result += "{\"range\":";
        result += lsp_range_from_span(&edits.items[index]);
        result += ",\"newText\":\"";
        result += lsp_json_escape(new_name);
        result += "\"}";
        index += 1;
    }
    result += "]}}";
    cblc_source_span_list_dispose(&edits);
    return (result);
}

static std::string lsp_semantic_tokens_result_json(const t_lsp_document *document)
{
    t_cblc_semantic_token_list tokens;
    std::string result;
    size_t index;
    size_t previous_line;
    size_t previous_start;
    size_t line;
    size_t start;
    size_t length;
    size_t delta_line;
    size_t delta_start;

    if (!document)
        return ("{\"data\":[]}");
    if (cblc_semantic_token_list_init(&tokens) != FT_SUCCESS)
        return ("{\"data\":[]}");
    if (cblc_frontend_collect_semantic_tokens(&document->analysis, &tokens) != FT_SUCCESS)
    {
        cblc_semantic_token_list_dispose(&tokens);
        return ("{\"data\":[]}");
    }
    result = "{\"data\":[";
    previous_line = 0;
    previous_start = 0;
    index = 0;
    while (index < tokens.count)
    {
        line = 0;
        start = 0;
        length = 0;
        if (tokens.items[index].span.start_line > 0)
            line = tokens.items[index].span.start_line - 1;
        if (tokens.items[index].span.start_column > 0)
            start = tokens.items[index].span.start_column - 1;
        if (tokens.items[index].span.end_column >= tokens.items[index].span.start_column)
            length = tokens.items[index].span.end_column - tokens.items[index].span.start_column;
        delta_line = line;
        delta_start = start;
        if (index > 0)
        {
            if (line >= previous_line)
                delta_line = line - previous_line;
            if (delta_line == 0 && start >= previous_start)
                delta_start = start - previous_start;
        }
        if (index > 0)
            result += ",";
        result += std::to_string(delta_line);
        result += ",";
        result += std::to_string(delta_start);
        result += ",";
        result += std::to_string(length);
        result += ",";
        result += std::to_string(lsp_semantic_token_kind(tokens.items[index].kind));
        result += ",0";
        previous_line = line;
        previous_start = start;
        index += 1;
    }
    result += "]}";
    cblc_semantic_token_list_dispose(&tokens);
    return (result);
}

static std::string lsp_initialize_result_json(void)
{
    return ("{\"capabilities\":{\"textDocumentSync\":1,\"hoverProvider\":true,"
        "\"definitionProvider\":true,\"referencesProvider\":true,"
        "\"renameProvider\":true,\"documentSymbolProvider\":true,"
        "\"semanticTokensProvider\":{\"full\":true,\"legend\":{\"tokenTypes\":["
        "\"keyword\",\"type\",\"function\",\"variable\",\"string\",\"number\",\"comment\"],"
        "\"tokenModifiers\":[]}},"
        "\"completionProvider\":{\"resolveProvider\":false}}}");
}

static int lsp_handle_did_open(const std::string &payload,
    std::vector<t_lsp_document *> *documents)
{
    std::string uri;
    std::string text;
    t_lsp_document *document;

    if (lsp_find_json_string_value(payload, "uri", 0, &uri) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lsp_find_json_string_value(payload, "text", 0, &text) != FT_SUCCESS)
        return (FT_FAILURE);
    document = lsp_require_document(documents, uri, lsp_path_from_uri(uri));
    if (!document)
        return (FT_FAILURE);
    document->text = text;
    (void)lsp_reanalyze_document(document);
    lsp_publish_diagnostics(document);
    return (FT_SUCCESS);
}

static int lsp_handle_did_change(const std::string &payload,
    std::vector<t_lsp_document *> *documents)
{
    std::string uri;
    size_t content_changes_index;
    std::string text;
    t_lsp_document *document;

    if (lsp_find_json_string_value(payload, "uri", 0, &uri) != FT_SUCCESS)
        return (FT_FAILURE);
    content_changes_index = payload.find("\"contentChanges\"");
    if (content_changes_index == std::string::npos)
        return (FT_FAILURE);
    if (lsp_find_json_string_value(payload, "text", content_changes_index, &text) != FT_SUCCESS)
        return (FT_FAILURE);
    document = lsp_require_document(documents, uri, lsp_path_from_uri(uri));
    if (!document)
        return (FT_FAILURE);
    document->text = text;
    (void)lsp_reanalyze_document(document);
    lsp_publish_diagnostics(document);
    return (FT_SUCCESS);
}

static std::string lsp_extract_text_document_uri(const std::string &payload)
{
    size_t text_document_index;
    std::string uri;

    text_document_index = payload.find("\"textDocument\"");
    if (text_document_index == std::string::npos)
        return ("");
    if (lsp_find_json_string_value(payload, "uri", text_document_index, &uri) != FT_SUCCESS)
        return ("");
    return (uri);
}

static int lsp_extract_position(const std::string &payload, int *line, int *character)
{
    size_t position_index;

    if (!line || !character)
        return (FT_FAILURE);
    position_index = payload.find("\"position\"");
    if (position_index == std::string::npos)
        return (FT_FAILURE);
    if (lsp_find_json_integer_value(payload, "line", position_index, line) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lsp_find_json_integer_value(payload, "character", position_index, character) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static std::string lsp_read_message(void)
{
    std::string line;
    size_t content_length;
    std::string payload;

    content_length = 0;
    while (std::getline(std::cin, line))
    {
        if (!line.empty() && line[line.size() - 1] == '\r')
            line.resize(line.size() - 1);
        if (line.empty())
            break ;
        if (line.rfind("Content-Length:", 0) == 0)
            content_length = std::strtoul(line.c_str() + 15, NULL, 10);
    }
    if (content_length == 0)
        return ("");
    payload.resize(content_length);
    std::cin.read(&payload[0], static_cast<std::streamsize>(content_length));
    if (std::cin.gcount() != static_cast<std::streamsize>(content_length))
        return ("");
    return (payload);
}

static int lsp_loop(void)
{
    std::vector<t_lsp_document *> documents;
    int running;

    running = 1;
    while (running)
    {
        std::string payload;
        std::string method;
        std::string id;

        payload = lsp_read_message();
        if (payload.empty())
            break ;
        if (lsp_find_json_string_value(payload, "method", 0, &method) != FT_SUCCESS)
            continue ;
        (void)lsp_extract_request_id(payload, &id);
        if (method == "initialize")
        {
            lsp_send_response(id, lsp_initialize_result_json());
        }
        else if (method == "initialized")
        {
        }
        else if (method == "shutdown")
        {
            lsp_send_response(id, "null");
        }
        else if (method == "exit")
        {
            running = 0;
        }
        else if (method == "textDocument/didOpen")
        {
            (void)lsp_handle_did_open(payload, &documents);
        }
        else if (method == "textDocument/didChange")
        {
            (void)lsp_handle_did_change(payload, &documents);
        }
        else if (method == "textDocument/hover")
        {
            std::string uri;
            t_lsp_document *document;
            int line;
            int character;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            line = 0;
            character = 0;
            if (document && lsp_extract_position(payload, &line, &character) == FT_SUCCESS)
                lsp_send_response(id, lsp_hover_result_json(document, line, character));
            else
                lsp_send_response(id, "null");
        }
        else if (method == "textDocument/definition")
        {
            std::string uri;
            t_lsp_document *document;
            int line;
            int character;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            line = 0;
            character = 0;
            if (document && lsp_extract_position(payload, &line, &character) == FT_SUCCESS)
                lsp_send_response(id, lsp_definition_result_json(&documents, document, line,
                        character));
            else
                lsp_send_response(id, "[]");
        }
        else if (method == "textDocument/references")
        {
            std::string uri;
            t_lsp_document *document;
            int line;
            int character;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            line = 0;
            character = 0;
            if (document && lsp_extract_position(payload, &line, &character) == FT_SUCCESS)
                lsp_send_response(id, lsp_references_result_json(document, line, character));
            else
                lsp_send_response(id, "[]");
        }
        else if (method == "textDocument/completion")
        {
            std::string uri;
            t_lsp_document *document;
            int line;
            int character;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            line = 0;
            character = 0;
            if (document && lsp_extract_position(payload, &line, &character) == FT_SUCCESS)
                lsp_send_response(id, lsp_completion_result_json(document, line, character,
                        &documents));
            else
                lsp_send_response(id, "[]");
        }
        else if (method == "textDocument/rename")
        {
            std::string uri;
            std::string new_name;
            t_lsp_document *document;
            int line;
            int character;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            line = 0;
            character = 0;
            new_name.clear();
            if (document && lsp_extract_position(payload, &line, &character) == FT_SUCCESS
                && lsp_find_json_string_value(payload, "newName", 0, &new_name) == FT_SUCCESS)
                lsp_send_response(id, lsp_rename_result_json(document, line, character, new_name));
            else
                lsp_send_response(id, "null");
        }
        else if (method == "textDocument/documentSymbol")
        {
            std::string uri;
            t_lsp_document *document;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            if (document)
                lsp_send_response(id, lsp_document_symbols_result_json(document));
            else
                lsp_send_response(id, "[]");
        }
        else if (method == "textDocument/semanticTokens/full")
        {
            std::string uri;
            t_lsp_document *document;

            uri = lsp_extract_text_document_uri(payload);
            document = lsp_find_document(&documents, uri);
            if (document)
                lsp_send_response(id, lsp_semantic_tokens_result_json(document));
            else
                lsp_send_response(id, "{\"data\":[]}");
        }
        else
        {
            if (!id.empty())
                lsp_send_response(id, "null");
        }
    }
    while (!documents.empty())
    {
        lsp_dispose_document(documents.back());
        documents.pop_back();
    }
    return (FT_SUCCESS);
}

int main(void)
{
    if (lsp_loop() != FT_SUCCESS)
        return (EXIT_FAILURE);
    return (EXIT_SUCCESS);
}
