#include "cblc_transpiler.hpp"

static int cblc_function_has_return_statement(const t_cblc_function *function)
{
    size_t index;

    if (!function)
        return (0);
    index = 0;
    while (index < function->statement_count)
    {
        if (function->statements[index].type == CBLC_STATEMENT_RETURN)
            return (1);
        index += 1;
    }
    return (0);
}

int transpiler_validate_generated_cblc(const char *text)
{
    t_cblc_translation_unit unit;
    size_t index;

    if (!text)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(text, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (unit.function_count == 0)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    else
    {
        size_t entry_index;

        entry_index = unit.entry_function_index;
        if (entry_index == static_cast<size_t>(-1) || entry_index >= unit.function_count)
            entry_index = 0;
        if (unit.functions[entry_index].return_kind != CBLC_FUNCTION_RETURN_VOID
            && !cblc_function_has_return_statement(&unit.functions[entry_index]))
        {
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
    }
    index = 0;
    while (index < unit.function_count)
    {
        if (std::strncmp(unit.functions[index].source_name, "main",
                sizeof(unit.functions[index].source_name)) == 0
            && unit.functions[index].return_kind != CBLC_FUNCTION_RETURN_VOID)
        {
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        if (unit.functions[index].return_kind != CBLC_FUNCTION_RETURN_VOID
            && !cblc_function_has_return_statement(&unit.functions[index]))
        {
            cblc_translation_unit_dispose(&unit);
            return (FT_FAILURE);
        }
        index += 1;
    }
    cblc_translation_unit_dispose(&unit);
    return (FT_SUCCESS);
}

static int transpiler_validation_identifier_equals(const t_lexer_token *token,
    const char *identifier)
{
    size_t index;

    if (!token || !token->lexeme || !identifier
        || token->length != std::strlen(identifier))
        return (0);
    index = 0;
    while (index < token->length)
    {
        char left;
        char right;

        left = token->lexeme[index];
        right = identifier[index];
        if (left >= 'a' && left <= 'z')
            left = static_cast<char>(left - 'a' + 'A');
        if (right >= 'a' && right <= 'z')
            right = static_cast<char>(right - 'a' + 'A');
        if (left != right)
            return (0);
        index += 1;
    }
    return (1);
}

static int transpiler_validation_has_paragraph(const t_ast_node *procedure_division,
    const char *target)
{
    size_t index;

    if (!procedure_division || !target)
        return (0);
    index = 0;
    while (index < ast_node_child_count(procedure_division))
    {
        const t_ast_node *paragraph;

        paragraph = ast_node_get_child(procedure_division, index);
        if (paragraph && paragraph->kind == AST_NODE_PARAGRAPH
            && transpiler_validation_identifier_equals(&paragraph->token, target))
            return (1);
        index += 1;
    }
    return (0);
}

static int transpiler_validation_has_duplicate_paragraphs(
    const t_ast_node *procedure_division, char *duplicate_name,
    size_t duplicate_name_size)
{
    size_t index;

    if (!procedure_division)
        return (1);
    index = 0;
    while (index < ast_node_child_count(procedure_division))
    {
        const t_ast_node *paragraph;
        size_t other_index;

        paragraph = ast_node_get_child(procedure_division, index);
        if (!paragraph || paragraph->kind != AST_NODE_PARAGRAPH)
        {
            index += 1;
            continue ;
        }
        other_index = index + 1;
        while (other_index < ast_node_child_count(procedure_division))
        {
            const t_ast_node *other_paragraph;

            other_paragraph = ast_node_get_child(procedure_division, other_index);
            if (other_paragraph && other_paragraph->kind == AST_NODE_PARAGRAPH
                && paragraph->token.length == other_paragraph->token.length
                && paragraph->token.length > 0
                && transpiler_validation_identifier_equals(&paragraph->token,
                    other_paragraph->token.lexeme))
            {
                if (duplicate_name && duplicate_name_size > 0)
                    std::snprintf(duplicate_name, duplicate_name_size, "%s",
                        paragraph->token.lexeme);
                return (1);
            }
            other_index += 1;
        }
        index += 1;
    }
    return (0);
}

static int transpiler_validation_has_unresolved_transfer(const t_ast_node *node,
    const t_ast_node *procedure_division, char *diagnostic,
    size_t diagnostic_size)
{
    size_t index;

    if (!node || !procedure_division)
        return (1);
    if (node->kind == AST_NODE_GO_TO_STATEMENT
        && !transpiler_validation_has_paragraph(procedure_division,
            node->token.lexeme))
    {
        if (diagnostic && diagnostic_size > 0)
            std::snprintf(diagnostic, diagnostic_size,
                "unresolved GO TO target '%s'", node->token.lexeme);
        return (1);
    }
    if (node->kind == AST_NODE_PERFORM_PARAGRAPH_STATEMENT
        && (!transpiler_validation_has_paragraph(procedure_division,
                node->token.lexeme)
            || ast_node_child_count(node) > 1
            || (ast_node_child_count(node) == 1
                && (!ast_node_get_child(node, 0)
                    || !transpiler_validation_has_paragraph(procedure_division,
                        ast_node_get_child(node, 0)->token.lexeme)))))
    {
        if (diagnostic && diagnostic_size > 0)
        {
            const t_ast_node *target;

            target = ast_node_get_child(node, 0);
            if (ast_node_child_count(node) > 1)
                std::snprintf(diagnostic, diagnostic_size,
                    "invalid PERFORM '%s' range with %zu endpoint(s)",
                    node->token.lexeme, ast_node_child_count(node));
            else if (target)
                std::snprintf(diagnostic, diagnostic_size,
                    "unresolved PERFORM range '%s' through '%s' (start=%s, end=%s)",
                    node->token.lexeme, target->token.lexeme,
                    transpiler_validation_has_paragraph(procedure_division,
                        node->token.lexeme) ? "present" : "missing",
                    transpiler_validation_has_paragraph(procedure_division,
                        target->token.lexeme) ? "present" : "missing");
            else
                std::snprintf(diagnostic, diagnostic_size,
                    "unresolved PERFORM target '%s' (start=%s)",
                    node->token.lexeme,
                    transpiler_validation_has_paragraph(procedure_division,
                        node->token.lexeme) ? "present" : "missing");
        }
        return (1);
    }
    index = 0;
    while (index < ast_node_child_count(node))
    {
        if (transpiler_validation_has_unresolved_transfer(
                ast_node_get_child(node, index), procedure_division,
                diagnostic, diagnostic_size))
            return (1);
        index += 1;
    }
    return (0);
}

int transpiler_validate_generated_cobol_with_diagnostic(const char *text,
    char *out_diagnostic, size_t diagnostic_size)
{
    t_parser parser;
    t_ast_node *program;
    const t_ast_node *procedure_division;
    size_t index;

    if (!text)
        return (FT_FAILURE);
    if (out_diagnostic && diagnostic_size > 0)
        out_diagnostic[0] = '\0';
    program = NULL;
    parser_init(&parser, text);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        if (out_diagnostic && diagnostic_size > 0)
            std::snprintf(out_diagnostic, diagnostic_size,
                "COBOL parse error %d at %zu:%zu near '%.80s'",
                parser.last_error, parser.error_token.line,
                parser.error_token.column, parser.error_token.lexeme
                    ? parser.error_token.lexeme : "");
        parser_dispose(&parser);
        return (FT_FAILURE);
    }
    parser_dispose(&parser);
    if (!program)
        return (FT_FAILURE);
    procedure_division = NULL;
    index = 0;
    while (index < ast_node_child_count(program))
    {
        const t_ast_node *child;

        child = ast_node_get_child(program, index);
        if (child && child->kind == AST_NODE_PROCEDURE_DIVISION)
        {
            procedure_division = child;
            break ;
        }
        index += 1;
    }
    if (!procedure_division)
    {
        if (out_diagnostic && diagnostic_size > 0)
            std::snprintf(out_diagnostic, diagnostic_size,
                "missing PROCEDURE DIVISION");
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    {
        char duplicate_name[TRANSPILE_IDENTIFIER_MAX];

        duplicate_name[0] = '\0';
        if (transpiler_validation_has_duplicate_paragraphs(procedure_division,
                duplicate_name, sizeof(duplicate_name)))
        {
            if (out_diagnostic && diagnostic_size > 0)
                std::snprintf(out_diagnostic, diagnostic_size,
                    "duplicate paragraph '%s'", duplicate_name);
            ast_node_destroy(program);
            return (FT_FAILURE);
        }
    }
    if (transpiler_validation_has_unresolved_transfer(procedure_division,
            procedure_division, out_diagnostic, diagnostic_size))
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    ast_node_destroy(program);
    return (FT_SUCCESS);
}

int transpiler_validate_generated_cobol(const char *text)
{
    return (transpiler_validate_generated_cobol_with_diagnostic(text, NULL, 0));
}
