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

int transpiler_validate_generated_cobol(const char *text)
{
    t_parser parser;
    t_ast_node *program;

    if (!text)
        return (FT_FAILURE);
    program = NULL;
    parser_init(&parser, text);
    if (parser_parse_program(&parser, &program) != FT_SUCCESS)
    {
        parser_dispose(&parser);
        return (FT_FAILURE);
    }
    parser_dispose(&parser);
    if (!program)
        return (FT_FAILURE);
    ast_node_destroy(program);
    return (FT_SUCCESS);
}
