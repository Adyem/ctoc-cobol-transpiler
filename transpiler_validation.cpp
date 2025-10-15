#include "cblc_transpiler.hpp"

int transpiler_validate_generated_cblc(const char *text)
{
    t_cblc_translation_unit unit;

    if (!text)
        return (FT_FAILURE);
    cblc_translation_unit_init(&unit);
    if (cblc_parse_translation_unit(text, &unit) != FT_SUCCESS)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
    }
    if (!unit.saw_return)
    {
        cblc_translation_unit_dispose(&unit);
        return (FT_FAILURE);
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
