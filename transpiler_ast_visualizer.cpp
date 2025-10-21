#include "cblc_transpiler.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static const char *g_ast_node_kind_names[] = {
    "Unknown",
    "Program",
    "IdentificationDivision",
    "EnvironmentDivision",
    "DataDivision",
    "ProcedureDivision",
    "ProgramId",
    "WorkingStorageSection",
    "DataItem",
    "CopybookInclude",
    "PictureClause",
    "ValueClause",
    "StatementSequence",
    "Paragraph",
    "MoveStatement",
    "AssignmentStatement",
    "ComputeStatement",
    "IfStatement",
    "PerformUntilStatement",
    "PerformVaryingStatement",
    "OpenStatement",
    "CloseStatement",
    "ReadStatement",
    "WriteStatement",
    "DisplayStatement",
    "StopStatement",
    "Condition",
    "ArithmeticExpression",
    "UnaryExpression",
    "ArithmeticOperator",
    "ComparisonOperator",
    "Identifier",
    "Literal"
};

static const char *transpiler_ast_visualizer_node_kind_name(t_ast_node_kind kind)
{
    size_t index;
    size_t count;

    index = static_cast<size_t>(kind);
    count = sizeof(g_ast_node_kind_names) / sizeof(g_ast_node_kind_names[0]);
    if (index >= count)
        return ("Unknown");
    return (g_ast_node_kind_names[index]);
}

static void transpiler_ast_visualizer_escape_lexeme(const char *source, size_t length,
    char *destination, size_t destination_size)
{
    size_t source_index;
    size_t destination_index;

    if (!destination || destination_size == 0)
        return ;
    destination[0] = '\0';
    if (!source || length == 0)
        return ;
    source_index = 0;
    destination_index = 0;
    while (source_index < length && source[source_index] != '\0')
    {
        char character;

        character = source[source_index];
        if (character == '"' || character == '\\')
        {
            if (destination_index + 2 >= destination_size)
                break ;
            destination[destination_index] = '\\';
            destination[destination_index + 1] = character;
            destination_index += 2;
        }
        else if (character < 32 || character > 126)
        {
            if (destination_index + 1 >= destination_size)
                break ;
            destination[destination_index] = '?';
            destination_index += 1;
        }
        else
        {
            if (destination_index + 1 >= destination_size)
                break ;
            destination[destination_index] = character;
            destination_index += 1;
        }
        source_index += 1;
    }
    if (destination_index >= destination_size)
        destination_index = destination_size - 1;
    destination[destination_index] = '\0';
}

static void transpiler_ast_visualizer_format_label(const t_ast_node *node, char *buffer, size_t buffer_size)
{
    const char *kind_name;
    char escaped[128];

    if (!buffer || buffer_size == 0)
        return ;
    buffer[0] = '\0';
    if (!node)
        return ;
    kind_name = transpiler_ast_visualizer_node_kind_name(node->kind);
    ft_strlcpy(buffer, kind_name, buffer_size);
    escaped[0] = '\0';
    if (node->token.lexeme && node->token.length > 0)
        transpiler_ast_visualizer_escape_lexeme(node->token.lexeme, node->token.length, escaped, sizeof(escaped));
    if (escaped[0] != '\0')
    {
        ft_strlcat(buffer, "\\n", buffer_size);
        ft_strlcat(buffer, escaped, buffer_size);
    }
}

static int transpiler_ast_visualizer_write_literal(t_runtime_file *file, const char *text)
{
    size_t length;

    if (!file || !text)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (runtime_file_write(file, text, length) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int transpiler_ast_visualizer_emit_node(const t_ast_node *node, t_runtime_file *file,
    size_t *counter, size_t *out_identifier)
{
    char line[256];
    char label[192];
    size_t node_id;
    size_t child_index;
    size_t child_id;
    int formatted_length;

    if (!node || !file || !counter || !out_identifier)
        return (FT_FAILURE);
    node_id = *counter;
    *counter += 1;
    transpiler_ast_visualizer_format_label(node, label, sizeof(label));
    formatted_length = pf_snprintf(line, sizeof(line), "    node%lu [label=\"%s\"];\n",
        static_cast<unsigned long>(node_id), label);
    if (formatted_length < 0)
        return (FT_FAILURE);
    if (runtime_file_write(file, line, static_cast<size_t>(formatted_length)) != FT_SUCCESS)
        return (FT_FAILURE);
    child_index = 0;
    while (child_index < node->child_count)
    {
        if (transpiler_ast_visualizer_emit_node(node->children[child_index], file, counter,
                &child_id) != FT_SUCCESS)
            return (FT_FAILURE);
        formatted_length = pf_snprintf(line, sizeof(line), "    node%lu -> node%lu;\n",
            static_cast<unsigned long>(node_id), static_cast<unsigned long>(child_id));
        if (formatted_length < 0)
            return (FT_FAILURE);
        if (runtime_file_write(file, line, static_cast<size_t>(formatted_length)) != FT_SUCCESS)
            return (FT_FAILURE);
        child_index += 1;
    }
    *out_identifier = node_id;
    return (FT_SUCCESS);
}

int transpiler_ast_visualize_program(const t_ast_node *program, const char *path)
{
    t_runtime_file file;
    size_t counter;
    size_t root_id;
    int status;

    if (!program || !path)
        return (FT_FAILURE);
    runtime_file_init(&file);
    if (runtime_file_open_write(&file, path) != FT_SUCCESS)
        return (FT_FAILURE);
    status = FT_FAILURE;
    if (transpiler_ast_visualizer_write_literal(&file, "digraph AST {\n") != FT_SUCCESS)
        goto cleanup;
    if (transpiler_ast_visualizer_write_literal(&file,
            "    node [shape=record, fontname=\"Courier\"];\n") != FT_SUCCESS)
        goto cleanup;
    counter = 0;
    if (transpiler_ast_visualizer_emit_node(program, &file, &counter, &root_id) != FT_SUCCESS)
        goto cleanup;
    if (transpiler_ast_visualizer_write_literal(&file, "}\n") != FT_SUCCESS)
        goto cleanup;
    status = FT_SUCCESS;
cleanup:
    if (runtime_file_close(&file) != FT_SUCCESS)
        status = FT_FAILURE;
    return (status);
}
