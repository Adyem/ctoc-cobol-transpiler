#include "transpiler_stub_cblc.hpp"

#include <cctype>
#include <fstream>
#include <string>
#include <vector>

typedef enum e_stub_statement_kind
{
    STUB_STATEMENT_NONE = 0,
    STUB_STATEMENT_PERFORM,
    STUB_STATEMENT_MOVE,
    STUB_STATEMENT_DISPLAY
}   t_stub_statement_kind;

typedef struct s_stub_variable
{
    std::string name;
    size_t length;
}   t_stub_variable;

typedef struct s_stub_statement
{
    t_stub_statement_kind kind;
    std::string target;
    std::string value;
}   t_stub_statement;

typedef struct s_stub_function
{
    std::string name;
    std::vector<t_stub_statement> statements;
}   t_stub_function;

static std::string transpiler_stub_trim(const std::string &text)
{
    size_t start;
    size_t end;

    start = 0;
    while (start < text.size() && std::isspace(static_cast<unsigned char>(text[start])))
        start += 1;
    end = text.size();
    while (end > start && std::isspace(static_cast<unsigned char>(text[end - 1])))
        end -= 1;
    return (text.substr(start, end - start));
}

static std::string transpiler_stub_upper_hyphenated(const std::string &name)
{
    std::string result;
    size_t index;

    result.clear();
    index = 0;
    while (index < name.size())
    {
        char character;

        character = name[index];
        if (character == '_')
            result.push_back('-');
        else
            result.push_back(static_cast<char>(std::toupper(static_cast<unsigned char>(character))));
        index += 1;
    }
    return (result);
}

static int transpiler_stub_parse_variable(const std::string &line, t_stub_variable *out_variable)
{
    size_t name_start;
    size_t bracket_start;
    size_t bracket_end;
    std::string name;
    std::string length_text;
    size_t index;
    long parsed_length;

    if (!out_variable)
        return (FT_FAILURE);
    name_start = line.find("char ");
    if (name_start != 0)
        return (FT_FAILURE);
    name_start += 5;
    while (name_start < line.size() && std::isspace(static_cast<unsigned char>(line[name_start])))
        name_start += 1;
    bracket_start = line.find('[', name_start);
    if (bracket_start == std::string::npos)
        return (FT_FAILURE);
    name = line.substr(name_start, bracket_start - name_start);
    bracket_start += 1;
    bracket_end = line.find(']', bracket_start);
    if (bracket_end == std::string::npos)
        return (FT_FAILURE);
    length_text = line.substr(bracket_start, bracket_end - bracket_start);
    index = 0;
    while (index < name.size() && std::isspace(static_cast<unsigned char>(name[index])))
        index += 1;
    name = name.substr(index);
    index = name.size();
    while (index > 0 && std::isspace(static_cast<unsigned char>(name[index - 1])))
        index -= 1;
    name = name.substr(0, index);
    if (name.empty())
        return (FT_FAILURE);
    parsed_length = std::strtol(length_text.c_str(), NULL, 10);
    if (parsed_length <= 0)
        return (FT_FAILURE);
    out_variable->name = name;
    out_variable->length = static_cast<size_t>(parsed_length);
    return (FT_SUCCESS);
}

static void transpiler_stub_store_statement(t_stub_function *function, const t_stub_statement &statement)
{
    if (!function)
        return ;
    function->statements.push_back(statement);
}

static void transpiler_stub_parse_assignment(const std::string &line, t_stub_function *function)
{
    size_t equals_pos;
    std::string target;
    std::string value;
    t_stub_statement statement;

    equals_pos = line.find('=');
    if (equals_pos == std::string::npos)
        return ;
    target = transpiler_stub_trim(line.substr(0, equals_pos));
    value = transpiler_stub_trim(line.substr(equals_pos + 1));
    if (!value.empty() && value[value.size() - 1] == ';')
        value = value.substr(0, value.size() - 1);
    value = transpiler_stub_trim(value);
    if (!value.empty() && value[0] == '"' && value[value.size() - 1] == '"')
    {
        statement.kind = STUB_STATEMENT_MOVE;
        statement.target = target;
        statement.value = value.substr(1, value.size() - 2);
        transpiler_stub_store_statement(function, statement);
    }
}

static void transpiler_stub_parse_display(const std::string &line, t_stub_function *function)
{
    size_t start;
    size_t end;
    t_stub_statement statement;

    start = line.find("display(");
    if (start != 0)
        return ;
    end = line.find(')', start + 8);
    if (end == std::string::npos)
        return ;
    statement.kind = STUB_STATEMENT_DISPLAY;
    statement.target = transpiler_stub_trim(line.substr(start + 8, end - (start + 8)));
    statement.value.clear();
    transpiler_stub_store_statement(function, statement);
}

static void transpiler_stub_parse_call(const std::string &line, t_stub_function *function)
{
    std::string stripped;
    size_t open_pos;
    t_stub_statement statement;

    stripped = line;
    if (!stripped.empty() && stripped[stripped.size() - 1] == ';')
        stripped = stripped.substr(0, stripped.size() - 1);
    stripped = transpiler_stub_trim(stripped);
    open_pos = stripped.find('(');
    if (open_pos == std::string::npos)
        return ;
    statement.kind = STUB_STATEMENT_PERFORM;
    statement.target = stripped.substr(0, open_pos);
    statement.value.clear();
    transpiler_stub_store_statement(function, statement);
}

static void transpiler_stub_parse_function_statement(const std::string &line, t_stub_function *function)
{
    std::string trimmed;

    if (!function)
        return ;
    trimmed = transpiler_stub_trim(line);
    if (trimmed.empty() || trimmed == "{" || trimmed == "}")
        return ;
    if (trimmed.find('=') != std::string::npos)
    {
        transpiler_stub_parse_assignment(trimmed, function);
        return ;
    }
    if (trimmed.find("display(") == 0)
    {
        transpiler_stub_parse_display(trimmed, function);
        return ;
    }
    transpiler_stub_parse_call(trimmed, function);
}

static int transpiler_stub_collect_source(const char *input_path, std::vector<t_stub_variable> &variables,
    std::vector<t_stub_function> &functions)
{
    std::ifstream stream;
    std::string line;
    t_stub_function *current_function;

    variables.clear();
    functions.clear();
    current_function = NULL;
    stream.open(input_path);
    if (!stream.is_open())
        return (FT_FAILURE);
    while (std::getline(stream, line))
    {
        std::string trimmed;
        t_stub_variable variable;

        trimmed = transpiler_stub_trim(line);
        if (trimmed.empty())
            continue ;
        if (!current_function && trimmed.find("char ") == 0)
        {
            if (transpiler_stub_parse_variable(trimmed, &variable) == FT_SUCCESS)
                variables.push_back(variable);
            continue ;
        }
        if (!current_function && trimmed.find("function void") == 0)
        {
            size_t name_start;
            size_t name_end;
            t_stub_function function;

            name_start = trimmed.find("void");
            if (name_start == std::string::npos)
                continue ;
            name_start += 4;
            while (name_start < trimmed.size() && std::isspace(static_cast<unsigned char>(trimmed[name_start])))
                name_start += 1;
            name_end = trimmed.find('(', name_start);
            if (name_end == std::string::npos)
                continue ;
            function.name = trimmed.substr(name_start, name_end - name_start);
            function.statements.clear();
            functions.push_back(function);
            current_function = &functions.back();
            continue ;
        }
        if (current_function)
        {
            if (trimmed == "}")
            {
                current_function = NULL;
                continue ;
            }
            transpiler_stub_parse_function_statement(trimmed, current_function);
        }
    }
    stream.close();
    return (FT_SUCCESS);
}

static void transpiler_stub_emit_header(std::string &buffer, const std::string &program_id)
{
    buffer.append("       IDENTIFICATION DIVISION.\n");
    buffer.append("       PROGRAM-ID. ");
    buffer.append(program_id);
    buffer.append(".\n");
}

static void transpiler_stub_emit_data_division(std::string &buffer, const std::vector<t_stub_variable> &variables)
{
    size_t index;
    size_t variable_count;

    buffer.append("       DATA DIVISION.\n");
    buffer.append("       WORKING-STORAGE SECTION.\n");
    variable_count = variables.size();
    index = 0;
    if (variable_count == 0)
        return ;
    while (index < variable_count)
    {
        std::string identifier;

        identifier = transpiler_stub_upper_hyphenated(variables[index].name);
        buffer.append("       01 ");
        buffer.append(identifier);
        buffer.append(" PIC X(");
        buffer.append(std::to_string(static_cast<unsigned long long>(variables[index].length)));
        buffer.append(").\n");
        index += 1;
    }
}

static void transpiler_stub_emit_perform(const t_stub_statement &statement, std::string &buffer)
{
    std::string target;

    target = transpiler_stub_upper_hyphenated(statement.target);
    buffer.append("           PERFORM ");
    buffer.append(target);
    buffer.append(".\n");
}

static void transpiler_stub_emit_move(const t_stub_statement &statement, std::string &buffer)
{
    std::string target;

    target = transpiler_stub_upper_hyphenated(statement.target);
    buffer.append("           MOVE \"");
    buffer.append(statement.value);
    buffer.append("\" TO ");
    buffer.append(target);
    buffer.append(".\n");
}

static void transpiler_stub_emit_display(const t_stub_statement &statement, std::string &buffer)
{
    std::string target;

    target = transpiler_stub_upper_hyphenated(statement.target);
    buffer.append("           DISPLAY ");
    buffer.append(target);
    buffer.append(".\n");
}

static void transpiler_stub_emit_function_paragraph(const t_stub_function &function, std::string &buffer)
{
    std::string name;
    size_t index;
    size_t statement_count;

    name = transpiler_stub_upper_hyphenated(function.name);
    buffer.append("       ");
    buffer.append(name);
    buffer.append(".\n");
    statement_count = function.statements.size();
    index = 0;
    while (index < statement_count)
    {
        const t_stub_statement &statement = function.statements[index];

        if (statement.kind == STUB_STATEMENT_PERFORM)
            transpiler_stub_emit_perform(statement, buffer);
        else if (statement.kind == STUB_STATEMENT_MOVE)
            transpiler_stub_emit_move(statement, buffer);
        else if (statement.kind == STUB_STATEMENT_DISPLAY)
            transpiler_stub_emit_display(statement, buffer);
        index += 1;
    }
    if (function.name != "main")
    {
        buffer.append("           EXIT.\n");
    }
}

static void transpiler_stub_emit_procedure_division(std::string &buffer, const std::vector<t_stub_function> &functions)
{
    size_t index;
    size_t function_count;

    buffer.append("       PROCEDURE DIVISION.\n");
    function_count = functions.size();
    index = 0;
    while (index < function_count)
    {
        const t_stub_function &function = functions[index];

        if (function.name == "main")
        {
            transpiler_stub_emit_function_paragraph(function, buffer);
            buffer.append("           GOBACK.\n");
            buffer.append("\n");
            break ;
        }
        index += 1;
    }
    index = 0;
    while (index < function_count)
    {
        const t_stub_function &function = functions[index];

        if (function.name != "main")
        {
            transpiler_stub_emit_function_paragraph(function, buffer);
            buffer.append("\n");
        }
        index += 1;
    }
}

static int transpiler_stub_write_output(const char *output_path, const std::string &program_id,
    const std::vector<t_stub_variable> &variables, const std::vector<t_stub_function> &functions)
{
    std::ofstream stream;
    std::string buffer;

    stream.open(output_path, std::ios::out | std::ios::trunc);
    if (!stream.is_open())
        return (FT_FAILURE);
    transpiler_stub_emit_header(buffer, program_id);
    transpiler_stub_emit_data_division(buffer, variables);
    transpiler_stub_emit_procedure_division(buffer, functions);
    buffer.append("       END PROGRAM ");
    buffer.append(program_id);
    buffer.append(".\n");
    stream << buffer;
    if (!stream.good())
    {
        stream.close();
        return (FT_FAILURE);
    }
    stream.close();
    return (FT_SUCCESS);
}

static std::string transpiler_stub_program_id_from_path(const char *input_path)
{
    std::string path;
    size_t slash_pos;
    size_t dot_pos;
    std::string base;

    if (!input_path)
        return ("PROGRAM");
    path = input_path;
    slash_pos = path.find_last_of("/\\");
    if (slash_pos == std::string::npos)
        base = path;
    else
        base = path.substr(slash_pos + 1);
    dot_pos = base.find_last_of('.');
    if (dot_pos != std::string::npos)
        base = base.substr(0, dot_pos);
    return (transpiler_stub_upper_hyphenated(base));
}

int transpiler_stub_cblc_to_cobol(const char *input_path, const char *output_path)
{
    std::vector<t_stub_variable> variables;
    std::vector<t_stub_function> functions;
    std::string program_id;

    if (!input_path || !output_path)
        return (FT_FAILURE);
    if (transpiler_stub_collect_source(input_path, variables, functions) != FT_SUCCESS)
        return (FT_FAILURE);
    program_id = transpiler_stub_program_id_from_path(input_path);
    if (transpiler_stub_write_output(output_path, program_id, variables, functions) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}
