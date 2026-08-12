#include "standard_library_test_support.hpp"

#include <fstream>
#include <string>

static int vector_cobol_contains(const char *source_path, const char *needle)
{
    std::ifstream source(source_path);
    std::string line;

    if (!source || !needle)
        return (FT_FAILURE);
    while (std::getline(source, line))
    {
        if (line.find(needle) != std::string::npos)
            return (FT_SUCCESS);
    }
    return (FT_FAILURE);
}

static const char *g_vector_runtime_source =
    "vector<int> score_history;\n"
    "vector<int> copied(score_history);\n"
    "vector<int> assigned;\n"
    "int value;\n"
    "\n"
    "void exercise_scope()\n"
    "{\n"
    "    vector<int> scoped;\n"
    "    scoped.push_back(41);\n"
    "    scoped.push_back(42);\n"
    "    value = scoped.size();\n"
    "    display(value);\n"
    "    value = scoped.front();\n"
    "    display(value);\n"
    "    value = scoped.back();\n"
    "    display(value);\n"
    "    return;\n"
    "}\n"
    "\n"
    "void main()\n"
    "{\n"
    "    display(\"DEFAULT\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    value = score_history.empty();\n"
    "    display(value);\n"
    "    value = score_history.max_size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.reserve(4);\n"
    "    display(\"RESERVE\");\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.reserve(2);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.push_back(10);\n"
    "    score_history.push_back(20);\n"
    "    score_history.emplace_back(30);\n"
    "    display(\"PUSH\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at(0);\n"
    "    display(value);\n"
    "    value = score_history.at(2);\n"
    "    display(value);\n"
    "    value = score_history.front();\n"
    "    display(value);\n"
    "    value = score_history.back();\n"
    "    display(value);\n"
    "\n"
    "    score_history.insert(1, 15);\n"
    "    score_history.insert(-1, 5);\n"
    "    score_history.insert(999, 35);\n"
    "    display(\"INSERT\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at(0);\n"
    "    display(value);\n"
    "    value = score_history.at(1);\n"
    "    display(value);\n"
    "    value = score_history.at(2);\n"
    "    display(value);\n"
    "    value = score_history.at(3);\n"
    "    display(value);\n"
    "    value = score_history.at(4);\n"
    "    display(value);\n"
    "    value = score_history.at(5);\n"
    "    display(value);\n"
    "\n"
    "    score_history.erase(2);\n"
    "    score_history.erase(-1);\n"
    "    score_history.erase(999);\n"
    "    score_history.pop_back();\n"
    "    display(\"ERASE_POP\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at(2);\n"
    "    display(value);\n"
    "    value = score_history.back();\n"
    "    display(value);\n"
    "\n"
    "    score_history.resize(8);\n"
    "    display(\"RESIZE_UP\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at(5);\n"
    "    display(value);\n"
    "    value = score_history.at(7);\n"
    "    display(value);\n"
    "    score_history.resize(3);\n"
    "    display(\"RESIZE_DOWN\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.back();\n"
    "    display(value);\n"
    "    score_history.resize(-1);\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.assign(4, 7);\n"
    "    display(\"ASSIGN\");\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.at(0);\n"
    "    display(value);\n"
    "    value = score_history.at(3);\n"
    "    display(value);\n"
    "    score_history.assign(-2, 9);\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "\n"
    "    score_history.reserve(8);\n"
    "    score_history.push_back(1);\n"
    "    score_history.push_back(2);\n"
    "    score_history.shrink_to_fit();\n"
    "    display(\"SHRINK\");\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.clear();\n"
    "    value = score_history.size();\n"
    "    display(value);\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.shrink_to_fit();\n"
    "    value = score_history.capacity();\n"
    "    display(value);\n"
    "    score_history.pop_back();\n"
    "    score_history.erase(0);\n"
    "\n"
    "    score_history.push_back(11);\n"
    "    score_history.push_back(22);\n"
    "    score_history.push_back(33);\n"
    "    assigned = score_history;\n"
    "    assigned = assigned;\n"
    "    score_history.erase(0);\n"
    "    display(\"COPY_ASSIGN\");\n"
    "    value = assigned.size();\n"
    "    display(value);\n"
    "    value = assigned.at(0);\n"
    "    display(value);\n"
    "    value = score_history.at(0);\n"
    "    display(value);\n"
    "\n"
    "    exercise_scope();\n"
    "    display(\"VECTOR COMPLETE\");\n"
    "    return;\n"
    "}\n";

static const char *g_vector_string_source =
    "vector<string> names;\n"
    "int value;\n"
    "int slot;\n"
    "\n"
    "void main()\n"
    "{\n"
    "    slot = 0;\n"
    "    value = names[slot]->len();\n"
    "    names[slot]->clear();\n"
    "    value = names->len();\n"
    "    return;\n"
    "}\n";

FT_TEST(test_standard_library_vector_translates_full_lifecycle)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[768];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_runtime.cblc", source_path, sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_runtime.cob", cobol_path, sizeof(cobol_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_runtime_source) != FT_SUCCESS)
        goto cleanup;
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input %s --output %s",
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector source should transpile\\n");
        goto cleanup;
    }
    {
        static const char *const required_methods[] = {
            "SIZE", "CAPACITY", "MAX-SIZE", "EMPTY", "RESERVE", "SHRINK-TO-FIT",
            "PUSH-BACK", "POP-BACK", "AT", "FRONT", "BACK", "EMPLACE-BACK", "INSERT",
            "ERASE", "CLEAR", "ASSIGN", "RESIZE"
        };
        size_t index;
        char marker[192];

        for (index = 0; index < sizeof(required_methods) / sizeof(required_methods[0]); ++index)
        {
            command_length = std::snprintf(marker, sizeof(marker),
                "CBLC-METHOD-SCORE-HISTORY-CBLC-TPL-VECTOR-INT-%s", required_methods[index]);
            if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(marker)
                || vector_cobol_contains(cobol_path, marker) != FT_SUCCESS)
            {
                std::printf("Assertion failed: vector translation should emit %s\n", required_methods[index]);
                goto cleanup;
            }
        }
    }
    if (vector_cobol_contains(cobol_path, "CBLC-CONSTRUCTOR-COPIED") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "ALLOCATE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "FREE") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-RAISING") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "CBLC-EX-PAYLOAD-OUT-OF-RANGE") != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector translation should emit ownership and bounds paths\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_directory(directory);
    return (status);
}

FT_TEST(test_standard_library_vector_string_indexed_string_methods)
{
    char directory[256];
    char source_path[256];
    char cobol_path[256];
    char command[1024];
    int command_length;
    int status;

    directory[0] = '\0';
    source_path[0] = '\0';
    cobol_path[0] = '\0';
    status = FT_FAILURE;
    if (test_create_temp_directory(directory, sizeof(directory)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_join_path(directory, "vector_string.cblc", source_path, sizeof(source_path)) != FT_SUCCESS
        || test_join_path(directory, "vector_string.cob", cobol_path, sizeof(cobol_path)) != FT_SUCCESS)
    {
        test_remove_directory(directory);
        return (FT_FAILURE);
    }
    if (test_write_text_file(source_path, g_vector_string_source) != FT_SUCCESS)
        goto cleanup;
    command_length = std::snprintf(command, sizeof(command),
        "./ctoc_cobol_transpiler --diagnostics silent --direction cblc-to-cobol --input %s --output %s",
        source_path, cobol_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command)
        || test_run_command(command) != FT_SUCCESS)
    {
        std::printf("Assertion failed: vector<string> indexed string methods should transpile\n");
        goto cleanup;
    }
    if (vector_cobol_contains(cobol_path, "01 CBLC-SPTR-NAMES-CBLC-USER-DATA BASED.") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "05 CBLC-SPTR-NAMES-CBLC-USER-DATA-LEN") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "05 CBLC-SPTR-NAMES-CBLC-USER-DATA-BUF") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "COMPUTE CBLC-USER-VALUE = CBLC-SPTR-NAMES-CBLC-USER-DATA-LEN") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "MOVE SPACES TO CBLC-SPTR-NAMES-CBLC-USER-DATA-BUF") != FT_SUCCESS
        || vector_cobol_contains(cobol_path, "COMPUTE CBLC-PTR-OFFSET = SLOT * 5") != FT_SUCCESS)
    {
        std::printf("Assertion failed: indexed string methods should use the element record view\n");
        goto cleanup;
    }
    status = FT_SUCCESS;
cleanup:
    test_remove_directory(directory);
    return (status);
}
