#include "test_suites.hpp"

#include "compatibility/libft_compat.hpp"
#include "compatibility/printf_compat.hpp"

static int grammar_document_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cblc_grammar.md", buffer, buffer_size) != FT_SUCCESS)
    {
        std::printf("Assertion failed: expected docs/cblc_grammar.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_grammar_document_exists)
{
    char buffer[65536];

    if (grammar_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (std::strlen(buffer) == 0)
    {
        std::printf("Assertion failed: docs/cblc_grammar.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "program             ::=", std::strlen(buffer)))
    {
        std::printf("Assertion failed: grammar should define the program production\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int grammar_document_expect_rule(const char *buffer, const char *rule)
{
    size_t length;

    if (!buffer)
        return (FT_FAILURE);
    if (!rule)
        return (FT_FAILURE);
    length = std::strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, rule, length))
    {
        std::printf("Assertion failed: grammar should include production '%s'\n", rule);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_grammar_document_lists_core_rules)
{
    static const char *rules[] = {
        "file_declaration    ::=",
        "record_declaration  ::=",
        "statement           ::=",
        "while_statement     ::=",
        "if_statement        ::=",
        "open_statement      ::=",
        "read_statement      ::=",
        "write_statement     ::=",
        "display_statement   ::=",
        "return_statement    ::=",
        "expression                  ::=",
        "primary_expression          ::="
    };
    char buffer[16384];
    size_t index;
    size_t count;

    if (grammar_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    index = 0;
    count = sizeof(rules) / sizeof(rules[0]);
    while (index < count)
    {
        if (grammar_document_expect_rule(buffer, rules[index]) != FT_SUCCESS)
            return (FT_FAILURE);
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_design_doc_mentions_grammar)
{
    char line[1024];
    FILE *file;

    file = std::fopen("design_doc.txt", "r");
    if (!file)
    {
        std::printf("Assertion failed: design_doc.txt should be readable\n");
        return (FT_FAILURE);
    }
    while (std::fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "docs/cblc_grammar.md", std::strlen(line)))
        {
            std::fclose(file);
            return (FT_SUCCESS);
        }
    }
    std::fclose(file);
    std::printf("Assertion failed: design_doc.txt should reference docs/cblc_grammar.md\n");
    return (FT_FAILURE);
}

const t_test_case *get_grammar_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"grammar_document_exists", test_grammar_document_exists},
        {"grammar_document_lists_core_rules", test_grammar_document_lists_core_rules},
        {"design_doc_mentions_grammar", test_design_doc_mentions_grammar}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
