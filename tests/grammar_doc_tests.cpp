#include "test_suites.hpp"

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"

static int grammar_document_load(char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    if (test_read_text_file("docs/cblc_grammar.md", buffer, buffer_size) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: expected docs/cblc_grammar.md to be readable\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_grammar_document_exists(void)
{
    char buffer[65536];

    if (grammar_document_load(buffer, sizeof(buffer)) != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strlen(buffer) == 0)
    {
        pf_printf("Assertion failed: docs/cblc_grammar.md should not be empty\n");
        return (FT_FAILURE);
    }
    if (!ft_strnstr(buffer, "program             ::=", ft_strlen(buffer)))
    {
        pf_printf("Assertion failed: grammar should define the program production\n");
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
    length = ft_strlen(buffer);
    if (length == 0)
        return (FT_FAILURE);
    if (!ft_strnstr(buffer, rule, length))
    {
        pf_printf("Assertion failed: grammar should include production '%s'\n", rule);
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

static int test_grammar_document_lists_core_rules(void)
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

static int test_design_doc_mentions_grammar(void)
{
    char line[1024];
    FILE *file;

    file = ft_fopen("design_doc.txt", "r");
    if (!file)
    {
        pf_printf("Assertion failed: design_doc.txt should be readable\n");
        return (FT_FAILURE);
    }
    while (ft_fgets(line, sizeof(line), file))
    {
        if (ft_strnstr(line, "docs/cblc_grammar.md", ft_strlen(line)))
        {
            ft_fclose(file);
            return (FT_SUCCESS);
        }
    }
    ft_fclose(file);
    pf_printf("Assertion failed: design_doc.txt should reference docs/cblc_grammar.md\n");
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
