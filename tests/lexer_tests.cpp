#include "lexer.hpp"

#include "test_suites.hpp"

FT_TEST(test_lexer_keyword_lookup_identifies_keywords)
{
    t_lexer_token_kind kind;

    kind = lexer_token_lookup_keyword("division", ft_strlen("division"));
    if (kind != LEXER_TOKEN_KEYWORD_DIVISION)
    {
        pf_printf("Assertion failed: lexer should classify DIVISION keyword\n");
        return (FT_FAILURE);
    }
    kind = lexer_token_lookup_keyword("Program-Id", ft_strlen("Program-Id"));
    if (kind != LEXER_TOKEN_KEYWORD_PROGRAM_ID)
    {
        pf_printf("Assertion failed: lexer should classify PROGRAM-ID keyword\n");
        return (FT_FAILURE);
    }
    kind = lexer_token_lookup_keyword("working-storage", ft_strlen("working-storage"));
    if (kind != LEXER_TOKEN_KEYWORD_WORKING_STORAGE)
    {
        pf_printf("Assertion failed: lexer should classify WORKING-STORAGE keyword\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_defaults_to_identifier)
{
    t_lexer_token_kind kind;

    kind = lexer_token_lookup_keyword("custom-name", ft_strlen("custom-name"));
    if (kind != LEXER_TOKEN_IDENTIFIER)
    {
        pf_printf("Assertion failed: lexer should treat unknown words as identifiers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_detects_whitespace)
{
    const char *text;
    t_lexer_trivia_kind trivia;

    text = " \t\n\r";
    trivia = lexer_classify_trivia(text, ft_strlen(text));
    if (trivia != LEXER_TRIVIA_WHITESPACE)
    {
        pf_printf("Assertion failed: lexer should treat whitespace as trivia\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_detects_comments)
{
    const char *comment;
    const char *not_comment;
    t_lexer_trivia_kind trivia;

    comment = "*> comment line";
    trivia = lexer_classify_trivia(comment, ft_strlen(comment));
    if (trivia != LEXER_TRIVIA_COMMENT)
    {
        pf_printf("Assertion failed: lexer should classify *> lines as comments\n");
        return (FT_FAILURE);
    }
    not_comment = "* missing arrow";
    trivia = lexer_classify_trivia(not_comment, ft_strlen(not_comment));
    if (trivia != LEXER_TRIVIA_NONE)
    {
        pf_printf("Assertion failed: lexer should ignore asterisk without > as comment start\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_sample_program)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "IDENTIFICATION DIVISION.\n"
        "PROGRAM-ID. SAMPLE.\n"
        "WORKING-STORAGE SECTION.\n"
        "77 COUNTER PIC 9(2).\n"
        "PROCEDURE DIVISION.\n"
        "    MOVE 10 TO COUNTER.\n"
        "    DISPLAY \"VALUE\".\n"
        "    STOP RUN.\n";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_IDENTIFICATION, "IDENTIFICATION", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_DIVISION, "DIVISION", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 1, 24) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_PROGRAM_ID, "PROGRAM-ID", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 2, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_IDENTIFIER, "SAMPLE", 2, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 2, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_WORKING_STORAGE, "WORKING-STORAGE", 3, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_SECTION, "SECTION", 3, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 3, 24) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unterminated_string)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "move \"unterminated\n";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_MOVE, "move", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
    {
        pf_printf("Assertion failed: lexer should fail for unterminated string literal\n");
        return (FT_FAILURE);
    }
    if (token.kind != LEXER_TOKEN_UNKNOWN)
    {
        pf_printf("Assertion failed: unterminated string should produce unknown token\n");
        return (FT_FAILURE);
    }
    if (token.line != 1 || token.column != 6)
    {
        pf_printf("Assertion failed: unterminated string should report start location\n");
        return (FT_FAILURE);
    }
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

const t_test_case *get_lexer_tests(size_t *count)
{
    static const t_test_case tests[] = {
        {"lexer_keyword_lookup_identifies_keywords", test_lexer_keyword_lookup_identifies_keywords},
        {"lexer_keyword_lookup_defaults_to_identifier", test_lexer_keyword_lookup_defaults_to_identifier},
        {"lexer_trivia_detects_whitespace", test_lexer_trivia_detects_whitespace},
        {"lexer_trivia_detects_comments", test_lexer_trivia_detects_comments},
        {"lexer_tokenizes_sample_program", test_lexer_tokenizes_sample_program},
        {"lexer_reports_unterminated_string", test_lexer_reports_unterminated_string}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
