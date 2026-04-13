#include "cblc_transpiler.hpp"

#include "test_suites.hpp"

static int lexer_expect_next(t_lexer *lexer, t_lexer_token_kind kind,
    const char *lexeme, size_t line, size_t column)
{
    t_lexer_token token;

    if (lexer_next_token(lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    return (test_expect_token(&token, kind, lexeme, line, column));
}

FT_TEST(test_lexer_keyword_lookup_identifies_keywords)
{
    t_lexer_token_kind kind;

    kind = lexer_token_lookup_keyword("division", std::strlen("division"));
    if (kind != LEXER_TOKEN_KEYWORD_DIVISION)
    {
        std::printf("Assertion failed: lexer should classify DIVISION keyword\n");
        return (FT_FAILURE);
    }
    kind = lexer_token_lookup_keyword("Program-Id", std::strlen("Program-Id"));
    if (kind != LEXER_TOKEN_KEYWORD_PROGRAM_ID)
    {
        std::printf("Assertion failed: lexer should classify PROGRAM-ID keyword\n");
        return (FT_FAILURE);
    }
    kind = lexer_token_lookup_keyword("working-storage", std::strlen("working-storage"));
    if (kind != LEXER_TOKEN_KEYWORD_WORKING_STORAGE)
    {
        std::printf("Assertion failed: lexer should classify WORKING-STORAGE keyword\n");
        return (FT_FAILURE);
    }
    kind = lexer_token_lookup_keyword("mod", std::strlen("mod"));
    if (kind != LEXER_TOKEN_KEYWORD_MOD)
    {
        std::printf("Assertion failed: lexer should classify MOD keyword\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_defaults_to_identifier)
{
    t_lexer_token_kind kind;

    kind = lexer_token_lookup_keyword("custom-name", std::strlen("custom-name"));
    if (kind != LEXER_TOKEN_IDENTIFIER)
    {
        std::printf("Assertion failed: lexer should treat unknown words as identifiers\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_handles_null_and_empty)
{
    if (lexer_token_lookup_keyword(NULL, 4) != LEXER_TOKEN_IDENTIFIER)
    {
        std::printf("Assertion failed: null keyword lookup should return identifier\n");
        return (FT_FAILURE);
    }
    if (lexer_token_lookup_keyword("", 0) != LEXER_TOKEN_IDENTIFIER)
    {
        std::printf("Assertion failed: empty keyword lookup should return identifier\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_end_keywords)
{
    if (lexer_token_lookup_keyword("end-if", std::strlen("end-if")) != LEXER_TOKEN_KEYWORD_END_IF)
    {
        std::printf("Assertion failed: lexer should classify END-IF keyword\n");
        return (FT_FAILURE);
    }
    if (lexer_token_lookup_keyword("End-Perform", std::strlen("End-Perform"))
        != LEXER_TOKEN_KEYWORD_END_PERFORM)
    {
        std::printf("Assertion failed: lexer should classify END-PERFORM keyword\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_io_keywords)
{
    if (lexer_token_lookup_keyword("select", std::strlen("select")) != LEXER_TOKEN_KEYWORD_SELECT)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("assign", std::strlen("assign")) != LEXER_TOKEN_KEYWORD_ASSIGN)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("fd", std::strlen("fd")) != LEXER_TOKEN_KEYWORD_FD)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("open", std::strlen("open")) != LEXER_TOKEN_KEYWORD_OPEN)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("close", std::strlen("close")) != LEXER_TOKEN_KEYWORD_CLOSE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("read", std::strlen("read")) != LEXER_TOKEN_KEYWORD_READ)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("write", std::strlen("write")) != LEXER_TOKEN_KEYWORD_WRITE)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_copy_replacing_keywords)
{
    if (lexer_token_lookup_keyword("copy", std::strlen("copy")) != LEXER_TOKEN_KEYWORD_COPY)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("replacing", std::strlen("replacing")) != LEXER_TOKEN_KEYWORD_REPLACING)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("leading", std::strlen("leading")) != LEXER_TOKEN_KEYWORD_LEADING)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("trailing", std::strlen("trailing")) != LEXER_TOKEN_KEYWORD_TRAILING)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("word", std::strlen("word")) != LEXER_TOKEN_KEYWORD_WORD)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_call_argument_keywords)
{
    if (lexer_token_lookup_keyword("call", std::strlen("call")) != LEXER_TOKEN_KEYWORD_CALL)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("using", std::strlen("using")) != LEXER_TOKEN_KEYWORD_USING)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("reference", std::strlen("reference")) != LEXER_TOKEN_KEYWORD_REFERENCE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("content", std::strlen("content")) != LEXER_TOKEN_KEYWORD_CONTENT)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("length", std::strlen("length")) != LEXER_TOKEN_KEYWORD_LENGTH)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("of", std::strlen("of")) != LEXER_TOKEN_KEYWORD_OF)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_data_clause_keywords)
{
    if (lexer_token_lookup_keyword("pic", std::strlen("pic")) != LEXER_TOKEN_KEYWORD_PIC)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("value", std::strlen("value")) != LEXER_TOKEN_KEYWORD_VALUE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("occurs", std::strlen("occurs")) != LEXER_TOKEN_KEYWORD_OCCURS)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("times", std::strlen("times")) != LEXER_TOKEN_KEYWORD_TIMES)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("depending", std::strlen("depending")) != LEXER_TOKEN_KEYWORD_DEPENDING)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_control_flow_keywords)
{
    if (lexer_token_lookup_keyword("if", std::strlen("if")) != LEXER_TOKEN_KEYWORD_IF)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("else", std::strlen("else")) != LEXER_TOKEN_KEYWORD_ELSE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("perform", std::strlen("perform")) != LEXER_TOKEN_KEYWORD_PERFORM)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("until", std::strlen("until")) != LEXER_TOKEN_KEYWORD_UNTIL)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("not", std::strlen("not")) != LEXER_TOKEN_KEYWORD_NOT)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("true", std::strlen("true")) != LEXER_TOKEN_KEYWORD_TRUE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("false", std::strlen("false")) != LEXER_TOKEN_KEYWORD_FALSE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("stop", std::strlen("stop")) != LEXER_TOKEN_KEYWORD_STOP)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("run", std::strlen("run")) != LEXER_TOKEN_KEYWORD_RUN)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_declarative_keywords)
{
    if (lexer_token_lookup_keyword("use", std::strlen("use")) != LEXER_TOKEN_KEYWORD_USE)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("after", std::strlen("after")) != LEXER_TOKEN_KEYWORD_AFTER)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("error", std::strlen("error")) != LEXER_TOKEN_KEYWORD_ERROR)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("on", std::strlen("on")) != LEXER_TOKEN_KEYWORD_ON)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("declaratives", std::strlen("declaratives"))
        != LEXER_TOKEN_KEYWORD_DECLARATIVES)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_identifies_varying_keywords)
{
    if (lexer_token_lookup_keyword("varying", std::strlen("varying")) != LEXER_TOKEN_KEYWORD_VARYING)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("from", std::strlen("from")) != LEXER_TOKEN_KEYWORD_FROM)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("by", std::strlen("by")) != LEXER_TOKEN_KEYWORD_BY)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("to", std::strlen("to")) != LEXER_TOKEN_KEYWORD_TO)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keyword_lookup_rejects_prefix_matches)
{
    if (lexer_token_lookup_keyword("division", std::strlen("div")) != LEXER_TOKEN_IDENTIFIER)
        return (FT_FAILURE);
    if (lexer_token_lookup_keyword("program-id-extra", std::strlen("program-id-extra"))
        != LEXER_TOKEN_IDENTIFIER)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_detects_whitespace)
{
    const char *text;
    t_lexer_trivia_kind trivia;

    text = " \t\n\r";
    trivia = lexer_classify_trivia(text, std::strlen(text));
    if (trivia != LEXER_TRIVIA_WHITESPACE)
    {
        std::printf("Assertion failed: lexer should treat whitespace as trivia\n");
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
    trivia = lexer_classify_trivia(comment, std::strlen(comment));
    if (trivia != LEXER_TRIVIA_COMMENT)
    {
        std::printf("Assertion failed: lexer should classify *> lines as comments\n");
        return (FT_FAILURE);
    }
    not_comment = "* missing arrow";
    trivia = lexer_classify_trivia(not_comment, std::strlen(not_comment));
    if (trivia != LEXER_TRIVIA_NONE)
    {
        std::printf("Assertion failed: lexer should ignore asterisk without > as comment start\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_handles_null_and_empty)
{
    if (lexer_classify_trivia(NULL, 4) != LEXER_TRIVIA_NONE)
    {
        std::printf("Assertion failed: null trivia lookup should return none\n");
        return (FT_FAILURE);
    }
    if (lexer_classify_trivia("", 0) != LEXER_TRIVIA_NONE)
    {
        std::printf("Assertion failed: empty trivia lookup should return none\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_rejects_mixed_text)
{
    if (lexer_classify_trivia(" \tDATA", std::strlen(" \tDATA")) != LEXER_TRIVIA_NONE)
    {
        std::printf("Assertion failed: mixed trivia and text should return none\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_trivia_classifies_multiline_comment_text)
{
    const char *comment;

    comment = "*> comment\nMOVE";
    if (lexer_classify_trivia(comment, std::strlen(comment)) != LEXER_TRIVIA_COMMENT)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_distinguishes_assignment_and_equality)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "= ==";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_ASSIGN, "=", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_EQUALS, "==", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_minus_operator)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "- 5";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_MINUS, "-", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_star_operator)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "* 5";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_STAR, "*", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_slash_operator)
{
    const char *source;
    t_lexer lexer;
    t_lexer_token token;

    source = "/ 5";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_SLASH, "/", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
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

FT_TEST(test_lexer_tokenizes_punctuation_sequence)
{
    t_lexer lexer;

    lexer_init(&lexer, ".,:;()");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COLON, ":", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SEMICOLON, ";", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_relational_operators)
{
    t_lexer lexer;

    lexer_init(&lexer, "< <= <> > >=");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_OR_EQUAL, "<=", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NOT_EQUALS, "<>", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_OR_EQUAL, ">=", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_decimal_numeric_literal)
{
    t_lexer lexer;

    lexer_init(&lexer, "123.45");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "123.45", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keeps_trailing_period_separate_from_integer)
{
    t_lexer lexer;

    lexer_init(&lexer, "123.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "123", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_single_quoted_string_literal)
{
    t_lexer lexer;

    lexer_init(&lexer, "'VALUE'");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "'VALUE'", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_comment_lines)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> generated comment\nMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_with_hyphen_underscore_and_digits)
{
    t_lexer lexer;

    lexer_init(&lexer, "FIELD-1_2");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD-1_2", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_character)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "@");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
    {
        std::printf("Assertion failed: lexer should fail for unknown characters\n");
        return (FT_FAILURE);
    }
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "@", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_next_token_rejects_null_lexer)
{
    t_lexer_token token;

    if (lexer_next_token(NULL, &token) != FT_FAILURE)
    {
        std::printf("Assertion failed: lexer_next_token should reject null lexer\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_next_token_rejects_null_token)
{
    t_lexer lexer;

    lexer_init(&lexer, "MOVE");
    if (lexer_next_token(&lexer, NULL) != FT_FAILURE)
    {
        std::printf("Assertion failed: lexer_next_token should reject null token\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_null_source_produces_eof)
{
    t_lexer lexer;

    lexer_init(&lexer, NULL);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_extended_whitespace)
{
    t_lexer lexer;

    lexer_init(&lexer, "\f\v\tMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_comment_without_newline)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> comment");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_double_quoted_string_literal)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"A B\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"A B\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_integer_period_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "12.a");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "12", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "a", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_leading_hyphen_from_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "-FIELD");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_MINUS, "-", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_plus_operator)
{
    t_lexer lexer;

    lexer_init(&lexer, "+ 5");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PLUS, "+", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_assignment_followed_by_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "=VALUE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VALUE, "VALUE", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_equality_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "A==B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_EQUALS, "==", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_columns_after_blank_lines)
{
    t_lexer lexer;

    lexer_init(&lexer, "\n\nMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 3, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 3, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_indented_comment_line)
{
    t_lexer lexer;

    lexer_init(&lexer, "   *> note\n  DISPLAY");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "DISPLAY", 2, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unterminated_single_quoted_string)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "'unterminated");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "'unterminated", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_character_after_token)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "MOVE #");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "#", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_less_than_followed_by_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "<VALUE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VALUE, "VALUE", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_greater_than_followed_by_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, ">VALUE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VALUE, "VALUE", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_less_or_equal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "A<=B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_OR_EQUAL, "<=", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_greater_or_equal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "A>=B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_OR_EQUAL, ">=", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_not_equal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "A<>B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NOT_EQUALS, "<>", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_decimal_then_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "1.2A");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1.2", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_lowercase_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "display");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "display", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_multiline_string_literal)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"A\nB\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"A\nB\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_consecutive_equals)
{
    t_lexer lexer;

    lexer_init(&lexer, "===");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_EQUALS, "==", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_decimal_with_second_period)
{
    t_lexer lexer;

    lexer_init(&lexer, "1.2.3");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1.2", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "3", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_double_period_between_integers)
{
    t_lexer lexer;

    lexer_init(&lexer, "1..2");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "2", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_slash_star_as_operators)
{
    t_lexer lexer;

    lexer_init(&lexer, "*/");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SLASH, "/", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_allows_identifier_trailing_hyphen)
{
    t_lexer lexer;

    lexer_init(&lexer, "FIELD-");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD-", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_leading_underscore_as_unknown)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "_FIELD");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "_", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_columns_after_tabs)
{
    t_lexer lexer;

    lexer_init(&lexer, "\t\tDISPLAY");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "DISPLAY", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_position_after_comment_and_blank_line)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> comment\n\n  MOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 3, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 3, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_empty_double_quoted_string)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_empty_single_quoted_string)
{
    t_lexer lexer;

    lexer_init(&lexer, "''");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "''", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_crlf_as_single_line_break)
{
    t_lexer lexer;

    lexer_init(&lexer, "\r\nMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_crlf_comment_line)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> comment\r\nMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_operators_across_lines)
{
    t_lexer lexer;

    lexer_init(&lexer, "+\n-");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PLUS, "+", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_MINUS, "-", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_greater_then_less_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "><");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_repeated_less_than_as_separate_tokens)
{
    t_lexer lexer;

    lexer_init(&lexer, "<<");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_integer_across_newline)
{
    t_lexer lexer;

    lexer_init(&lexer, "1\n2");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "2", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keeps_keyword_with_underscore_as_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "END_IF");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "END_IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keeps_keyword_suffix_as_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "end-ifx");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "end-ifx", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_eof_after_whitespace_only_source)
{
    t_lexer lexer;

    lexer_init(&lexer, "  \n\t");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_eof_after_comment_and_trailing_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> comment\n   ");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_with_repeated_hyphens)
{
    t_lexer lexer;

    lexer_init(&lexer, "A--B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A--B", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_lowercase_identifier_with_digits_and_underscore)
{
    t_lexer lexer;

    lexer_init(&lexer, "abc123_def");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "abc123_def", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_numeric_minus_numeric_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "12-34");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "12", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_MINUS, "-", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "34", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_string_followed_by_identifier_without_space)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"A\"B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"A\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_leading_period_from_digit)
{
    t_lexer lexer;

    lexer_init(&lexer, ".5");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_comma_separated_identifiers)
{
    t_lexer lexer;

    lexer_init(&lexer, "A,B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_after_newline)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "MOVE\n?");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "?", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_comment_immediately_after_token)
{
    t_lexer lexer;

    lexer_init(&lexer, "A*> comment\nB");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_pic_clause_symbols)
{
    t_lexer lexer;

    lexer_init(&lexer, "9(2)");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "9", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "2", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_keyword_followed_by_period_without_space)
{
    t_lexer lexer;

    lexer_init(&lexer, "END-IF.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_END_IF, "END-IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_colon_and_semicolon_between_identifiers)
{
    t_lexer lexer;

    lexer_init(&lexer, "A:B;C");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COLON, ":", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SEMICOLON, ";", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "C", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_string_containing_single_quote)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"A'B\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"A'B\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_single_quoted_string_containing_double_quote)
{
    t_lexer lexer;

    lexer_init(&lexer, "'A\"B'");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "'A\"B'", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_multiple_comment_lines)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> first\n*> second\nMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 3, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 3, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_after_tab)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "\t%");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "%", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_repeated_star_as_operators)
{
    t_lexer lexer;

    lexer_init(&lexer, "**");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_slash_between_identifiers)
{
    t_lexer lexer;

    lexer_init(&lexer, "A/B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SLASH, "/", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_decimal_minus_decimal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "1.5-0.5");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1.5", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_MINUS, "-", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "0.5", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_mixed_case_end_perform_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "end-Perform");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_END_PERFORM, "end-Perform", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_digit_then_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "1A2");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A2", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_splits_numeric_plus_numeric_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "7+8");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "7", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PLUS, "+", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "8", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_in_parentheses)
{
    t_lexer lexer;

    lexer_init(&lexer, "(FIELD)");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_less_equal_then_greater)
{
    t_lexer lexer;

    lexer_init(&lexer, "<=>");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_OR_EQUAL, "<=", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_not_equal_then_assign)
{
    t_lexer lexer;

    lexer_init(&lexer, "<>=");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NOT_EQUALS, "<>", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_star_space_greater_as_operators)
{
    t_lexer lexer;

    lexer_init(&lexer, "* >");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_form_feed_column)
{
    t_lexer lexer;

    lexer_init(&lexer, "\fMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_vertical_tab_column)
{
    t_lexer lexer;

    lexer_init(&lexer, "\vMOVE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_keyword_comma_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "MOVE,DISPLAY");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "DISPLAY", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_empty_source_produces_eof)
{
    t_lexer lexer;

    lexer_init(&lexer, "");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_two_equality_operators_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "====");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_EQUALS, "==", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_EQUALS, "==", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_assign_then_greater_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "=>");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_assign_then_less_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "=<");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_less_then_less_or_equal)
{
    t_lexer lexer;

    lexer_init(&lexer, "<<=");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_THAN, "<", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_OR_EQUAL, "<=", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_greater_then_greater_or_equal)
{
    t_lexer lexer;

    lexer_init(&lexer, ">>=");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_OR_EQUAL, ">=", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_keyword_followed_by_string_without_space)
{
    t_lexer lexer;

    lexer_init(&lexer, "DISPLAY\"X\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "DISPLAY", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"X\"", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_strings_across_lines)
{
    t_lexer lexer;

    lexer_init(&lexer, "'A'\n\"B\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "'A'", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"B\"", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_carriage_return_as_column_whitespace)
{
    t_lexer lexer;

    lexer_init(&lexer, "A\rB");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_skips_spaced_comment_after_token)
{
    t_lexer lexer;

    lexer_init(&lexer, "A *> comment\nB");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_mixed_case_working_storage_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "Working-Storage");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_WORKING_STORAGE,
            "Working-Storage", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_data_keyword_with_period)
{
    t_lexer lexer;

    lexer_init(&lexer, "DATA.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DATA, "DATA", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_argument_list)
{
    t_lexer lexer;

    lexer_init(&lexer, "CALL(A,B)");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CALL, "CALL", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_numeric_slash_numeric_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "8/2");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "8", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SLASH, "/", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "2", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_numeric_star_numeric_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "3*4");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "3", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "4", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_numeric_comma_numeric)
{
    t_lexer lexer;

    lexer_init(&lexer, "1,234");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "234", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_period_separated_identifiers)
{
    t_lexer lexer;

    lexer_init(&lexer, "A.B.C");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "C", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_tab_between_string_literals)
{
    t_lexer lexer;

    lexer_init(&lexer, "'A'\t'B'");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "'A'", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "'B'", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unterminated_empty_double_quote)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "\"");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_repeated_unknown_characters)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "??");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "?", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "?", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_environment_division_compact)
{
    t_lexer lexer;

    lexer_init(&lexer, "ENVIRONMENT DIVISION.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ENVIRONMENT, "ENVIRONMENT", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DIVISION, "DIVISION", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 21) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 22) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_file_section_compact)
{
    t_lexer lexer;

    lexer_init(&lexer, "FILE SECTION.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FILE, "FILE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_SECTION, "SECTION", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_call_using_reference_keywords)
{
    t_lexer lexer;

    lexer_init(&lexer, "CALL USING REFERENCE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CALL, "CALL", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_USING, "USING", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_REFERENCE, "REFERENCE", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 21) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_open_close_read_write_keywords)
{
    t_lexer lexer;

    lexer_init(&lexer, "OPEN CLOSE READ WRITE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_OPEN, "OPEN", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CLOSE, "CLOSE", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_READ, "READ", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_WRITE, "WRITE", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 22) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_boolean_condition_keywords)
{
    t_lexer lexer;

    lexer_init(&lexer, "TRUE FALSE NOT");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TRUE, "TRUE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FALSE, "FALSE", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_NOT, "NOT", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 15) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_after_multiline_comment_with_indent)
{
    t_lexer lexer;

    lexer_init(&lexer, "*> first\n   *> second\n    VALUE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VALUE, "VALUE", 3, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 3, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_repeated_right_parens)
{
    t_lexer lexer;

    lexer_init(&lexer, "))");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_repeated_left_parens)
{
    t_lexer lexer;

    lexer_init(&lexer, "((");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_after_decimal_without_space)
{
    t_lexer lexer;

    lexer_init(&lexer, "10.0FIELD");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "10.0", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_between_identifiers)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "A$B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "$", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_stop_run_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "STOP RUN.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_STOP, "STOP", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_RUN, "RUN", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_declarative_error_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "USE AFTER ERROR ON");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_USE, "USE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_AFTER, "AFTER", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ERROR, "ERROR", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ON, "ON", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_occurs_times_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "OCCURS 10 TIMES");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_OCCURS, "OCCURS", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "10", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TIMES, "TIMES", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_depending_on_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "DEPENDING ON COUNT");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DEPENDING, "DEPENDING", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ON, "ON", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "COUNT", 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_length_of_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "LENGTH OF FIELD");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_LENGTH, "LENGTH", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_OF, "OF", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_content_reference_keywords)
{
    t_lexer lexer;

    lexer_init(&lexer, "CONTENT REFERENCE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CONTENT, "CONTENT", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_REFERENCE, "REFERENCE", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_replacing_position_keywords)
{
    t_lexer lexer;

    lexer_init(&lexer, "LEADING TRAILING WORD");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_LEADING, "LEADING", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TRAILING, "TRAILING", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_WORD, "WORD", 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 22) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_compute_assignment_without_space)
{
    t_lexer lexer;

    lexer_init(&lexer, "COMPUTE RESULT=1");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_COMPUTE, "COMPUTE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "RESULT", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_ASSIGN, "=", 1, 15) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_abs_negative_argument)
{
    t_lexer lexer;

    lexer_init(&lexer, "ABS(-5)");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ABS, "ABS", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_MINUS, "-", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "5", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keeps_select_suffix_as_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "SELECT-FILE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "SELECT-FILE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_copy_replacing_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "COPY BASE REPLACING");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_COPY, "COPY", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "BASE", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_REPLACING, "REPLACING", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 20) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_perform_varying_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_PERFORM, "PERFORM", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VARYING, "VARYING", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "IDX", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FROM, "FROM", 1, 21) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 26) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_BY, "BY", 1, 28) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 31) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_UNTIL, "UNTIL", 1, 33) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "IDX", 1, 39) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 43) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "10", 1, 45) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 47) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_if_else_end_if_sequence)
{
    t_lexer lexer;

    lexer_init(&lexer, "IF TRUE ELSE END-IF");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_IF, "IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TRUE, "TRUE", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ELSE, "ELSE", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_END_IF, "END-IF", 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 20) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_select_assign_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "SELECT INFILE ASSIGN TO \"input.txt\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_SELECT, "SELECT", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ASSIGN, "ASSIGN", 1, 15) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TO, "TO", 1, 22) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"input.txt\"", 1, 25) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 36) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_fd_record_declaration)
{
    t_lexer lexer;

    lexer_init(&lexer, "FD INFILE.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FD, "FD", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_pic_value_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "PIC 9 VALUE 0");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_PIC, "PIC", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "9", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VALUE, "VALUE", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "0", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_mod_expression)
{
    t_lexer lexer;

    lexer_init(&lexer, "10 MOD 3");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "10", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOD, "MOD", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "3", 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_decimal_plus_decimal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "0.25+1.75");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "0.25", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PLUS, "+", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1.75", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unknown_after_comment)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "*> note\n!");
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "!", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_keeps_program_id_suffix_as_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "PROGRAM-ID-EXTRA");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "PROGRAM-ID-EXTRA", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_lowercase_stop_run_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "stop run.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_STOP, "stop", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_RUN, "run", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_display_string_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "DISPLAY \"OK\".");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DISPLAY, "DISPLAY", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"OK\"", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_move_numeric_to_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "MOVE 42 TO RESULT.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "42", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TO, "TO", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "RESULT", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_read_identifier_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "READ INFILE.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_READ, "READ", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_write_identifier_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "WRITE OUTREC.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_WRITE, "WRITE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "OUTREC", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_call_string_using_content)
{
    t_lexer lexer;

    lexer_init(&lexer, "CALL \"SUB\" USING CONTENT ARG");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CALL, "CALL", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"SUB\"", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_USING, "USING", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CONTENT, "CONTENT", 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "ARG", 1, 26) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 29) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_declaratives_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "DECLARATIVES.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DECLARATIVES, "DECLARATIVES", 1, 1)
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 14) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identifier_with_mixed_separators)
{
    t_lexer lexer;

    lexer_init(&lexer, "A_B-C_D");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A_B-C_D", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_decimal_slash_decimal_without_spaces)
{
    t_lexer lexer;

    lexer_init(&lexer, "8.0/2.0");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "8.0", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SLASH, "/", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "2.0", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_unterminated_single_quote_after_newline)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "MOVE\n'");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "'", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_procedure_division_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "PROCEDURE DIVISION.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_PROCEDURE, "PROCEDURE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DIVISION, "DIVISION", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 20) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_identification_program_id_headers)
{
    t_lexer lexer;

    lexer_init(&lexer, "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_IDENTIFICATION, "IDENTIFICATION", 1, 1)
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_DIVISION, "DIVISION", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 24) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_PROGRAM_ID, "PROGRAM-ID", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 2, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "HELLO", 2, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 2, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 2, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_77_level_pic_declaration)
{
    t_lexer lexer;

    lexer_init(&lexer, "77 COUNTER PIC 9.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "77", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "COUNTER", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_PIC, "PIC", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "9", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_01_group_declaration)
{
    t_lexer lexer;

    lexer_init(&lexer, "01 CUSTOMER-REC.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "01", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "CUSTOMER-REC", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_open_read_write_close_sequence)
{
    t_lexer lexer;

    lexer_init(&lexer, "OPEN INFILE\nREAD INFILE\nWRITE OUTREC\nCLOSE INFILE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_OPEN, "OPEN", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_READ, "READ", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 2, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_WRITE, "WRITE", 3, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "OUTREC", 3, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CLOSE, "CLOSE", 4, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 4, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 4, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_if_less_or_equal_condition)
{
    t_lexer lexer;

    lexer_init(&lexer, "IF A <= B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_IF, "IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LESS_OR_EQUAL, "<=", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_if_not_equal_condition)
{
    t_lexer lexer;

    lexer_init(&lexer, "IF A <> B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_IF, "IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NOT_EQUALS, "<>", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_end_perform_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "END-PERFORM.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_END_PERFORM, "END-PERFORM", 1, 1)
        != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_false_period)
{
    t_lexer lexer;

    lexer_init(&lexer, "FALSE.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FALSE, "FALSE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_string_with_period_inside)
{
    t_lexer lexer;

    lexer_init(&lexer, "\"A.B\"");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STRING_LITERAL, "\"A.B\"", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_end_if_sentence)
{
    t_lexer lexer;

    lexer_init(&lexer, "END-IF.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_END_IF, "END-IF", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 8) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_true_period)
{
    t_lexer lexer;

    lexer_init(&lexer, "TRUE.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_TRUE, "TRUE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_not_false_condition)
{
    t_lexer lexer;

    lexer_init(&lexer, "NOT FALSE");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_NOT, "NOT", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FALSE, "FALSE", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_use_after_error_on_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "USE AFTER ERROR ON INFILE.");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_USE, "USE", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_AFTER, "AFTER", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ERROR, "ERROR", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_ON, "ON", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "INFILE", 1, 20) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PERIOD, ".", 1, 26) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 27) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_using_reference_content_length_of)
{
    t_lexer lexer;

    lexer_init(&lexer, "USING REFERENCE A CONTENT B LENGTH OF C");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_USING, "USING", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_REFERENCE, "REFERENCE", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 17) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_CONTENT, "CONTENT", 1, 19) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 27) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_LENGTH, "LENGTH", 1, 29) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_OF, "OF", 1, 36) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "C", 1, 39) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 40) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_varying_from_by_until_clause)
{
    t_lexer lexer;

    lexer_init(&lexer, "VARYING I FROM 1 BY 1 UNTIL I > 9");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_VARYING, "VARYING", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "I", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_FROM, "FROM", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_BY, "BY", 1, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "1", 1, 21) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_UNTIL, "UNTIL", 1, 23) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "I", 1, 29) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_GREATER_THAN, ">", 1, 31) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "9", 1, 33) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 34) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_parenthesized_addition_times_identifier)
{
    t_lexer lexer;

    lexer_init(&lexer, "(A + B) * C");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_LEFT_PAREN, "(", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_PLUS, "+", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_RIGHT_PAREN, ")", 1, 7) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_STAR, "*", 1, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "C", 1, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tokenizes_adjacent_punctuation_between_identifiers)
{
    t_lexer lexer;

    lexer_init(&lexer, "A,:;B");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "A", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COMMA, ",", 1, 2) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_COLON, ":", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_SEMICOLON, ";", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "B", 1, 5) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_tracks_leading_spaces_before_keyword)
{
    t_lexer lexer;

    lexer_init(&lexer, "  MOVE   FIELD");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_KEYWORD_MOVE, "MOVE", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_IDENTIFIER, "FIELD", 1, 10) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 15) != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_lexer_reports_percent_between_numbers)
{
    t_lexer lexer;
    t_lexer_token token;

    lexer_init(&lexer, "10%20");
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "10", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_FAILURE)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_UNKNOWN, "%", 1, 3) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_NUMERIC_LITERAL, "20", 1, 4) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_expect_next(&lexer, LEXER_TOKEN_END_OF_FILE, NULL, 1, 6) != FT_SUCCESS)
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
        std::printf("Assertion failed: lexer should fail for unterminated string literal\n");
        return (FT_FAILURE);
    }
    if (token.kind != LEXER_TOKEN_UNKNOWN)
    {
        std::printf("Assertion failed: unterminated string should produce unknown token\n");
        return (FT_FAILURE);
    }
    if (token.line != 1 || token.column != 6)
    {
        std::printf("Assertion failed: unterminated string should report start location\n");
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
        {"lexer_keyword_lookup_handles_null_and_empty", test_lexer_keyword_lookup_handles_null_and_empty},
        {"lexer_keyword_lookup_identifies_end_keywords", test_lexer_keyword_lookup_identifies_end_keywords},
        {"lexer_keyword_lookup_identifies_io_keywords", test_lexer_keyword_lookup_identifies_io_keywords},
        {"lexer_keyword_lookup_identifies_copy_replacing_keywords",
            test_lexer_keyword_lookup_identifies_copy_replacing_keywords},
        {"lexer_keyword_lookup_identifies_call_argument_keywords",
            test_lexer_keyword_lookup_identifies_call_argument_keywords},
        {"lexer_keyword_lookup_identifies_data_clause_keywords",
            test_lexer_keyword_lookup_identifies_data_clause_keywords},
        {"lexer_keyword_lookup_identifies_control_flow_keywords",
            test_lexer_keyword_lookup_identifies_control_flow_keywords},
        {"lexer_keyword_lookup_identifies_declarative_keywords",
            test_lexer_keyword_lookup_identifies_declarative_keywords},
        {"lexer_keyword_lookup_identifies_varying_keywords",
            test_lexer_keyword_lookup_identifies_varying_keywords},
        {"lexer_keyword_lookup_rejects_prefix_matches", test_lexer_keyword_lookup_rejects_prefix_matches},
        {"lexer_trivia_detects_whitespace", test_lexer_trivia_detects_whitespace},
        {"lexer_trivia_detects_comments", test_lexer_trivia_detects_comments},
        {"lexer_trivia_handles_null_and_empty", test_lexer_trivia_handles_null_and_empty},
        {"lexer_trivia_rejects_mixed_text", test_lexer_trivia_rejects_mixed_text},
        {"lexer_trivia_classifies_multiline_comment_text",
            test_lexer_trivia_classifies_multiline_comment_text},
        {"lexer_distinguishes_assignment_and_equality", test_lexer_distinguishes_assignment_and_equality},
        {"lexer_tokenizes_minus_operator", test_lexer_tokenizes_minus_operator},
        {"lexer_tokenizes_star_operator", test_lexer_tokenizes_star_operator},
        {"lexer_tokenizes_slash_operator", test_lexer_tokenizes_slash_operator},
        {"lexer_tokenizes_sample_program", test_lexer_tokenizes_sample_program},
        {"lexer_tokenizes_plus_operator", test_lexer_tokenizes_plus_operator},
        {"lexer_tokenizes_assignment_followed_by_identifier",
            test_lexer_tokenizes_assignment_followed_by_identifier},
        {"lexer_tokenizes_equality_without_spaces", test_lexer_tokenizes_equality_without_spaces},
        {"lexer_tracks_columns_after_blank_lines", test_lexer_tracks_columns_after_blank_lines},
        {"lexer_skips_indented_comment_line", test_lexer_skips_indented_comment_line},
        {"lexer_null_source_produces_eof", test_lexer_null_source_produces_eof},
        {"lexer_skips_extended_whitespace", test_lexer_skips_extended_whitespace},
        {"lexer_skips_comment_without_newline", test_lexer_skips_comment_without_newline},
        {"lexer_tokenizes_double_quoted_string_literal", test_lexer_tokenizes_double_quoted_string_literal},
        {"lexer_splits_integer_period_identifier", test_lexer_splits_integer_period_identifier},
        {"lexer_splits_leading_hyphen_from_identifier", test_lexer_splits_leading_hyphen_from_identifier},
        {"lexer_tokenizes_punctuation_sequence", test_lexer_tokenizes_punctuation_sequence},
        {"lexer_tokenizes_relational_operators", test_lexer_tokenizes_relational_operators},
        {"lexer_tokenizes_decimal_numeric_literal", test_lexer_tokenizes_decimal_numeric_literal},
        {"lexer_keeps_trailing_period_separate_from_integer",
            test_lexer_keeps_trailing_period_separate_from_integer},
        {"lexer_tokenizes_single_quoted_string_literal", test_lexer_tokenizes_single_quoted_string_literal},
        {"lexer_skips_comment_lines", test_lexer_skips_comment_lines},
        {"lexer_tokenizes_identifier_with_hyphen_underscore_and_digits",
            test_lexer_tokenizes_identifier_with_hyphen_underscore_and_digits},
        {"lexer_reports_unknown_character", test_lexer_reports_unknown_character},
        {"lexer_next_token_rejects_null_lexer", test_lexer_next_token_rejects_null_lexer},
        {"lexer_next_token_rejects_null_token", test_lexer_next_token_rejects_null_token},
        {"lexer_reports_unterminated_single_quoted_string",
            test_lexer_reports_unterminated_single_quoted_string},
        {"lexer_reports_unknown_character_after_token", test_lexer_reports_unknown_character_after_token},
        {"lexer_tokenizes_less_than_followed_by_keyword", test_lexer_tokenizes_less_than_followed_by_keyword},
        {"lexer_tokenizes_greater_than_followed_by_keyword", test_lexer_tokenizes_greater_than_followed_by_keyword},
        {"lexer_tokenizes_less_or_equal_without_spaces", test_lexer_tokenizes_less_or_equal_without_spaces},
        {"lexer_tokenizes_greater_or_equal_without_spaces", test_lexer_tokenizes_greater_or_equal_without_spaces},
        {"lexer_tokenizes_not_equal_without_spaces", test_lexer_tokenizes_not_equal_without_spaces},
        {"lexer_tokenizes_decimal_then_identifier", test_lexer_tokenizes_decimal_then_identifier},
        {"lexer_tokenizes_lowercase_keyword", test_lexer_tokenizes_lowercase_keyword},
        {"lexer_tokenizes_multiline_string_literal", test_lexer_tokenizes_multiline_string_literal},
        {"lexer_tokenizes_consecutive_equals", test_lexer_tokenizes_consecutive_equals},
        {"lexer_splits_decimal_with_second_period", test_lexer_splits_decimal_with_second_period},
        {"lexer_splits_double_period_between_integers", test_lexer_splits_double_period_between_integers},
        {"lexer_tokenizes_slash_star_as_operators", test_lexer_tokenizes_slash_star_as_operators},
        {"lexer_allows_identifier_trailing_hyphen", test_lexer_allows_identifier_trailing_hyphen},
        {"lexer_reports_leading_underscore_as_unknown", test_lexer_reports_leading_underscore_as_unknown},
        {"lexer_tracks_columns_after_tabs", test_lexer_tracks_columns_after_tabs},
        {"lexer_tracks_position_after_comment_and_blank_line",
            test_lexer_tracks_position_after_comment_and_blank_line},
        {"lexer_tokenizes_empty_double_quoted_string", test_lexer_tokenizes_empty_double_quoted_string},
        {"lexer_tokenizes_empty_single_quoted_string", test_lexer_tokenizes_empty_single_quoted_string},
        {"lexer_tracks_crlf_as_single_line_break", test_lexer_tracks_crlf_as_single_line_break},
        {"lexer_skips_crlf_comment_line", test_lexer_skips_crlf_comment_line},
        {"lexer_tokenizes_operators_across_lines", test_lexer_tokenizes_operators_across_lines},
        {"lexer_tokenizes_greater_then_less_without_spaces",
            test_lexer_tokenizes_greater_then_less_without_spaces},
        {"lexer_tokenizes_repeated_less_than_as_separate_tokens",
            test_lexer_tokenizes_repeated_less_than_as_separate_tokens},
        {"lexer_splits_integer_across_newline", test_lexer_splits_integer_across_newline},
        {"lexer_keeps_keyword_with_underscore_as_identifier",
            test_lexer_keeps_keyword_with_underscore_as_identifier},
        {"lexer_keeps_keyword_suffix_as_identifier", test_lexer_keeps_keyword_suffix_as_identifier},
        {"lexer_reports_eof_after_whitespace_only_source",
            test_lexer_reports_eof_after_whitespace_only_source},
        {"lexer_reports_eof_after_comment_and_trailing_spaces",
            test_lexer_reports_eof_after_comment_and_trailing_spaces},
        {"lexer_tokenizes_identifier_with_repeated_hyphens",
            test_lexer_tokenizes_identifier_with_repeated_hyphens},
        {"lexer_tokenizes_lowercase_identifier_with_digits_and_underscore",
            test_lexer_tokenizes_lowercase_identifier_with_digits_and_underscore},
        {"lexer_splits_numeric_minus_numeric_without_spaces",
            test_lexer_splits_numeric_minus_numeric_without_spaces},
        {"lexer_tokenizes_string_followed_by_identifier_without_space",
            test_lexer_tokenizes_string_followed_by_identifier_without_space},
        {"lexer_splits_leading_period_from_digit", test_lexer_splits_leading_period_from_digit},
        {"lexer_tokenizes_comma_separated_identifiers", test_lexer_tokenizes_comma_separated_identifiers},
        {"lexer_reports_unknown_after_newline", test_lexer_reports_unknown_after_newline},
        {"lexer_skips_comment_immediately_after_token", test_lexer_skips_comment_immediately_after_token},
        {"lexer_tokenizes_pic_clause_symbols", test_lexer_tokenizes_pic_clause_symbols},
        {"lexer_tokenizes_keyword_followed_by_period_without_space",
            test_lexer_tokenizes_keyword_followed_by_period_without_space},
        {"lexer_tokenizes_colon_and_semicolon_between_identifiers",
            test_lexer_tokenizes_colon_and_semicolon_between_identifiers},
        {"lexer_tokenizes_string_containing_single_quote", test_lexer_tokenizes_string_containing_single_quote},
        {"lexer_tokenizes_single_quoted_string_containing_double_quote",
            test_lexer_tokenizes_single_quoted_string_containing_double_quote},
        {"lexer_skips_multiple_comment_lines", test_lexer_skips_multiple_comment_lines},
        {"lexer_reports_unknown_after_tab", test_lexer_reports_unknown_after_tab},
        {"lexer_tokenizes_repeated_star_as_operators", test_lexer_tokenizes_repeated_star_as_operators},
        {"lexer_tokenizes_slash_between_identifiers", test_lexer_tokenizes_slash_between_identifiers},
        {"lexer_tokenizes_decimal_minus_decimal_without_spaces",
            test_lexer_tokenizes_decimal_minus_decimal_without_spaces},
        {"lexer_tokenizes_mixed_case_end_perform_keyword",
            test_lexer_tokenizes_mixed_case_end_perform_keyword},
        {"lexer_splits_digit_then_identifier", test_lexer_splits_digit_then_identifier},
        {"lexer_splits_numeric_plus_numeric_without_spaces",
            test_lexer_splits_numeric_plus_numeric_without_spaces},
        {"lexer_tokenizes_identifier_in_parentheses", test_lexer_tokenizes_identifier_in_parentheses},
        {"lexer_tokenizes_less_equal_then_greater", test_lexer_tokenizes_less_equal_then_greater},
        {"lexer_tokenizes_not_equal_then_assign", test_lexer_tokenizes_not_equal_then_assign},
        {"lexer_tokenizes_star_space_greater_as_operators",
            test_lexer_tokenizes_star_space_greater_as_operators},
        {"lexer_tracks_form_feed_column", test_lexer_tracks_form_feed_column},
        {"lexer_tracks_vertical_tab_column", test_lexer_tracks_vertical_tab_column},
        {"lexer_tokenizes_keyword_comma_keyword", test_lexer_tokenizes_keyword_comma_keyword},
        {"lexer_empty_source_produces_eof", test_lexer_empty_source_produces_eof},
        {"lexer_tokenizes_two_equality_operators_without_spaces",
            test_lexer_tokenizes_two_equality_operators_without_spaces},
        {"lexer_tokenizes_assign_then_greater_without_spaces",
            test_lexer_tokenizes_assign_then_greater_without_spaces},
        {"lexer_tokenizes_assign_then_less_without_spaces",
            test_lexer_tokenizes_assign_then_less_without_spaces},
        {"lexer_tokenizes_less_then_less_or_equal", test_lexer_tokenizes_less_then_less_or_equal},
        {"lexer_tokenizes_greater_then_greater_or_equal",
            test_lexer_tokenizes_greater_then_greater_or_equal},
        {"lexer_tokenizes_keyword_followed_by_string_without_space",
            test_lexer_tokenizes_keyword_followed_by_string_without_space},
        {"lexer_tokenizes_strings_across_lines", test_lexer_tokenizes_strings_across_lines},
        {"lexer_tracks_carriage_return_as_column_whitespace",
            test_lexer_tracks_carriage_return_as_column_whitespace},
        {"lexer_skips_spaced_comment_after_token", test_lexer_skips_spaced_comment_after_token},
        {"lexer_tokenizes_mixed_case_working_storage_keyword",
            test_lexer_tokenizes_mixed_case_working_storage_keyword},
        {"lexer_tokenizes_data_keyword_with_period", test_lexer_tokenizes_data_keyword_with_period},
        {"lexer_tokenizes_identifier_argument_list", test_lexer_tokenizes_identifier_argument_list},
        {"lexer_tokenizes_numeric_slash_numeric_without_spaces",
            test_lexer_tokenizes_numeric_slash_numeric_without_spaces},
        {"lexer_tokenizes_numeric_star_numeric_without_spaces",
            test_lexer_tokenizes_numeric_star_numeric_without_spaces},
        {"lexer_tokenizes_numeric_comma_numeric", test_lexer_tokenizes_numeric_comma_numeric},
        {"lexer_tokenizes_period_separated_identifiers",
            test_lexer_tokenizes_period_separated_identifiers},
        {"lexer_tracks_tab_between_string_literals", test_lexer_tracks_tab_between_string_literals},
        {"lexer_reports_unterminated_empty_double_quote",
            test_lexer_reports_unterminated_empty_double_quote},
        {"lexer_reports_repeated_unknown_characters", test_lexer_reports_repeated_unknown_characters},
        {"lexer_tokenizes_environment_division_compact", test_lexer_tokenizes_environment_division_compact},
        {"lexer_tokenizes_file_section_compact", test_lexer_tokenizes_file_section_compact},
        {"lexer_tokenizes_call_using_reference_keywords",
            test_lexer_tokenizes_call_using_reference_keywords},
        {"lexer_tokenizes_open_close_read_write_keywords",
            test_lexer_tokenizes_open_close_read_write_keywords},
        {"lexer_tokenizes_boolean_condition_keywords", test_lexer_tokenizes_boolean_condition_keywords},
        {"lexer_tokenizes_identifier_after_multiline_comment_with_indent",
            test_lexer_tokenizes_identifier_after_multiline_comment_with_indent},
        {"lexer_tokenizes_repeated_right_parens", test_lexer_tokenizes_repeated_right_parens},
        {"lexer_tokenizes_repeated_left_parens", test_lexer_tokenizes_repeated_left_parens},
        {"lexer_tokenizes_identifier_after_decimal_without_space",
            test_lexer_tokenizes_identifier_after_decimal_without_space},
        {"lexer_reports_unknown_between_identifiers", test_lexer_reports_unknown_between_identifiers},
        {"lexer_tokenizes_stop_run_sentence", test_lexer_tokenizes_stop_run_sentence},
        {"lexer_tokenizes_declarative_error_clause", test_lexer_tokenizes_declarative_error_clause},
        {"lexer_tokenizes_occurs_times_clause", test_lexer_tokenizes_occurs_times_clause},
        {"lexer_tokenizes_depending_on_identifier", test_lexer_tokenizes_depending_on_identifier},
        {"lexer_tokenizes_length_of_identifier", test_lexer_tokenizes_length_of_identifier},
        {"lexer_tokenizes_content_reference_keywords", test_lexer_tokenizes_content_reference_keywords},
        {"lexer_tokenizes_replacing_position_keywords", test_lexer_tokenizes_replacing_position_keywords},
        {"lexer_tokenizes_compute_assignment_without_space",
            test_lexer_tokenizes_compute_assignment_without_space},
        {"lexer_tokenizes_abs_negative_argument", test_lexer_tokenizes_abs_negative_argument},
        {"lexer_keeps_select_suffix_as_identifier", test_lexer_keeps_select_suffix_as_identifier},
        {"lexer_tokenizes_copy_replacing_clause", test_lexer_tokenizes_copy_replacing_clause},
        {"lexer_tokenizes_perform_varying_clause", test_lexer_tokenizes_perform_varying_clause},
        {"lexer_tokenizes_if_else_end_if_sequence", test_lexer_tokenizes_if_else_end_if_sequence},
        {"lexer_tokenizes_select_assign_clause", test_lexer_tokenizes_select_assign_clause},
        {"lexer_tokenizes_fd_record_declaration", test_lexer_tokenizes_fd_record_declaration},
        {"lexer_tokenizes_pic_value_clause", test_lexer_tokenizes_pic_value_clause},
        {"lexer_tokenizes_mod_expression", test_lexer_tokenizes_mod_expression},
        {"lexer_tokenizes_decimal_plus_decimal_without_spaces",
            test_lexer_tokenizes_decimal_plus_decimal_without_spaces},
        {"lexer_reports_unknown_after_comment", test_lexer_reports_unknown_after_comment},
        {"lexer_keeps_program_id_suffix_as_identifier", test_lexer_keeps_program_id_suffix_as_identifier},
        {"lexer_tokenizes_lowercase_stop_run_sentence", test_lexer_tokenizes_lowercase_stop_run_sentence},
        {"lexer_tokenizes_display_string_sentence", test_lexer_tokenizes_display_string_sentence},
        {"lexer_tokenizes_move_numeric_to_identifier", test_lexer_tokenizes_move_numeric_to_identifier},
        {"lexer_tokenizes_read_identifier_sentence", test_lexer_tokenizes_read_identifier_sentence},
        {"lexer_tokenizes_write_identifier_sentence", test_lexer_tokenizes_write_identifier_sentence},
        {"lexer_tokenizes_call_string_using_content", test_lexer_tokenizes_call_string_using_content},
        {"lexer_tokenizes_declaratives_sentence", test_lexer_tokenizes_declaratives_sentence},
        {"lexer_tokenizes_identifier_with_mixed_separators",
            test_lexer_tokenizes_identifier_with_mixed_separators},
        {"lexer_tokenizes_decimal_slash_decimal_without_spaces",
            test_lexer_tokenizes_decimal_slash_decimal_without_spaces},
        {"lexer_reports_unterminated_single_quote_after_newline",
            test_lexer_reports_unterminated_single_quote_after_newline},
        {"lexer_tokenizes_procedure_division_sentence", test_lexer_tokenizes_procedure_division_sentence},
        {"lexer_tokenizes_identification_program_id_headers",
            test_lexer_tokenizes_identification_program_id_headers},
        {"lexer_tokenizes_77_level_pic_declaration", test_lexer_tokenizes_77_level_pic_declaration},
        {"lexer_tokenizes_01_group_declaration", test_lexer_tokenizes_01_group_declaration},
        {"lexer_tokenizes_open_read_write_close_sequence",
            test_lexer_tokenizes_open_read_write_close_sequence},
        {"lexer_tokenizes_if_less_or_equal_condition", test_lexer_tokenizes_if_less_or_equal_condition},
        {"lexer_tokenizes_if_not_equal_condition", test_lexer_tokenizes_if_not_equal_condition},
        {"lexer_tokenizes_end_perform_sentence", test_lexer_tokenizes_end_perform_sentence},
        {"lexer_tokenizes_false_period", test_lexer_tokenizes_false_period},
        {"lexer_tokenizes_string_with_period_inside", test_lexer_tokenizes_string_with_period_inside},
        {"lexer_tokenizes_end_if_sentence", test_lexer_tokenizes_end_if_sentence},
        {"lexer_tokenizes_true_period", test_lexer_tokenizes_true_period},
        {"lexer_tokenizes_not_false_condition", test_lexer_tokenizes_not_false_condition},
        {"lexer_tokenizes_use_after_error_on_clause", test_lexer_tokenizes_use_after_error_on_clause},
        {"lexer_tokenizes_using_reference_content_length_of",
            test_lexer_tokenizes_using_reference_content_length_of},
        {"lexer_tokenizes_varying_from_by_until_clause",
            test_lexer_tokenizes_varying_from_by_until_clause},
        {"lexer_tokenizes_parenthesized_addition_times_identifier",
            test_lexer_tokenizes_parenthesized_addition_times_identifier},
        {"lexer_tokenizes_adjacent_punctuation_between_identifiers",
            test_lexer_tokenizes_adjacent_punctuation_between_identifiers},
        {"lexer_tracks_leading_spaces_before_keyword", test_lexer_tracks_leading_spaces_before_keyword},
        {"lexer_reports_percent_between_numbers", test_lexer_reports_percent_between_numbers},
        {"lexer_reports_unterminated_string", test_lexer_reports_unterminated_string}
    };

    if (count)
        *count = sizeof(tests) / sizeof(tests[0]);
    return (tests);
}
