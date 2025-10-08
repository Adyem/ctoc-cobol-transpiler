#include <cerrno>
#include <cstddef>
#include <cstdlib>
#include <fcntl.h>
#include <sys/wait.h>
#include <unistd.h>

#include "libft/Libft/libft.hpp"
#include "libft/Printf/printf.hpp"
#include "ast.hpp"
#include "lexer.hpp"
#include "lexer_token.hpp"
#include "runtime_file.hpp"
#include "runtime_scalar.hpp"
#include "runtime_string.hpp"
#include "transpiler_context.hpp"
#include "transpiler_pipeline.hpp"

typedef struct s_test_case
{
    const char *name;
    int (*execute)(void);
}   t_test_case;

static int test_expect_success(int status, const char *message)
{
    if (status == FT_SUCCESS)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s\n", message);
    return (FT_FAILURE);
}

static int test_expect_int_equal(int actual, int expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %d, got %d)\n", message, expected, actual);
    return (FT_FAILURE);
}

static int test_expect_char_equal(char actual, char expected, const char *message)
{
    if (actual == expected)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %c, got %c)\n", message, expected, actual);
    return (FT_FAILURE);
}

static int test_expect_cstring_equal(const char *actual, const char *expected, const char *message)
{
    if (!actual && !expected)
        return (FT_SUCCESS);
    if (!actual || !expected)
    {
        if (message)
            pf_printf("Assertion failed: %s (expected %s, got %s)\n", message,
                expected ? expected : "(null)", actual ? actual : "(null)");
        return (FT_FAILURE);
    }
    if (ft_strncmp(actual, expected, ft_strlen(expected) + 1) == 0)
        return (FT_SUCCESS);
    if (message)
        pf_printf("Assertion failed: %s (expected %s, got %s)\n", message, expected, actual);
    return (FT_FAILURE);
}

static int test_expect_token(const t_lexer_token *token, t_lexer_token_kind expected_kind,
    const char *expected_lexeme, size_t expected_line, size_t expected_column)
{
    size_t index;
    size_t expected_length;

    if (!token)
        return (FT_FAILURE);
    if (token->kind != expected_kind)
    {
        pf_printf("Assertion failed: token kind mismatch (expected %d, got %d)\n", expected_kind, token->kind);
        return (FT_FAILURE);
    }
    if (token->line != expected_line)
    {
        pf_printf("Assertion failed: token line mismatch (expected %zu, got %zu)\n", expected_line, token->line);
        return (FT_FAILURE);
    }
    if (token->column != expected_column)
    {
        pf_printf("Assertion failed: token column mismatch (expected %zu, got %zu)\n", expected_column, token->column);
        return (FT_FAILURE);
    }
    if (!expected_lexeme)
    {
        if (token->length != 0)
        {
            pf_printf("Assertion failed: token length mismatch (expected 0, got %zu)\n", token->length);
            return (FT_FAILURE);
        }
        return (FT_SUCCESS);
    }
    expected_length = ft_strlen(expected_lexeme);
    if (token->length != expected_length)
    {
        pf_printf("Assertion failed: token length mismatch (expected %zu, got %zu)\n", expected_length, token->length);
        return (FT_FAILURE);
    }
    if (!token->lexeme)
    {
        pf_printf("Assertion failed: token lexeme should not be null\n");
        return (FT_FAILURE);
    }
    index = 0;
    while (index < expected_length)
    {
        if (token->lexeme[index] != expected_lexeme[index])
        {
            pf_printf("Assertion failed: token lexeme mismatch at index %zu (expected %c, got %c)\n",
                index, expected_lexeme[index], token->lexeme[index]);
            return (FT_FAILURE);
        }
        index += 1;
    }
    return (FT_SUCCESS);
}

FT_TEST(test_ast_node_add_child_preserves_order)
{
    t_ast_node *program;
    t_ast_node *division;
    t_ast_node *sequence;
    int status;

    program = ast_node_create(AST_NODE_PROGRAM);
    if (!program)
        return (FT_FAILURE);
    division = ast_node_create(AST_NODE_IDENTIFICATION_DIVISION);
    if (!division)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, division) != FT_SUCCESS)
    {
        ast_node_destroy(division);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    sequence = ast_node_create(AST_NODE_STATEMENT_SEQUENCE);
    if (!sequence)
    {
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    if (ast_node_add_child(program, sequence) != FT_SUCCESS)
    {
        ast_node_destroy(sequence);
        ast_node_destroy(program);
        return (FT_FAILURE);
    }
    status = FT_SUCCESS;
    if (ast_node_child_count(program) != 2)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 0) != division)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 1) != sequence)
        status = FT_FAILURE;
    else if (ast_node_get_child(program, 1)->kind != AST_NODE_STATEMENT_SEQUENCE)
        status = FT_FAILURE;
    ast_node_destroy(program);
    return (status);
}

FT_TEST(test_ast_node_set_token_copies_lexeme)
{
    t_ast_node *node;
    t_lexer_token token;
    char buffer[16];
    const char *source;
    size_t length;
    size_t index;

    node = ast_node_create(AST_NODE_IDENTIFIER);
    if (!node)
        return (FT_FAILURE);
    source = "program";
    length = 0;
    while (source[length] != '\0')
    {
        buffer[length] = source[length];
        length += 1;
    }
    buffer[length] = '\0';
    token.kind = LEXER_TOKEN_IDENTIFIER;
    token.lexeme = buffer;
    token.length = length;
    token.line = 4;
    token.column = 2;
    if (ast_node_set_token(node, &token) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    buffer[0] = 'X';
    if (!node->token.lexeme)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.kind != LEXER_TOKEN_IDENTIFIER)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.length != length)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.lexeme[0] != 'p')
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    index = 0;
    while (index < node->token.length)
    {
        if (node->token.lexeme[index] != source[index])
        {
            ast_node_destroy(node);
            return (FT_FAILURE);
        }
        index += 1;
    }
    if (node->token.lexeme[node->token.length] != '\0')
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (ast_node_set_token(node, NULL) != FT_SUCCESS)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.lexeme != NULL)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.length != 0)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    if (node->token.kind != LEXER_TOKEN_UNKNOWN)
    {
        ast_node_destroy(node);
        return (FT_FAILURE);
    }
    ast_node_destroy(node);
    return (FT_SUCCESS);
}

static int test_write_text_file(const char *path, const char *contents)
{
    int fd;
    size_t length;
    size_t offset;
    ssize_t result;

    if (!path)
        return (FT_FAILURE);
    if (!contents)
        return (FT_FAILURE);
    fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0)
        return (FT_FAILURE);
    length = ft_strlen(contents);
    offset = 0;
    while (offset < length)
    {
        result = write(fd, contents + offset, length - offset);
        if (result < 0)
        {
            close(fd);
            return (FT_FAILURE);
        }
        offset += static_cast<size_t>(result);
    }
    if (close(fd) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_read_text_file(const char *path, char *buffer, size_t buffer_size)
{
    int fd;
    size_t offset;
    ssize_t result;

    if (!path)
        return (FT_FAILURE);
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    fd = open(path, O_RDONLY);
    if (fd < 0)
        return (FT_FAILURE);
    offset = 0;
    while (offset + 1 < buffer_size)
    {
        result = read(fd, buffer + offset, buffer_size - 1 - offset);
        if (result < 0)
        {
            close(fd);
            return (FT_FAILURE);
        }
        if (result == 0)
            break;
        offset += static_cast<size_t>(result);
    }
    if (offset + 1 >= buffer_size)
    {
        close(fd);
        return (FT_FAILURE);
    }
    buffer[offset] = '\0';
    if (close(fd) != 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_execute_command(const char *command, int expect_success)
{
    int pipe_fds[2];
    pid_t pid;
    char buffer[256];
    ssize_t bytes_read;
    int status;

    if (!command)
        return (FT_FAILURE);
    if (pipe(pipe_fds) != 0)
        return (FT_FAILURE);
    pid = fork();
    if (pid < 0)
    {
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        return (FT_FAILURE);
    }
    if (pid == 0)
    {
        if (dup2(pipe_fds[1], STDOUT_FILENO) < 0)
            _exit(127);
        if (dup2(pipe_fds[1], STDERR_FILENO) < 0)
            _exit(127);
        close(pipe_fds[0]);
        close(pipe_fds[1]);
        execl("/bin/sh", "sh", "-c", command, (char *)NULL);
        _exit(127);
    }
    close(pipe_fds[1]);
    while (1)
    {
        bytes_read = read(pipe_fds[0], buffer, sizeof(buffer));
        if (bytes_read > 0)
            continue ;
        if (bytes_read == 0)
            break ;
        if (errno == EINTR)
            continue ;
        close(pipe_fds[0]);
        while (waitpid(pid, &status, 0) < 0)
        {
            if (errno != EINTR)
                return (FT_FAILURE);
        }
        return (FT_FAILURE);
    }
    close(pipe_fds[0]);
    while (waitpid(pid, &status, 0) < 0)
    {
        if (errno != EINTR)
            return (FT_FAILURE);
    }
    if (WIFEXITED(status) == 0)
        return (FT_FAILURE);
    if (expect_success)
    {
        if (WEXITSTATUS(status) != 0)
            return (FT_FAILURE);
    }
    else if (WEXITSTATUS(status) == 0)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int test_run_command(const char *command)
{
    return (test_execute_command(command, 1));
}

static void test_remove_file(const char *path)
{
    if (!path)
        return ;
    unlink(path);
}

static int test_run_command_expect_failure(const char *command)
{
    return (test_execute_command(command, 0));
}

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

    source = "identification division.\n"
        "program-id. sample-program.\n"
        "*> comment line\n"
        "move 42 to result.\n";
    lexer_init(&lexer, source);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_IDENTIFICATION, "identification", 1, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_DIVISION, "division", 1, 16) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 1, 24) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_PROGRAM_ID, "program-id", 2, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 2, 11) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_IDENTIFIER, "sample-program", 2, 13) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 2, 27) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_MOVE, "move", 4, 1) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_NUMERIC_LITERAL, "42", 4, 6) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_KEYWORD_TO, "to", 4, 9) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_IDENTIFIER, "result", 4, 12) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_PERIOD, ".", 4, 18) != FT_SUCCESS)
        return (FT_FAILURE);
    if (lexer_next_token(&lexer, &token) != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_token(&token, LEXER_TOKEN_END_OF_FILE, NULL, 5, 1) != FT_SUCCESS)
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

static void test_cleanup_example_artifacts(const char *source_path, const char *binary_path, const char *output_path)
{
    test_remove_file(output_path);
    test_remove_file(binary_path);
    test_remove_file(source_path);
}

FT_TEST(test_runtime_file_write_and_read_text)
{
    const char *path;
    const char *contents;
    t_runtime_file file;
    char buffer[128];
    size_t bytes_read;
    size_t expected_length;

    path = "test_runtime_file.txt";
    contents = "runtime-file-contents";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path), "runtime_file_open_write should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_file_write(&file, contents, ft_strlen(contents)), "runtime_file_write should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_read(&file, path), "runtime_file_open_read should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    ft_memset(buffer, 0, sizeof(buffer));
    if (test_expect_success(runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read), "runtime_file_read should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    expected_length = static_cast<size_t>(ft_strlen(contents));
    if (bytes_read != expected_length)
    {
        pf_printf("Assertion failed: runtime_file_read should report full length (expected %zu, got %zu)\n",
            expected_length, bytes_read);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, contents, "runtime_file_read should capture file contents") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    runtime_file_close(&file);
    test_remove_file(path);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_open_read_missing_path)
{
    t_runtime_file file;

    runtime_file_init(&file);
    if (runtime_file_open_read(&file, "nonexistent-runtime-file.txt") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_open_read should fail for missing files\n");
        runtime_file_close(&file);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should allow closing unopened file") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_reopen_transitions_modes)
{
    const char *path;
    const char *contents;
    t_runtime_file file;
    char buffer[64];
    size_t bytes_read;
    size_t expected_length;

    path = "test_runtime_file_reopen.txt";
    contents = "runtime-file-reopen";
    runtime_file_init(&file);
    if (test_expect_success(runtime_file_open_write(&file, path), "runtime_file_open_write should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_file_write(&file, contents, ft_strlen(contents)),
        "runtime_file_write should succeed") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_open_read(&file, path),
        "runtime_file_open_read should replace existing descriptor") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    ft_memset(buffer, 0, sizeof(buffer));
    if (test_expect_success(runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read),
        "runtime_file_read should succeed after reopening") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    expected_length = ft_strlen(contents);
    if (bytes_read != expected_length)
    {
        pf_printf("Assertion failed: runtime_file_read should report full length after reopening (expected %zu, got %zu)\n",
            expected_length, bytes_read);
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(buffer, contents, "runtime_file_read should return written contents") != FT_SUCCESS)
    {
        runtime_file_close(&file);
        test_remove_file(path);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should succeed") != FT_SUCCESS)
    {
        test_remove_file(path);
        return (FT_FAILURE);
    }
    test_remove_file(path);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_file_requires_open_descriptor)
{
    t_runtime_file file;
    char buffer[8];
    size_t bytes_read;

    runtime_file_init(&file);
    if (runtime_file_read(&file, buffer, sizeof(buffer), &bytes_read) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_read should fail without open descriptor\n");
        return (FT_FAILURE);
    }
    if (runtime_file_write(&file, "x", 1) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_file_write should fail without open descriptor\n");
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_file_close(&file), "runtime_file_close should allow closing unopened file") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_add_and_subtract)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, 72);
    runtime_int_set(&right, 30);
    if (test_expect_success(runtime_int_add(left, right, &result), "runtime_int_add should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 102, "runtime_int_add should add values") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_int_subtract(result, left, &result), "runtime_int_subtract should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(result.value, 30, "runtime_int_subtract should subtract values") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_add_detects_overflow)
{
    t_runtime_int left;
    t_runtime_int right;
    t_runtime_int result;

    runtime_int_set(&left, FT_INT_MAX);
    runtime_int_set(&right, 1);
    runtime_int_set(&result, 73);
    if (runtime_int_add(left, right, &result) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_add should detect overflow\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(result.value, 73, "runtime_int_add should not update on overflow") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_to_string)
{
    t_runtime_int value;
    char buffer[32];

    runtime_int_set(&value, 512);
    if (test_expect_success(runtime_int_to_string(value, buffer, sizeof(buffer)), "runtime_int_to_string should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (ft_strncmp(buffer, "512", 4) != 0)
    {
        pf_printf("Assertion failed: runtime_int_to_string should write textual representation\n");
        return (FT_FAILURE);
    }
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_to_string_rejects_small_buffer)
{
    t_runtime_int value;
    char buffer[1];

    runtime_int_set(&value, 8);
    buffer[0] = 'Z';
    if (runtime_int_to_string(value, buffer, sizeof(buffer)) != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_to_string should reject undersized buffers\n");
        return (FT_FAILURE);
    }
    if (test_expect_char_equal(buffer[0], 'Z', "runtime_int_to_string should leave buffer unchanged on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_int_from_string)
{
    t_runtime_int value;

    runtime_int_set(&value, 0);
    if (test_expect_success(runtime_int_from_string(&value, "77"), "runtime_int_from_string should parse integers") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(value.value, 77, "runtime_int_from_string should store parsed values") != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_int_set(&value, 42);
    if (runtime_int_from_string(&value, "abc") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_int_from_string should reject invalid text\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, 42, "runtime_int_from_string should keep destination on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_char_transforms)
{
    t_runtime_char value;

    runtime_char_from_string(&value, "a");
    runtime_char_to_upper(&value);
    if (test_expect_char_equal(value.value, 'A', "runtime_char_to_upper should uppercase values") != FT_SUCCESS)
        return (FT_FAILURE);
    runtime_char_to_lower(&value);
    if (test_expect_char_equal(value.value, 'a', "runtime_char_to_lower should lowercase values") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_char_from_string_rejects_empty_input)
{
    t_runtime_char value;

    runtime_char_set(&value, 'q');
    if (runtime_char_from_string(&value, "") != FT_FAILURE)
    {
        pf_printf("Assertion failed: runtime_char_from_string should reject empty input\n");
        return (FT_FAILURE);
    }
    if (test_expect_char_equal(value.value, 'q', "runtime_char_from_string should keep destination on failure") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_char_to_string_and_compare)
{
    t_runtime_char left;
    t_runtime_char right;
    char buffer[4];

    runtime_char_set(&left, 'X');
    runtime_char_set(&right, 'Y');
    if (runtime_char_to_string(left, buffer, sizeof(buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: runtime_char_to_string should succeed\n");
        return (FT_FAILURE);
    }
    if (ft_strncmp(buffer, "X", 2) != 0)
    {
        pf_printf("Assertion failed: runtime_char_to_string should copy character to buffer\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(runtime_char_compare(left, right), -1, "runtime_char_compare should order characters") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(right, left), 1, "runtime_char_compare should reverse ordering") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_int_equal(runtime_char_compare(left, left), 0, "runtime_char_compare should detect equality") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_assign_and_trim)
{
    t_runtime_string value;

    if (test_expect_success(runtime_string_init(&value, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_assign(&value, "   hello world  "), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_trim(&value), "runtime_string_trim should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_cstring_equal(value.data, "hello world", "runtime_string_trim should remove leading and trailing whitespace") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(static_cast<int>(value.length), 11, "runtime_string_trim should update length") != FT_SUCCESS)
    {
        runtime_string_dispose(&value);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&value);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_compare_orders_text)
{
    t_runtime_string left;
    t_runtime_string right;
    int comparison;

    if (test_expect_success(runtime_string_init(&left, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_init(&right, 0), "runtime_string_init should succeed") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&left, "APPLE"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "APRICOT"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&left, &right);
    if (test_expect_int_equal(comparison, -1, "runtime_string_compare should order lexicographically") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&right, &left);
    if (test_expect_int_equal(comparison, 1, "runtime_string_compare should reverse ordering") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    if (test_expect_success(runtime_string_assign(&right, "APPLE"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    comparison = runtime_string_compare(&left, &right);
    if (test_expect_int_equal(comparison, 0, "runtime_string_compare should report equality") != FT_SUCCESS)
    {
        runtime_string_dispose(&left);
        runtime_string_dispose(&right);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&left);
    runtime_string_dispose(&right);
    return (FT_SUCCESS);
}

FT_TEST(test_runtime_string_to_int_parses_numbers)
{
    t_runtime_string text;
    t_runtime_int value;

    if (test_expect_success(runtime_string_init(&text, 0), "runtime_string_init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(runtime_string_assign(&text, "   -256  "), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_int_set(&value, 99);
    if (test_expect_success(runtime_string_to_int(&text, &value), "runtime_string_to_int should parse trimmed numbers") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, -256, "runtime_string_to_int should store parsed integers") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_int_set(&value, 73);
    if (test_expect_success(runtime_string_assign(&text, "12a"), "runtime_string_assign should copy text") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    if (runtime_string_to_int(&text, &value) != FT_FAILURE)
    {
        runtime_string_dispose(&text);
        pf_printf("Assertion failed: runtime_string_to_int should reject invalid numbers\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(value.value, 73, "runtime_string_to_int should leave destination unchanged on failure") != FT_SUCCESS)
    {
        runtime_string_dispose(&text);
        return (FT_FAILURE);
    }
    runtime_string_dispose(&text);
    return (FT_SUCCESS);
}

static int test_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    (void)context;
    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_pipeline_executes_stage)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_set_languages(&context, TRANSPILE_LANGUAGE_CBL_C, TRANSPILE_LANGUAGE_COBOL);
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "unit-stage", test_stage_callback, &counter), "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_execute(&pipeline, &context), "pipeline should execute successfully") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    if (test_expect_int_equal(counter, 1, "pipeline should execute the stage once") != FT_SUCCESS)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    int *counter;

    counter = static_cast<int *>(user_data);
    if (!counter)
        return (FT_FAILURE);
    *counter += 1;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

static int early_failing_stage_callback(t_transpiler_context *context, void *user_data)
{
    (void)user_data;
    transpiler_context_record_error(context, FT_FAILURE);
    return (FT_FAILURE);
}

FT_TEST(test_transpiler_pipeline_reports_failure)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "failing-stage", failing_stage_callback, &counter), "stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline execute should fail when a stage fails\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(counter, 1, "failing stage should still run once") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(pipeline.last_error, FT_FAILURE, "pipeline should retain last error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.diagnostics.count > 0, 1, "diagnostics should record pipeline failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(context.last_error_code, FT_FAILURE, "context should track last error code") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    return (FT_SUCCESS);
}

FT_TEST(test_transpiler_pipeline_stops_after_failure)
{
    t_transpiler_pipeline pipeline;
    t_transpiler_context context;
    int counter;

    counter = 0;
    if (test_expect_success(transpiler_pipeline_init(&pipeline), "pipeline init should succeed") != FT_SUCCESS)
        return (FT_FAILURE);
    if (test_expect_success(transpiler_context_init(&context), "context init should succeed") != FT_SUCCESS)
    {
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "early-failure", early_failing_stage_callback, NULL), "failing stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (test_expect_success(transpiler_pipeline_add_stage(&pipeline, "later-stage", test_stage_callback, &counter), "second stage registration should succeed") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    if (transpiler_pipeline_execute(&pipeline, &context) != FT_FAILURE)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        pf_printf("Assertion failed: pipeline should stop execution when an early stage fails\n");
        return (FT_FAILURE);
    }
    if (test_expect_int_equal(counter, 0, "pipeline should not run later stages after a failure") != FT_SUCCESS)
    {
        transpiler_context_dispose(&context);
        transpiler_pipeline_dispose(&pipeline);
        return (FT_FAILURE);
    }
    transpiler_context_dispose(&context);
    transpiler_pipeline_dispose(&pipeline);
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_builds_example_c_file)
{
    const char *source_path;
    const char *binary_path;
    const char *output_path;
    const char *source_code;
    char command[256];
    char output_buffer[128];
    int command_length;

    source_path = "test_example_compiler.c";
    binary_path = "test_example_compiler.bin";
    output_path = "test_example_compiler.txt";
    source_code = "#include <stdio.h>\n"
        "int main(void)\n"
        "{\n"
        "    printf(\"example-ok\\n\");\n"
        "    return 0;\n"
        "}\n";
    if (test_write_text_file(source_path, source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s", source_path, binary_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should build sample source\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "./%s > %s", binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiled program should run successfully\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (ft_strncmp(output_buffer, "example-ok\n", 12) != 0)
    {
        pf_printf("Assertion failed: compiled program should emit expected output\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(source_path, binary_path, output_path);
    return (FT_SUCCESS);
}

FT_TEST(test_compiler_rejects_invalid_c_file)
{
    const char *source_path;
    const char *binary_path;
    const char *output_path;
    const char *source_code;
    const char *expected_fragment;
    char command[256];
    char output_buffer[512];
    int command_length;

    source_path = "test_example_invalid_compiler.c";
    binary_path = "test_example_invalid_compiler.bin";
    output_path = "test_example_invalid_compiler.log";
    source_code = "int main(void)\n"
        "{\n"
        "    return 0\n";
    if (test_write_text_file(source_path, source_code) != FT_SUCCESS)
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    command_length = pf_snprintf(command, sizeof(command), "cc %s -o %s 2> %s", source_path, binary_path, output_path);
    if (command_length < 0 || static_cast<size_t>(command_length) >= sizeof(command))
    {
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_run_command_expect_failure(command) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler should reject invalid source\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    if (test_read_text_file(output_path, output_buffer, sizeof(output_buffer)) != FT_SUCCESS)
    {
        pf_printf("Assertion failed: compiler diagnostics should be captured\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    expected_fragment = "error: expected";
    if (!ft_strnstr(output_buffer, expected_fragment, ft_strlen(output_buffer)))
    {
        pf_printf("Assertion failed: compiler diagnostics should mention syntax error\n");
        test_cleanup_example_artifacts(source_path, binary_path, output_path);
        return (FT_FAILURE);
    }
    test_cleanup_example_artifacts(source_path, binary_path, output_path);
    return (FT_SUCCESS);
}

static int run_test_case(const t_test_case *test)
{
    int status;

    pf_printf("Running %s...\n", test->name);
    status = test->execute();
    if (status != FT_SUCCESS)
        pf_printf("FAILED %s\n", test->name);
    else
        pf_printf("PASSED %s\n", test->name);
    return (status);
}

static int run_all_tests(void)
{
    static const t_test_case tests[] = {
        {"ast_node_add_child_preserves_order", test_ast_node_add_child_preserves_order},
        {"ast_node_set_token_copies_lexeme", test_ast_node_set_token_copies_lexeme},
        {"lexer_keyword_lookup_identifies_keywords", test_lexer_keyword_lookup_identifies_keywords},
        {"lexer_keyword_lookup_defaults_to_identifier", test_lexer_keyword_lookup_defaults_to_identifier},
        {"lexer_trivia_detects_whitespace", test_lexer_trivia_detects_whitespace},
        {"lexer_trivia_detects_comments", test_lexer_trivia_detects_comments},
        {"lexer_tokenizes_sample_program", test_lexer_tokenizes_sample_program},
        {"lexer_reports_unterminated_string", test_lexer_reports_unterminated_string},
        {"runtime_int_add_and_subtract", test_runtime_int_add_and_subtract},
        {"runtime_int_add_detects_overflow", test_runtime_int_add_detects_overflow},
        {"runtime_int_to_string", test_runtime_int_to_string},
        {"runtime_int_to_string_rejects_small_buffer", test_runtime_int_to_string_rejects_small_buffer},
        {"runtime_int_from_string", test_runtime_int_from_string},
        {"runtime_char_transforms", test_runtime_char_transforms},
        {"runtime_char_from_string_rejects_empty_input", test_runtime_char_from_string_rejects_empty_input},
        {"runtime_char_to_string_and_compare", test_runtime_char_to_string_and_compare},
        {"runtime_string_assign_and_trim", test_runtime_string_assign_and_trim},
        {"runtime_string_compare_orders_text", test_runtime_string_compare_orders_text},
        {"runtime_string_to_int_parses_numbers", test_runtime_string_to_int_parses_numbers},
        {"runtime_file_write_and_read_text", test_runtime_file_write_and_read_text},
        {"runtime_file_open_read_missing_path", test_runtime_file_open_read_missing_path},
        {"runtime_file_reopen_transitions_modes", test_runtime_file_reopen_transitions_modes},
        {"runtime_file_requires_open_descriptor", test_runtime_file_requires_open_descriptor},
        {"transpiler_pipeline_executes_stage", test_transpiler_pipeline_executes_stage},
        {"transpiler_pipeline_reports_failure", test_transpiler_pipeline_reports_failure},
        {"transpiler_pipeline_stops_after_failure", test_transpiler_pipeline_stops_after_failure},
        {"compiler_builds_example_c_file", test_compiler_builds_example_c_file},
        {"compiler_rejects_invalid_c_file", test_compiler_rejects_invalid_c_file}
    };
    size_t index;
    int status;

    index = 0;
    status = FT_SUCCESS;
    while (index < sizeof(tests) / sizeof(tests[0]))
    {
        if (run_test_case(&tests[index]) != FT_SUCCESS)
            status = FT_FAILURE;
        index += 1;
    }
    return (status);
}

int main(void)
{
    if (run_all_tests() != FT_SUCCESS)
        return (1);
    return (0);
}
