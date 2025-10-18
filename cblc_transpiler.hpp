#ifndef TRANSPILE_MAIN_HPP
#define TRANSPILE_MAIN_HPP

#include <cstddef>

#include "libft/Libft/libft.hpp"

// ===============================
// Core runtime support utilities
// ===============================
typedef struct s_runtime_file
{
    int descriptor;
}   t_runtime_file;

void runtime_file_init(t_runtime_file *file);
int runtime_file_open_read(t_runtime_file *file, const char *path);
int runtime_file_open_write(t_runtime_file *file, const char *path);
int runtime_file_read(t_runtime_file *file, char *buffer, size_t buffer_size, size_t *bytes_read);
int runtime_file_write(t_runtime_file *file, const char *buffer, size_t length);
int runtime_file_close(t_runtime_file *file);

typedef struct s_runtime_record
{
    char *data;
    size_t length;
    size_t capacity;
}   t_runtime_record;

typedef struct s_runtime_record_key
{
    size_t offset;
    size_t length;
    int ascending;
}   t_runtime_record_key;

int runtime_record_init(t_runtime_record *record, size_t initial_capacity);
void runtime_record_dispose(t_runtime_record *record);
int runtime_record_set_length(t_runtime_record *record, size_t length);
int runtime_record_fill(t_runtime_record *record, char value);
int runtime_record_copy_from_buffer(t_runtime_record *record, const char *buffer, size_t length);
int runtime_record_copy_to_buffer(const t_runtime_record *record, char *buffer, size_t buffer_size, size_t *written);
int runtime_record_compare_keys(const t_runtime_record *left, const t_runtime_record *right,
    const t_runtime_record_key *keys, size_t key_count, int *result);
int runtime_record_sort(t_runtime_record *records, size_t record_count, const t_runtime_record_key *keys,
    size_t key_count);
int runtime_record_search_all(const t_runtime_record *records, size_t record_count, const t_runtime_record *target,
    const t_runtime_record_key *keys, size_t key_count, size_t *found_index, int *found);

typedef struct s_runtime_int
{
    int value;
}   t_runtime_int;

typedef struct s_runtime_long
{
    long value;
}   t_runtime_long;

typedef struct s_runtime_long_long
{
    long long value;
}   t_runtime_long_long;

typedef struct s_runtime_char
{
    char value;
}   t_runtime_char;

void runtime_int_set(t_runtime_int *destination, int value);
int runtime_int_from_string(t_runtime_int *destination, const char *text);
int runtime_int_add(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_subtract(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_multiply(t_runtime_int left, t_runtime_int right, t_runtime_int *result);
int runtime_int_divide(t_runtime_int dividend, t_runtime_int divisor, t_runtime_int *result);
int runtime_int_unary_plus(t_runtime_int value, t_runtime_int *result);
int runtime_int_unary_minus(t_runtime_int value, t_runtime_int *result);
int runtime_int_absolute(t_runtime_int value, t_runtime_int *result);
void runtime_long_set(t_runtime_long *destination, long value);
int runtime_long_add(t_runtime_long left, t_runtime_long right, t_runtime_long *result);
int runtime_long_subtract(t_runtime_long left, t_runtime_long right, t_runtime_long *result);
int runtime_long_multiply(t_runtime_long left, t_runtime_long right, t_runtime_long *result);
int runtime_long_divide(t_runtime_long dividend, t_runtime_long divisor, t_runtime_long *result);
int runtime_long_unary_plus(t_runtime_long value, t_runtime_long *result);
int runtime_long_unary_minus(t_runtime_long value, t_runtime_long *result);
int runtime_long_absolute(t_runtime_long value, t_runtime_long *result);
void runtime_long_long_set(t_runtime_long_long *destination, long long value);
int runtime_long_long_add(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result);
int runtime_long_long_subtract(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result);
int runtime_long_long_multiply(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result);
int runtime_long_long_divide(t_runtime_long_long dividend, t_runtime_long_long divisor,
    t_runtime_long_long *result);
int runtime_long_long_unary_plus(t_runtime_long_long value, t_runtime_long_long *result);
int runtime_long_long_unary_minus(t_runtime_long_long value, t_runtime_long_long *result);
int runtime_long_long_absolute(t_runtime_long_long value, t_runtime_long_long *result);
int runtime_int_compare(t_runtime_int left, t_runtime_int right);
int runtime_int_to_string(t_runtime_int value, char *buffer, size_t buffer_size);

void runtime_char_set(t_runtime_char *destination, char value);
int runtime_char_from_string(t_runtime_char *destination, const char *text);
void runtime_char_to_upper(t_runtime_char *value);
void runtime_char_to_lower(t_runtime_char *value);
int runtime_char_compare(t_runtime_char left, t_runtime_char right);
int runtime_char_to_string(t_runtime_char value, char *buffer, size_t buffer_size);

void runtime_demo(void);

typedef struct s_runtime_string
{
    char *data;
    size_t length;
    size_t capacity;
}   t_runtime_string;

int runtime_string_init(t_runtime_string *value, size_t initial_capacity);
void runtime_string_dispose(t_runtime_string *value);
int runtime_string_assign(t_runtime_string *value, const char *text);
int runtime_string_trim(t_runtime_string *value);
int runtime_string_compare(const t_runtime_string *left, const t_runtime_string *right);
int runtime_string_to_int(const t_runtime_string *value, t_runtime_int *destination);
int runtime_string_equals(const t_runtime_string *left, const t_runtime_string *right);
int runtime_string_concat(t_runtime_string *destination, const t_runtime_string *left,
    const t_runtime_string *right);
int runtime_string_blank(char *destination, size_t destination_length);
int runtime_string_copy_checked(char *destination, size_t destination_length, const char *source,
    size_t source_length, size_t *written_length, int *was_truncated);
int runtime_line_read_fixed(t_runtime_file *file, size_t record_length, t_runtime_string *destination,
    int *end_of_file);
int runtime_line_read_variable(t_runtime_file *file, char terminator, t_runtime_string *destination,
    int *end_of_file);
int runtime_line_write_fixed(t_runtime_file *file, const char *buffer, size_t length, size_t record_length);
int runtime_line_write_variable(t_runtime_file *file, const char *buffer, size_t length, char terminator);
int runtime_csv_parse_line(const char *line, t_runtime_string *fields, size_t field_capacity, size_t *field_count);
int runtime_csv_format_line(const t_runtime_string *fields, size_t field_count, t_runtime_string *destination);

int runtime_memory_copy_checked(void *destination, size_t destination_length, const void *source,
    size_t source_length);

#define RUNTIME_ENCODING_TABLE_SIZE 256

typedef struct s_runtime_encoding_table
{
    unsigned char to_ascii[RUNTIME_ENCODING_TABLE_SIZE];
    unsigned char from_ascii[RUNTIME_ENCODING_TABLE_SIZE];
}   t_runtime_encoding_table;

int runtime_encoding_get_active(t_runtime_encoding_table *table);
int runtime_encoding_set_active(const t_runtime_encoding_table *table);
int runtime_encoding_reset(void);
int runtime_encoding_transcode_to_ascii(const unsigned char *source, size_t source_length,
    unsigned char *destination, size_t destination_length, size_t *written_length);
int runtime_encoding_transcode_to_ebcdic(const unsigned char *source, size_t source_length,
    unsigned char *destination, size_t destination_length, size_t *written_length);

typedef int (*t_runtime_collation_compare_callback)(const char *left, size_t left_length,
    const char *right, size_t right_length, int *result, void *user_data);

typedef struct s_runtime_collation_bridge
{
    t_runtime_collation_compare_callback compare;
    void *user_data;
}   t_runtime_collation_bridge;

void runtime_collation_get_bridge(t_runtime_collation_bridge *bridge);
int runtime_collation_set_bridge(const t_runtime_collation_bridge *bridge);
void runtime_collation_clear_bridge(void);
int runtime_collation_compare(const char *left, size_t left_length, const char *right,
    size_t right_length, int *result);

// ===============================
// Context management and helpers
// ===============================
#define TRANSPILE_DIAGNOSTIC_MESSAGE_MAX 256

typedef enum e_transpiler_severity
{
    TRANSPILE_SEVERITY_INFO = 0,
    TRANSPILE_SEVERITY_WARNING,
    TRANSPILE_SEVERITY_ERROR
}   t_transpiler_severity;

typedef struct s_transpiler_diagnostic
{
    t_transpiler_severity severity;
    int code;
    char message[TRANSPILE_DIAGNOSTIC_MESSAGE_MAX];
}   t_transpiler_diagnostic;

typedef struct s_transpiler_diagnostic_list
{
    t_transpiler_diagnostic *items;
    size_t count;
    size_t capacity;
}   t_transpiler_diagnostic_list;

int transpiler_diagnostics_init(t_transpiler_diagnostic_list *list);
void transpiler_diagnostics_dispose(t_transpiler_diagnostic_list *list);
int transpiler_diagnostics_push(t_transpiler_diagnostic_list *list, t_transpiler_severity severity, int code,
    const char *message);

typedef enum e_transpiler_warning_group
{
    TRANSPILE_WARNING_GROUP_CONVERSION = 0,
    TRANSPILE_WARNING_GROUP_OVERFLOW,
    TRANSPILE_WARNING_GROUP_STRING_TRUNCATION,
    TRANSPILE_WARNING_GROUP_SHADOW,
    TRANSPILE_WARNING_GROUP_UNUSED
}   t_transpiler_warning_group;

typedef struct s_transpiler_warning_settings
{
    int conversion;
    int overflow;
    int string_truncation;
    int shadow;
    int unused;
}   t_transpiler_warning_settings;

typedef enum e_transpiler_language
{
    TRANSPILE_LANGUAGE_NONE = 0,
    TRANSPILE_LANGUAGE_CBL_C,
    TRANSPILE_LANGUAGE_COBOL
}   t_transpiler_language;

typedef enum e_transpiler_format_mode
{
    TRANSPILE_FORMAT_DEFAULT = 0,
    TRANSPILE_FORMAT_MINIMAL,
    TRANSPILE_FORMAT_PRETTY
}   t_transpiler_format_mode;

typedef enum e_transpiler_diagnostic_level
{
    TRANSPILE_DIAGNOSTIC_SILENT = 0,
    TRANSPILE_DIAGNOSTIC_NORMAL,
    TRANSPILE_DIAGNOSTIC_VERBOSE
}   t_transpiler_diagnostic_level;

#define TRANSPILE_FUNCTION_NAME_MAX 64
#define TRANSPILE_IDENTIFIER_MAX 64
#define TRANSPILE_MODULE_NAME_MAX 64
#define TRANSPILE_FILE_PATH_MAX 260

typedef enum e_transpiler_function_return_mode
{
    TRANSPILE_FUNCTION_RETURN_VOID = 0,
    TRANSPILE_FUNCTION_RETURN_VALUE
}   t_transpiler_function_return_mode;

typedef enum e_transpiler_symbol_visibility
{
    TRANSPILE_SYMBOL_PRIVATE = 0,
    TRANSPILE_SYMBOL_PUBLIC
}   t_transpiler_symbol_visibility;

typedef struct s_transpiler_function_signature
{
    char name[TRANSPILE_FUNCTION_NAME_MAX];
    char module[TRANSPILE_MODULE_NAME_MAX];
    t_transpiler_function_return_mode return_mode;
    t_transpiler_symbol_visibility visibility;
}   t_transpiler_function_signature;

#define TRANSPILE_ERROR_FUNCTION_RETURNS_VALUE 1001
#define TRANSPILE_ERROR_ENTRYPOINT_INVALID_NAME 1002
#define TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH 1003
#define TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE 1004
#define TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME 1005
#define TRANSPILE_ERROR_FILE_DUPLICATE_NAME 1006
#define TRANSPILE_ERROR_FILE_UNKNOWN 1007
#define TRANSPILE_ERROR_MODULE_DUPLICATE_NAME 1008
#define TRANSPILE_ERROR_MODULE_UNKNOWN 1009
#define TRANSPILE_ERROR_MODULE_IMPORT_UNKNOWN 1010
#define TRANSPILE_ERROR_MODULE_IMPORT_CYCLE 1011
#define TRANSPILE_ERROR_FUNCTION_EXPORT_CONFLICT 1012
#define TRANSPILE_ERROR_DATA_ITEM_PARAMETER_TRUNCATION 1013
#define TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS 1014
#define TRANSPILE_ERROR_COPYBOOK_DUPLICATE 1015

typedef enum e_transpiler_file_role
{
    TRANSPILE_FILE_ROLE_INPUT = 0,
    TRANSPILE_FILE_ROLE_OUTPUT,
    TRANSPILE_FILE_ROLE_DATA
}   t_transpiler_file_role;

typedef struct s_transpiler_file_declaration
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_file_role role;
    char path[TRANSPILE_FILE_PATH_MAX];
    size_t explicit_record_length;
    size_t inferred_record_length;
}   t_transpiler_file_declaration;

typedef struct s_transpiler_module_import
{
    char path[TRANSPILE_FILE_PATH_MAX];
    size_t resolved_index;
}   t_transpiler_module_import;

typedef struct s_transpiler_module
{
    char name[TRANSPILE_MODULE_NAME_MAX];
    char path[TRANSPILE_FILE_PATH_MAX];
    t_transpiler_module_import *imports;
    size_t import_count;
    size_t import_capacity;
    size_t initialization_rank;
}   t_transpiler_module;

typedef struct s_transpiler_entrypoint
{
    int present;
    char name[TRANSPILE_FUNCTION_NAME_MAX];
    int has_argument_vectors;
    int needs_argument_copy;
    char argc_identifier[TRANSPILE_IDENTIFIER_MAX];
    char argv_identifier[TRANSPILE_IDENTIFIER_MAX];
}   t_transpiler_entrypoint;

typedef enum e_transpiler_data_item_kind
{
    TRANSPILE_DATA_ITEM_UNKNOWN = 0,
    TRANSPILE_DATA_ITEM_ALPHANUMERIC,
    TRANSPILE_DATA_ITEM_NUMERIC,
    TRANSPILE_DATA_ITEM_FLOATING
}   t_transpiler_data_item_kind;

typedef struct s_transpiler_data_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_data_item_kind kind;
    size_t declared_length;
    int has_caller_length;
    int is_read_only;
}   t_transpiler_data_item;

typedef struct s_transpiler_copybook_item
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_data_item_kind kind;
    size_t declared_length;
    int is_read_only;
}   t_transpiler_copybook_item;

typedef struct s_transpiler_copybook
{
    char name[TRANSPILE_IDENTIFIER_MAX];
    t_transpiler_copybook_item *items;
    size_t item_count;
}   t_transpiler_copybook;

typedef struct s_transpiler_source_span
{
    char path[TRANSPILE_FILE_PATH_MAX];
    size_t start_line;
    size_t start_column;
    size_t end_line;
    size_t end_column;
}   t_transpiler_source_span;

typedef struct s_transpiler_source_map_entry
{
    t_transpiler_source_span cblc_span;
    t_transpiler_source_span cobol_span;
}   t_transpiler_source_map_entry;

typedef struct s_transpiler_context
{
    t_transpiler_language source_language;
    t_transpiler_language target_language;
    const char *source_path;
    const char *target_path;
    const char **source_paths;
    size_t source_count;
    size_t source_capacity;
    const char **target_paths;
    size_t target_count;
    size_t target_capacity;
    const char *output_directory;
    t_transpiler_format_mode format_mode;
    t_transpiler_diagnostic_level diagnostic_level;
    int warnings_as_errors;
    t_transpiler_warning_settings warning_settings;
    t_transpiler_diagnostic_list diagnostics;
    int last_error_code;
    t_transpiler_function_signature *functions;
    size_t function_count;
    size_t function_capacity;
    t_transpiler_file_declaration *files;
    size_t file_count;
    size_t file_capacity;
    t_transpiler_module *modules;
    size_t module_count;
    size_t module_capacity;
    size_t *module_order;
    size_t module_order_count;
    size_t module_order_capacity;
    t_transpiler_entrypoint entrypoint;
    t_transpiler_data_item *data_items;
    size_t data_item_count;
    size_t data_item_capacity;
    t_transpiler_source_map_entry *source_maps;
    size_t source_map_count;
    size_t source_map_capacity;
    t_transpiler_copybook *copybooks;
    size_t copybook_count;
    size_t copybook_capacity;
}   t_transpiler_context;

int transpiler_context_init(t_transpiler_context *context);
void transpiler_context_dispose(t_transpiler_context *context);
void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source,
    t_transpiler_language target);
int transpiler_context_set_io_paths(t_transpiler_context *context, const char **source_paths, size_t source_count,
    const char **target_paths, size_t target_count);
void transpiler_context_set_output_directory(t_transpiler_context *context, const char *output_directory);
void transpiler_context_set_format_mode(t_transpiler_context *context, t_transpiler_format_mode mode);
void transpiler_context_set_diagnostic_level(t_transpiler_context *context, t_transpiler_diagnostic_level level);
void transpiler_context_set_warnings_as_errors(t_transpiler_context *context, int warnings_as_errors);
void transpiler_context_set_warning_settings(t_transpiler_context *context, const t_transpiler_warning_settings *settings);
int transpiler_context_warning_group_enabled(const t_transpiler_context *context, t_transpiler_warning_group group);
void transpiler_context_reset_unit_state(t_transpiler_context *context);
void transpiler_context_record_error(t_transpiler_context *context, int error_code);
int transpiler_context_has_errors(const t_transpiler_context *context);
int transpiler_context_register_module(t_transpiler_context *context, const char *name, const char *path);
const t_transpiler_module *transpiler_context_get_modules(const t_transpiler_context *context, size_t *count);
int transpiler_context_register_module_import(t_transpiler_context *context, const char *module_name,
    const char *import_path);
int transpiler_context_scan_imports_for_module(t_transpiler_context *context, const char *module_name,
    const char *source_text);
int transpiler_context_compute_module_initialization_order(t_transpiler_context *context);
const size_t *transpiler_context_get_module_initialization_order(const t_transpiler_context *context, size_t *count);
int transpiler_context_register_function(t_transpiler_context *context, const char *module_name, const char *name,
    t_transpiler_function_return_mode return_mode, t_transpiler_symbol_visibility visibility);
const t_transpiler_function_signature *transpiler_context_find_function(const t_transpiler_context *context,
    const char *module_name, const char *name);
const t_transpiler_function_signature *transpiler_context_resolve_function_access(t_transpiler_context *context,
    const char *requesting_module, const char *module_name, const char *name);
int transpiler_context_register_entrypoint(t_transpiler_context *context, const char *module_name, const char *name,
    t_transpiler_function_return_mode return_mode, const char *argc_identifier, const char *argv_identifier);
const t_transpiler_entrypoint *transpiler_context_get_entrypoint(const t_transpiler_context *context);
int transpiler_context_register_file(t_transpiler_context *context, const char *name, t_transpiler_file_role role,
    const char *path, size_t explicit_record_length);
int transpiler_context_record_file_length_hint(t_transpiler_context *context, const char *name, size_t record_length);
const t_transpiler_file_declaration *transpiler_context_get_files(const t_transpiler_context *context, size_t *count);
int transpiler_context_register_data_item(t_transpiler_context *context, const char *name,
    t_transpiler_data_item_kind kind, size_t declared_length, int is_read_only);
const t_transpiler_data_item *transpiler_context_find_data_item(const t_transpiler_context *context, const char *name);
int transpiler_context_register_copybook(t_transpiler_context *context, const char *name,
    const t_transpiler_copybook_item *items, size_t item_count);
const t_transpiler_copybook *transpiler_context_find_copybook(const t_transpiler_context *context, const char *name);
const t_transpiler_data_item *transpiler_context_get_data_items(const t_transpiler_context *context, size_t *count);
int transpiler_context_record_source_map_entry(t_transpiler_context *context,
    const t_transpiler_source_span *cblc_span, const t_transpiler_source_span *cobol_span);
const t_transpiler_source_map_entry *transpiler_context_get_source_maps(const t_transpiler_context *context,
    size_t *count);
const t_transpiler_source_map_entry *transpiler_context_map_cblc_to_cobol(const t_transpiler_context *context,
    const char *path, size_t line, size_t column);
const t_transpiler_source_map_entry *transpiler_context_map_cobol_to_cblc(const t_transpiler_context *context,
    const char *path, size_t line, size_t column);

// ===============================
// Lexing, parsing, and AST nodes
// ===============================
typedef enum e_lexer_token_kind
{
    LEXER_TOKEN_UNKNOWN = 0,
    LEXER_TOKEN_END_OF_FILE,
    LEXER_TOKEN_IDENTIFIER,
    LEXER_TOKEN_NUMERIC_LITERAL,
    LEXER_TOKEN_STRING_LITERAL,
    LEXER_TOKEN_PERIOD,
    LEXER_TOKEN_COMMA,
    LEXER_TOKEN_COLON,
    LEXER_TOKEN_SEMICOLON,
    LEXER_TOKEN_ASSIGN,
    LEXER_TOKEN_LEFT_PAREN,
    LEXER_TOKEN_RIGHT_PAREN,
    LEXER_TOKEN_PLUS,
    LEXER_TOKEN_MINUS,
    LEXER_TOKEN_STAR,
    LEXER_TOKEN_SLASH,
    LEXER_TOKEN_KEYWORD_MOD,
    LEXER_TOKEN_KEYWORD_IDENTIFICATION,
    LEXER_TOKEN_KEYWORD_DIVISION,
    LEXER_TOKEN_KEYWORD_PROGRAM_ID,
    LEXER_TOKEN_KEYWORD_ENVIRONMENT,
    LEXER_TOKEN_KEYWORD_DATA,
    LEXER_TOKEN_KEYWORD_PROCEDURE,
    LEXER_TOKEN_KEYWORD_WORKING_STORAGE,
    LEXER_TOKEN_KEYWORD_SECTION,
    LEXER_TOKEN_KEYWORD_FILE,
    LEXER_TOKEN_KEYWORD_SELECT,
    LEXER_TOKEN_KEYWORD_ASSIGN,
    LEXER_TOKEN_KEYWORD_TO,
    LEXER_TOKEN_KEYWORD_FD,
    LEXER_TOKEN_KEYWORD_PIC,
    LEXER_TOKEN_KEYWORD_VALUE,
    LEXER_TOKEN_KEYWORD_IF,
    LEXER_TOKEN_KEYWORD_ELSE,
    LEXER_TOKEN_KEYWORD_PERFORM,
    LEXER_TOKEN_KEYWORD_UNTIL,
    LEXER_TOKEN_KEYWORD_MOVE,
    LEXER_TOKEN_KEYWORD_COMPUTE,
    LEXER_TOKEN_KEYWORD_COPY,
    LEXER_TOKEN_KEYWORD_ABS,
    LEXER_TOKEN_KEYWORD_OPEN,
    LEXER_TOKEN_KEYWORD_CLOSE,
    LEXER_TOKEN_KEYWORD_READ,
    LEXER_TOKEN_KEYWORD_WRITE,
    LEXER_TOKEN_KEYWORD_DISPLAY,
    LEXER_TOKEN_KEYWORD_VARYING,
    LEXER_TOKEN_KEYWORD_FROM,
    LEXER_TOKEN_KEYWORD_BY,
    LEXER_TOKEN_KEYWORD_END_IF,
    LEXER_TOKEN_KEYWORD_END_PERFORM,
    LEXER_TOKEN_KEYWORD_NOT,
    LEXER_TOKEN_KEYWORD_TRUE,
    LEXER_TOKEN_KEYWORD_FALSE,
    LEXER_TOKEN_KEYWORD_STOP,
    LEXER_TOKEN_KEYWORD_RUN,
    LEXER_TOKEN_EQUALS,
    LEXER_TOKEN_NOT_EQUALS,
    LEXER_TOKEN_LESS_THAN,
    LEXER_TOKEN_LESS_OR_EQUAL,
    LEXER_TOKEN_GREATER_THAN,
    LEXER_TOKEN_GREATER_OR_EQUAL
}   t_lexer_token_kind;

typedef enum e_lexer_trivia_kind
{
    LEXER_TRIVIA_NONE = 0,
    LEXER_TRIVIA_WHITESPACE,
    LEXER_TRIVIA_COMMENT
}   t_lexer_trivia_kind;

typedef struct s_lexer_token
{
    t_lexer_token_kind kind;
    const char *lexeme;
    size_t length;
    size_t line;
    size_t column;
}   t_lexer_token;

t_lexer_token_kind lexer_token_lookup_keyword(const char *text, size_t length);
t_lexer_trivia_kind lexer_classify_trivia(const char *text, size_t length);

typedef struct s_lexer
{
    const char *text;
    size_t length;
    size_t offset;
    size_t line;
    size_t column;
}   t_lexer;

void lexer_init(t_lexer *lexer, const char *text);
int lexer_next_token(t_lexer *lexer, t_lexer_token *token);

typedef enum e_ast_node_kind
{
    AST_NODE_UNKNOWN = 0,
    AST_NODE_PROGRAM,
    AST_NODE_IDENTIFICATION_DIVISION,
    AST_NODE_ENVIRONMENT_DIVISION,
    AST_NODE_DATA_DIVISION,
    AST_NODE_PROCEDURE_DIVISION,
    AST_NODE_PROGRAM_ID,
    AST_NODE_WORKING_STORAGE_SECTION,
    AST_NODE_DATA_ITEM,
    AST_NODE_COPYBOOK_INCLUDE,
    AST_NODE_PICTURE_CLAUSE,
    AST_NODE_VALUE_CLAUSE,
    AST_NODE_STATEMENT_SEQUENCE,
    AST_NODE_PARAGRAPH,
    AST_NODE_MOVE_STATEMENT,
    AST_NODE_ASSIGNMENT_STATEMENT,
    AST_NODE_COMPUTE_STATEMENT,
    AST_NODE_IF_STATEMENT,
    AST_NODE_PERFORM_UNTIL_STATEMENT,
    AST_NODE_PERFORM_VARYING_STATEMENT,
    AST_NODE_OPEN_STATEMENT,
    AST_NODE_CLOSE_STATEMENT,
    AST_NODE_READ_STATEMENT,
    AST_NODE_WRITE_STATEMENT,
    AST_NODE_DISPLAY_STATEMENT,
    AST_NODE_STOP_STATEMENT,
    AST_NODE_CONDITION,
    AST_NODE_ARITHMETIC_EXPRESSION,
    AST_NODE_UNARY_EXPRESSION,
    AST_NODE_ARITHMETIC_OPERATOR,
    AST_NODE_COMPARISON_OPERATOR,
    AST_NODE_IDENTIFIER,
    AST_NODE_LITERAL
}   t_ast_node_kind;

typedef struct s_ast_node
{
    t_ast_node_kind kind;
    t_lexer_token token;
    char *lexeme_storage;
    struct s_ast_node **children;
    size_t child_count;
    size_t child_capacity;
}   t_ast_node;

int ast_node_init(t_ast_node *node, t_ast_node_kind kind);
void ast_node_dispose(t_ast_node *node);

t_ast_node *ast_node_create(t_ast_node_kind kind);
void ast_node_destroy(t_ast_node *node);

int ast_node_set_token(t_ast_node *node, const t_lexer_token *token);
int ast_node_add_child(t_ast_node *parent, t_ast_node *child);

size_t ast_node_child_count(const t_ast_node *node);
t_ast_node *ast_node_get_child(const t_ast_node *node, size_t index);

typedef struct s_parser
{
    t_lexer lexer;
    t_lexer_token current;
    int has_current;
    int last_error;
    size_t error_count;
}   t_parser;

void parser_init(t_parser *parser, const char *text);
void parser_dispose(t_parser *parser);
int parser_parse_program(t_parser *parser, t_ast_node **out_program);

// =============================================
// Pipeline orchestration and semantic validation
// =============================================
#define TRANSPILE_STAGE_NAME_MAX 64

typedef int (*t_transpiler_stage_callback)(t_transpiler_context *context, void *user_data);

typedef struct s_transpiler_stage
{
    t_transpiler_stage_callback callback;
    void *user_data;
    char name[TRANSPILE_STAGE_NAME_MAX];
}   t_transpiler_stage;

typedef struct s_transpiler_pipeline
{
    t_transpiler_stage *stages;
    size_t stage_count;
    size_t stage_capacity;
    int last_error;
}   t_transpiler_pipeline;

int transpiler_pipeline_init(t_transpiler_pipeline *pipeline);
void transpiler_pipeline_dispose(t_transpiler_pipeline *pipeline);
int transpiler_pipeline_add_stage(t_transpiler_pipeline *pipeline, const char *name,
    t_transpiler_stage_callback callback, void *user_data);
int transpiler_pipeline_execute(t_transpiler_pipeline *pipeline, t_transpiler_context *context);
void transpiler_pipeline_reset(t_transpiler_pipeline *pipeline);

#define TRANSPILE_ERROR_SEMANTIC_UNDECLARED_IDENTIFIER 2001
#define TRANSPILE_ERROR_SEMANTIC_DUPLICATE_DATA_ITEM 2002
#define TRANSPILE_ERROR_SEMANTIC_INVALID_MOVE 2003
#define TRANSPILE_ERROR_SEMANTIC_TYPE_MISMATCH 2004
#define TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION 2005
#define TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET 2006
#define TRANSPILE_ERROR_SEMANTIC_UNKNOWN_COPYBOOK 2007
#define TRANSPILE_ERROR_SEMANTIC_INVALID_CONDITION 2008
#define TRANSPILE_ERROR_SEMANTIC_INVALID_EXPRESSION 2009
#define TRANSPILE_ERROR_SEMANTIC_INVALID_ASSIGNMENT 2010
#define TRANSPILE_ERROR_SEMANTIC_NUMERIC_OVERFLOW 2011
#define TRANSPILE_ERROR_SEMANTIC_FLOATING_TRUNCATION 2012
#define TRANSPILE_ERROR_SEMANTIC_DECIMAL_SCALE_MISMATCH 2013

#define TRANSPILE_WARNING_SEMANTIC_FLOAT_TO_DOUBLE 3001
#define TRANSPILE_WARNING_SEMANTIC_DOUBLE_TO_FLOAT 3002
#define TRANSPILE_WARNING_SEMANTIC_INTEGRAL_TO_FLOATING 3003
#define TRANSPILE_WARNING_SEMANTIC_INTEGRAL_TO_ALPHANUMERIC 3004
#define TRANSPILE_WARNING_SEMANTIC_BOOLEAN_TO_NUMERIC 3005
#define TRANSPILE_WARNING_SEMANTIC_NUMERIC_TO_BOOLEAN 3006
#define TRANSPILE_WARNING_SEMANTIC_BOOLEAN_TO_ALPHANUMERIC 3007
#define TRANSPILE_WARNING_SEMANTIC_ALPHANUMERIC_TO_BOOLEAN 3008
#define TRANSPILE_WARNING_SEMANTIC_UNREACHABLE_CODE 3009
#define TRANSPILE_WARNING_SEMANTIC_UNUSED_DATA_ITEM 3010
#define TRANSPILE_WARNING_SEMANTIC_WRITE_ONLY_DATA_ITEM 3011
#define TRANSPILE_WARNING_SEMANTIC_READ_WITHOUT_WRITE 3012

int transpiler_semantics_analyze_program(t_transpiler_context *context, const t_ast_node *program);

int transpiler_validate_generated_cblc(const char *text);
int transpiler_validate_generated_cobol(const char *text);

// ==================================
// COBOL and CBLC transformation APIs
// ==================================
typedef enum e_cblc_data_kind
{
    CBLC_DATA_KIND_CHAR = 0,
    CBLC_DATA_KIND_INT,
    CBLC_DATA_KIND_STRING
}   t_cblc_data_kind;

#define TRANSPILE_STATEMENT_TEXT_MAX 256

typedef struct s_cblc_data_item
{
    char source_name[TRANSPILE_IDENTIFIER_MAX];
    char cobol_name[TRANSPILE_IDENTIFIER_MAX];
    size_t length;
    t_cblc_data_kind kind;
}   t_cblc_data_item;

typedef enum e_cblc_statement_type
{
    CBLC_STATEMENT_ASSIGNMENT,
    CBLC_STATEMENT_DISPLAY,
    CBLC_STATEMENT_COMPUTE
}   t_cblc_statement_type;

typedef struct s_cblc_statement
{
    t_cblc_statement_type type;
    char target[TRANSPILE_IDENTIFIER_MAX];
    char source[TRANSPILE_STATEMENT_TEXT_MAX];
    int is_literal;
}   t_cblc_statement;

typedef struct s_cblc_translation_unit
{
    t_cblc_data_item *data_items;
    size_t data_count;
    size_t data_capacity;
    t_cblc_statement *statements;
    size_t statement_count;
    size_t statement_capacity;
    char program_name[TRANSPILE_IDENTIFIER_MAX];
    int saw_return;
}   t_cblc_translation_unit;

void cblc_translation_unit_init(t_cblc_translation_unit *unit);
void cblc_translation_unit_dispose(t_cblc_translation_unit *unit);
int cblc_parse_translation_unit(const char *text, t_cblc_translation_unit *unit);
int cblc_generate_cobol(const t_cblc_translation_unit *unit, char **out_text);

int transpiler_cobol_program_to_cblc(t_transpiler_context *context, const t_ast_node *program,
    char **out_text);

typedef enum e_transpiler_cobol_elementary_kind
{
    TRANSPILE_COBOL_ELEMENTARY_ALPHABETIC = 0,
    TRANSPILE_COBOL_ELEMENTARY_ALPHANUMERIC,
    TRANSPILE_COBOL_ELEMENTARY_BOOLEAN,
    TRANSPILE_COBOL_ELEMENTARY_NUMERIC_SIGNED,
    TRANSPILE_COBOL_ELEMENTARY_NUMERIC_UNSIGNED
}   t_transpiler_cobol_elementary_kind;

typedef enum e_transpiler_cobol_usage
{
    TRANSPILE_COBOL_USAGE_DISPLAY = 0,
    TRANSPILE_COBOL_USAGE_COMP_5
}   t_transpiler_cobol_usage;

typedef struct s_transpiler_cobol_elementary
{
    t_transpiler_cobol_elementary_kind kind;
    size_t length;
    size_t scale;
    t_transpiler_cobol_usage usage;
}   t_transpiler_cobol_elementary;

typedef struct s_transpiler_cobol_group_field
{
    const char *name;
    size_t level;
    t_transpiler_cobol_elementary element;
}   t_transpiler_cobol_group_field;

typedef struct s_transpiler_cobol_group
{
    const char *name;
    size_t level;
    const t_transpiler_cobol_group_field *fields;
    size_t field_count;
}   t_transpiler_cobol_group;

int transpiler_cobol_describe_c_int(size_t digits, int is_signed, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_long(int is_signed, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_long_long(int is_signed, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_float(t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_double(t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_char_array(size_t length, t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_c_bool(t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_numeric(size_t digits, int is_signed, t_transpiler_cobol_usage usage,
    t_transpiler_cobol_elementary *out);
int transpiler_cobol_describe_fixed_point(size_t integral_digits, size_t fractional_digits, int is_signed,
    t_transpiler_cobol_usage usage, t_transpiler_cobol_elementary *out);
int transpiler_cobol_format_elementary(const char *name, size_t level,
    const t_transpiler_cobol_elementary *element, size_t indentation, char **out);
int transpiler_cobol_format_group(const t_transpiler_cobol_group *group,
    size_t indentation, char **out);

typedef enum e_transpiler_cobol_comparison_operator
{
    TRANSPILE_COBOL_COMPARISON_EQUALS = 0,
    TRANSPILE_COBOL_COMPARISON_NOT_EQUALS,
    TRANSPILE_COBOL_COMPARISON_LESS_THAN,
    TRANSPILE_COBOL_COMPARISON_LESS_OR_EQUAL,
    TRANSPILE_COBOL_COMPARISON_GREATER_THAN,
    TRANSPILE_COBOL_COMPARISON_GREATER_OR_EQUAL
}   t_transpiler_cobol_comparison_operator;

typedef struct s_transpiler_cobol_condition
{
    const char *left;
    const char *right;
    t_transpiler_cobol_comparison_operator op;
    int negated;
}   t_transpiler_cobol_condition;

typedef struct s_transpiler_cobol_statement t_transpiler_cobol_statement;

typedef struct s_transpiler_cobol_statement_block
{
    t_transpiler_cobol_statement **statements;
    size_t count;
    size_t capacity;
}   t_transpiler_cobol_statement_block;

typedef enum e_transpiler_cobol_statement_kind
{
    TRANSPILE_COBOL_STATEMENT_MOVE = 0,
    TRANSPILE_COBOL_STATEMENT_IF,
    TRANSPILE_COBOL_STATEMENT_PERFORM_UNTIL,
    TRANSPILE_COBOL_STATEMENT_PERFORM_VARYING,
    TRANSPILE_COBOL_STATEMENT_CALL
}   t_transpiler_cobol_statement_kind;

typedef struct s_transpiler_cobol_move_statement
{
    const char *source;
    const char *target;
}   t_transpiler_cobol_move_statement;

typedef struct s_transpiler_cobol_call_statement
{
    const char *subprogram;
    const char **arguments;
    size_t argument_count;
    size_t argument_capacity;
    const char *return_slot;
}   t_transpiler_cobol_call_statement;

typedef struct s_transpiler_cobol_if_statement
{
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block then_branch;
    t_transpiler_cobol_statement_block else_branch;
}   t_transpiler_cobol_if_statement;

typedef struct s_transpiler_cobol_perform_until
{
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block body;
}   t_transpiler_cobol_perform_until;

typedef struct s_transpiler_cobol_perform_varying
{
    const char *counter;
    const char *initial;
    const char *step;
    t_transpiler_cobol_condition condition;
    t_transpiler_cobol_statement_block body;
}   t_transpiler_cobol_perform_varying;

typedef struct s_transpiler_cobol_statement
{
    t_transpiler_cobol_statement_kind kind;
    t_transpiler_cobol_move_statement move;
    t_transpiler_cobol_if_statement if_statement;
    t_transpiler_cobol_perform_until perform_until;
    t_transpiler_cobol_perform_varying perform_varying;
    t_transpiler_cobol_call_statement call;
}   t_transpiler_cobol_statement;

typedef struct s_transpiler_cobol_paragraph
{
    char *name;
    t_transpiler_cobol_statement_block statements;
}   t_transpiler_cobol_paragraph;

typedef struct s_transpiler_cobol_procedure
{
    t_transpiler_cobol_paragraph **paragraphs;
    size_t count;
    size_t capacity;
}   t_transpiler_cobol_procedure;

void transpiler_cobol_statement_block_init(t_transpiler_cobol_statement_block *block);
void transpiler_cobol_statement_block_dispose(t_transpiler_cobol_statement_block *block);
int transpiler_cobol_statement_block_append(t_transpiler_cobol_statement_block *block,
    t_transpiler_cobol_statement *statement);
t_transpiler_cobol_statement *transpiler_cobol_statement_create_move(const char *source, const char *target);
t_transpiler_cobol_statement *transpiler_cobol_statement_create_if(const t_transpiler_cobol_condition *condition);
t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_until(
    const t_transpiler_cobol_condition *condition);
t_transpiler_cobol_statement *transpiler_cobol_statement_create_perform_varying(const char *counter,
    const char *initial, const char *step, const t_transpiler_cobol_condition *condition);
t_transpiler_cobol_statement *transpiler_cobol_statement_create_call(const char *subprogram);
int transpiler_cobol_call_append_argument(t_transpiler_cobol_statement *statement, const char *argument);
int transpiler_cobol_call_set_return_slot(t_transpiler_cobol_statement *statement, const char *return_slot);
void transpiler_cobol_statement_destroy(t_transpiler_cobol_statement *statement);
t_transpiler_cobol_statement_block *transpiler_cobol_if_get_then_branch(t_transpiler_cobol_statement *statement);
t_transpiler_cobol_statement_block *transpiler_cobol_if_get_else_branch(t_transpiler_cobol_statement *statement);
t_transpiler_cobol_statement_block *transpiler_cobol_perform_until_get_body(t_transpiler_cobol_statement *statement);
t_transpiler_cobol_statement_block *transpiler_cobol_perform_varying_get_body(t_transpiler_cobol_statement *statement);
t_transpiler_cobol_paragraph *transpiler_cobol_paragraph_create(const char *name);
void transpiler_cobol_paragraph_destroy(t_transpiler_cobol_paragraph *paragraph);
t_transpiler_cobol_statement_block *transpiler_cobol_paragraph_get_statements(t_transpiler_cobol_paragraph *paragraph);
void transpiler_cobol_procedure_init(t_transpiler_cobol_procedure *procedure);
void transpiler_cobol_procedure_dispose(t_transpiler_cobol_procedure *procedure);
int transpiler_cobol_procedure_append(t_transpiler_cobol_procedure *procedure,
    t_transpiler_cobol_paragraph *paragraph);

typedef struct s_transpiler_cobol_file_sections
{
    char *environment_division;
    char *data_division;
}   t_transpiler_cobol_file_sections;

void transpiler_codegen_file_sections_init(t_transpiler_cobol_file_sections *sections);
void transpiler_codegen_file_sections_dispose(t_transpiler_cobol_file_sections *sections);
int transpiler_codegen_build_file_sections(const t_transpiler_context *context,
    t_transpiler_cobol_file_sections *sections);
int transpiler_codegen_build_procedure_division(const t_transpiler_cobol_procedure *procedure,
    char **out);

int cblc_formatter_format(const char *input, t_transpiler_format_mode mode, char **output);

// ===================================
// Standard library generation helpers
// ===================================
typedef enum e_transpiler_standard_library_buffer_kind
{
    TRANSPILE_STANDARD_LIBRARY_BUFFER_NONE = 0,
    TRANSPILE_STANDARD_LIBRARY_BUFFER_CHAR,
    TRANSPILE_STANDARD_LIBRARY_BUFFER_STRING
}   t_transpiler_standard_library_buffer_kind;

typedef struct s_transpiler_standard_library_entry
{
    const char *qualified_name;
    t_transpiler_standard_library_buffer_kind buffer_kind;
    const char *program_name;
    int (*generator)(char **out_text);
}   t_transpiler_standard_library_entry;

void transpiler_standard_library_reset_usage(void);
void transpiler_standard_library_note_strlen_usage(size_t declared_length);
size_t transpiler_standard_library_get_strlen_limit(void);
void transpiler_standard_library_note_strlen_string_usage(size_t declared_length);
size_t transpiler_standard_library_get_strlen_string_limit(void);
int transpiler_standard_library_generate_date_duration(char **out_text);
int transpiler_standard_library_generate_date_yyyymmdd(char **out_text);
int transpiler_standard_library_generate_strlen(char **out_text);
int transpiler_standard_library_generate_strlen_string(char **out_text);
int transpiler_standard_library_generate_strnlen_string(char **out_text);
int transpiler_standard_library_generate_strnlen(char **out_text);
int transpiler_standard_library_generate_strcmp(char **out_text);
int transpiler_standard_library_generate_strcmp_string(char **out_text);
int transpiler_standard_library_generate_strcpy(char **out_text);
int transpiler_standard_library_generate_strcpy_string(char **out_text);
int transpiler_standard_library_generate_strncpy(char **out_text);
int transpiler_standard_library_generate_strncpy_string(char **out_text);
int transpiler_standard_library_generate_memcmp(char **out_text);
int transpiler_standard_library_generate_memcmp_string(char **out_text);
int transpiler_standard_library_generate_strcat(char **out_text);
int transpiler_standard_library_generate_strcat_string(char **out_text);
int transpiler_standard_library_generate_strtod(char **out_text);
int transpiler_standard_library_generate_strtod_string(char **out_text);
int transpiler_standard_library_generate_abs(char **out_text);
int transpiler_standard_library_generate_fabs(char **out_text);
int transpiler_standard_library_generate_floor(char **out_text);
int transpiler_standard_library_generate_ceil(char **out_text);
int transpiler_standard_library_generate_exp(char **out_text);
int transpiler_standard_library_generate_log(char **out_text);
int transpiler_standard_library_generate_sin(char **out_text);
int transpiler_standard_library_generate_cos(char **out_text);
int transpiler_standard_library_generate_tan(char **out_text);
int transpiler_standard_library_generate_rounded(char **out_text);
int transpiler_standard_library_generate_banker_round(char **out_text);
int transpiler_standard_library_generate_atoi(char **out_text);
int transpiler_standard_library_generate_atoi_string(char **out_text);
int transpiler_standard_library_generate_atol(char **out_text);
int transpiler_standard_library_generate_atol_string(char **out_text);
int transpiler_standard_library_generate_atoll(char **out_text);
int transpiler_standard_library_generate_atoll_string(char **out_text);
int transpiler_standard_library_generate_powerof(char **out_text);
int transpiler_standard_library_generate_sqrt(char **out_text);
int transpiler_standard_library_generate_min(char **out_text);
int transpiler_standard_library_generate_max(char **out_text);
int transpiler_standard_library_generate_toupper(char **out_text);
int transpiler_standard_library_generate_toupper_string(char **out_text);
int transpiler_standard_library_generate_tolower(char **out_text);
int transpiler_standard_library_generate_tolower_string(char **out_text);
int transpiler_standard_library_generate_isdigit(char **out_text);
int transpiler_standard_library_generate_isalpha(char **out_text);

const t_transpiler_standard_library_entry *transpiler_standard_library_get_entries(size_t *count);
const t_transpiler_standard_library_entry *transpiler_standard_library_lookup(const char *qualified_name);
const t_transpiler_standard_library_entry *transpiler_standard_library_lookup_with_buffer_kind(
    const char *qualified_name, t_transpiler_standard_library_buffer_kind buffer_kind);

// ============================
// Command line and diagnostics
// ============================
typedef struct s_transpiler_cli_options
{
    const char **input_paths;
    size_t input_count;
    size_t input_capacity;
    const char **output_paths;
    size_t output_count;
    size_t output_capacity;
    const char *output_directory;
    t_transpiler_language source_language;
    t_transpiler_language target_language;
    t_transpiler_format_mode format_mode;
    t_transpiler_diagnostic_level diagnostic_level;
    int warnings_as_errors;
    t_transpiler_warning_settings warning_settings;
    int show_help;
}   t_transpiler_cli_options;

int transpiler_cli_options_init(t_transpiler_cli_options *options);
void transpiler_cli_options_dispose(t_transpiler_cli_options *options);
int transpiler_cli_parse(t_transpiler_cli_options *options, int argc, const char **argv);
int transpiler_cli_apply(const t_transpiler_cli_options *options, t_transpiler_context *context);
void transpiler_cli_print_usage(void);

int transpiler_logging_emit(t_transpiler_context *context, t_transpiler_severity severity, int code, const char *message);
void transpiler_logging_stage_start(t_transpiler_context *context, const char *stage_name);
void transpiler_logging_stage_success(t_transpiler_context *context, const char *stage_name);
void transpiler_logging_stage_failure(t_transpiler_context *context, const char *stage_name, int error_code);
void transpiler_logging_flush(const t_transpiler_context *context);

#endif
