#ifndef TRANSPILER_CONTEXT_HPP
#define TRANSPILER_CONTEXT_HPP

#include "libft/Libft/libft.hpp"
#include "transpiler_diagnostics.hpp"

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
#define TRANSPILE_FILE_PATH_MAX 260

typedef enum e_transpiler_function_return_mode
{
    TRANSPILE_FUNCTION_RETURN_VOID = 0,
    TRANSPILE_FUNCTION_RETURN_VALUE
}   t_transpiler_function_return_mode;

typedef struct s_transpiler_function_signature
{
    char name[TRANSPILE_FUNCTION_NAME_MAX];
    t_transpiler_function_return_mode return_mode;
}   t_transpiler_function_signature;

#define TRANSPILE_ERROR_FUNCTION_RETURNS_VALUE 1001
#define TRANSPILE_ERROR_ENTRYPOINT_INVALID_NAME 1002
#define TRANSPILE_ERROR_ENTRYPOINT_ARGUMENT_MISMATCH 1003
#define TRANSPILE_ERROR_ENTRYPOINT_DUPLICATE 1004
#define TRANSPILE_ERROR_FUNCTION_DUPLICATE_NAME 1005
#define TRANSPILE_ERROR_FILE_DUPLICATE_NAME 1006
#define TRANSPILE_ERROR_FILE_UNKNOWN 1007

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

typedef struct s_transpiler_entrypoint
{
    int present;
    char name[TRANSPILE_FUNCTION_NAME_MAX];
    int has_argument_vectors;
    int needs_argument_copy;
    char argc_identifier[TRANSPILE_IDENTIFIER_MAX];
    char argv_identifier[TRANSPILE_IDENTIFIER_MAX];
}   t_transpiler_entrypoint;

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
    t_transpiler_diagnostic_list diagnostics;
    int last_error_code;
    t_transpiler_function_signature *functions;
    size_t function_count;
    size_t function_capacity;
    t_transpiler_file_declaration *files;
    size_t file_count;
    size_t file_capacity;
    t_transpiler_entrypoint entrypoint;
}   t_transpiler_context;

int transpiler_context_init(t_transpiler_context *context);
void transpiler_context_dispose(t_transpiler_context *context);
void transpiler_context_set_languages(t_transpiler_context *context, t_transpiler_language source, t_transpiler_language target);
int transpiler_context_set_io_paths(t_transpiler_context *context, const char **source_paths, size_t source_count,
    const char **target_paths, size_t target_count);
void transpiler_context_set_output_directory(t_transpiler_context *context, const char *output_directory);
void transpiler_context_set_format_mode(t_transpiler_context *context, t_transpiler_format_mode mode);
void transpiler_context_set_diagnostic_level(t_transpiler_context *context, t_transpiler_diagnostic_level level);
void transpiler_context_record_error(t_transpiler_context *context, int error_code);
int transpiler_context_has_errors(const t_transpiler_context *context);
int transpiler_context_register_function(t_transpiler_context *context, const char *name,
    t_transpiler_function_return_mode return_mode);
const t_transpiler_function_signature *transpiler_context_find_function(const t_transpiler_context *context,
    const char *name);
int transpiler_context_register_entrypoint(t_transpiler_context *context, const char *name,
    t_transpiler_function_return_mode return_mode, const char *argc_identifier, const char *argv_identifier);
const t_transpiler_entrypoint *transpiler_context_get_entrypoint(const t_transpiler_context *context);
int transpiler_context_register_file(t_transpiler_context *context, const char *name, t_transpiler_file_role role,
    const char *path, size_t explicit_record_length);
int transpiler_context_record_file_length_hint(t_transpiler_context *context, const char *name, size_t record_length);
const t_transpiler_file_declaration *transpiler_context_get_files(const t_transpiler_context *context, size_t *count);

#endif
