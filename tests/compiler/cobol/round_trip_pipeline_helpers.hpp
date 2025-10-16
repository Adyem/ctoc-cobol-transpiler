#ifndef ROUND_TRIP_PIPELINE_HELPERS_HPP
#define ROUND_TRIP_PIPELINE_HELPERS_HPP

int compile_generated_cobol(const char *binary_path, const char *source_path,
    const char *log_path, const char *failure_context);
int compile_generated_cobol_with_module(const char *binary_path,
    const char *primary_source_path, const char *secondary_source_path,
    const char *log_path, const char *failure_context);
int execute_binary(const char *directory, const char *binary_name,
    const char *output_name, const char *failure_context);

#endif
