#ifndef TRANSPILE_SEMANTIC_DUMP_HPP
#define TRANSPILE_SEMANTIC_DUMP_HPP

#include <cstddef>

#include "cblc_transpiler.hpp"

int transpiler_semantic_dump_build_output_path(const t_transpiler_context *context,
    const char *input_path, const char *resolved_output_path, const char *suffix,
    char *buffer, size_t buffer_size);
int transpiler_semantic_dump_emit(t_transpiler_context *context, const char *input_path,
    const char *resolved_output_path);

#endif
