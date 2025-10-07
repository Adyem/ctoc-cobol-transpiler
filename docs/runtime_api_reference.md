# Runtime Helper API Reference

This document summarizes the libft-backed helper functions that generated code
uses at runtime. Each module mirrors the naming convention used throughout the
transpiler (`runtime_*`) so that code generation can link directly against the
implementations provided in this repository.

## Memory Management Strategy

All runtime helpers avoid the C++ standard library and instead rely on the
libft portable memory layer. Any allocation performed inside the helpers uses
`cma_calloc`/`cma_free`, and every function that hands ownership back to caller
clearly documents the expectations around allocation and disposal. Generated
programs should never free buffers manually; instead they call the companion
`dispose` helpers for each module.

## Scalar Helpers (`runtime_scalar`)

The scalar module wraps primitive integer and character values in simple
structures so the transpiler can pass them by reference. The following entry
points are stable and safe for generated code to consume:

- `runtime_int_set`, `runtime_int_from_string`, `runtime_int_add`,
  `runtime_int_subtract`, `runtime_int_multiply`, `runtime_int_divide`, and
  `runtime_int_compare` manage arithmetic while guarding against overflow.
- `runtime_int_to_string` exposes formatting when COBOL DISPLAY clauses need to
  render numeric values.
- `runtime_char_set`, `runtime_char_from_string`, `runtime_char_to_upper`,
  `runtime_char_to_lower`, `runtime_char_compare`, and `runtime_char_to_string`
  provide character-level transformations that mirror COBOL MOVE and INSPECT
  semantics.

All scalar helpers expect non-null destination pointers and leave values
unchanged when validation fails. This makes them safe to compose within more
complex generated statements.

## String Helpers (`runtime_string`)

Generated programs rely on the dynamic string wrapper when translating COBOL
alphanumeric data to libft-friendly buffers. The API comprises:

- `runtime_string_init` / `runtime_string_dispose` for allocation lifecycle.
- `runtime_string_assign` and `runtime_string_trim` to ingest COBOL literals and
  enforce ANSI-85 trimming rules.
- `runtime_string_compare`, `runtime_string_equals`, and
  `runtime_string_concat` to support relational operators and concatenation.
- `runtime_string_to_int` bridges to `runtime_scalar` when numeric conversions
  are required.

All string buffers expand automatically using CMA-backed growth and guarantee a
null-terminated `data` pointer so they remain compatible with libft routines.

## Record Helpers (`runtime_record`)

COBOL records map to mutable buffers managed through `runtime_record`. Generated
code uses these helpers to size, fill, and copy record payloads without touching
raw allocation APIs:

- `runtime_record_init` / `runtime_record_dispose` establish the backing
  storage.
- `runtime_record_set_length` adjusts logical record length while preserving the
  allocated capacity.
- `runtime_record_fill` initializes slack space with a filler byte (typically
  space).
- `runtime_record_copy_from_buffer` and `runtime_record_copy_to_buffer` marshal
  between external buffers (file I/O, DISPLAY) and the record storage owned by
  the runtime.

All record helpers return `FT_SUCCESS`/`FT_FAILURE` and never leak when callers
follow the init/dispose contract.

## File Helpers (`runtime_file`)

The file module provides a thin abstraction over POSIX descriptors that mirrors
COBOL file verbs:

- `runtime_file_init` clears descriptors before use.
- `runtime_file_open_read` / `runtime_file_open_write` expose the supported open
  modes (input and output/extend).
- `runtime_file_read` / `runtime_file_write` translate to `read`/`write` system
  calls while surfacing byte counts to the caller.
- `runtime_file_close` finalizes the descriptor once COBOL CLOSE statements
  execute.

Generated code composes file helpers with record helpers to move data between
files and working-storage records.

## Usage Guidance

1. Always call the relevant `*_init` function before passing a runtime object to
   generated statements.
2. Propagate error codes (`FT_SUCCESS`/`FT_FAILURE`) back into the COBOL status
   flow. The generated code stubs already follow this pattern, so new helpers
   should continue to use the same contract.
3. Prefer composing helpers instead of accessing struct fields directly. The
   accessors maintain validation and trimming rules required for COBOL
   compatibility.

By adhering to these rules, generated programs remain portable across platforms
that supply the libft compatibility layer.
