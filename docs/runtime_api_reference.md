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

## Standard Library Subprograms

Generated programs rely on a small catalog of COBOL subprograms to bridge
between CBL-C semantics and the runtime helpers. The current catalog covers the
following helpers:

- `CBLC-STRLEN` accepts a by-reference alphanumeric buffer (up to 255
  characters), the caller-declared length passed by value, and a trailing
  by-reference numeric slot that receives the computed length. The subprogram
  scans at most the caller-provided length, exits early on NUL bytes, trims
  trailing spaces, and writes the resulting character count into the return
  slot.
- `CBLC-STRNLEN` accepts the same parameters as `CBLC-STRLEN` plus an explicit
  request limit passed by value. It caps the scan to the minimum of the declared
  length, requested length, and 255-character ceiling, exits early on NUL bytes,
  trims trailing spaces within that window, and writes the resulting character
  count into the trailing numeric slot.
- `CBLC-STRCMP` compares two alphanumeric buffers (each up to 255 characters)
  supplied by reference along with their caller-declared lengths passed by
  value. It computes the lexicographic ordering consistent with ANSI COBOL
  string comparisons and populates a signed numeric result in the trailing
  return slot (`-1`, `0`, or `1`) while honoring the provided length limits.
- `CBLC-STRCPY` copies from a caller-supplied source buffer (up to 255
  characters) into a destination buffer passed by reference along with their
  respective declared lengths. The subprogram blanks the destination up to its
  declared size, copies characters until either length limit is reached, and
  sets a numeric status flag in the trailing return slot (`0` for success,
  `1` when truncation occurs).
- `CBLC-STRNCPY` performs a bounded copy between caller-supplied alphanumeric
  buffers (each up to 255 characters) using their declared lengths plus an
  explicit request length. The routine blanks the destination, copies up to the
  minimum of the declared lengths and request, pads with spaces when the source
  is shorter, and writes a status flag to the trailing return slot (`0` for
  success, `1` when the request exceeds the available source or destination
  space).
- `CBLC-SQRT` accepts a floating operand (`USAGE COMP-2`) by reference along
  with a trailing floating result slot and numeric status flag. The helper
  rejects negative operands by zeroing the result and returning status `1`,
  otherwise computing the square root via `FUNCTION SQRT` and writing the
  result with status `0`. Callers reference the helper through `std::sqrt` and
  are expected to widen narrower numeric inputs before invocation.
- Callers reference standard library helpers through the `std::` prefix (for
  example, `std::strlen`, `std::strnlen`, `std::strcmp`, `std::strcpy`, `std::strncpy`, or `std::sqrt`), which resolves to the COBOL
  subprogram names listed above and prevents collisions with user-defined
  procedures.

The standard library subprograms are emitted alongside generated code so COBOL
callers can link against a stable ABI without duplicating helper logic.

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
