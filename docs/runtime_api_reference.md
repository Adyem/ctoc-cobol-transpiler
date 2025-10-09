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
- `CBLC-MEMCMP` compares two caller-supplied buffers (each up to 255 characters)
  using their declared lengths, an explicit byte-count limit, and a trailing
  signed result slot. The helper clamps the comparison to the smallest of the
  declared lengths, the caller-provided count, and the 255-byte ceiling, never
  reads past those limits, and writes `-1`, `0`, or `1` into the result
  depending on the ordering of the examined bytes.
- `CBLC-STRCAT` appends two caller-supplied alphanumeric buffers (each up to
  255 characters) into a destination buffer passed by reference along with its
  declared length. The helper scans the destination to find the existing
  content length, appends the left operand followed by the right operand while
  respecting the destination limit, reports truncation through a numeric status
  flag, and writes the resulting character count into the trailing return slot.
- `CBLC-ATOI` converts a caller-supplied alphanumeric buffer (up to 255
  characters) that is passed by reference alongside a declared length supplied
  by value. The helper trims leading spaces, accepts an optional sign, validates
  that the remaining characters are decimal digits, and accumulates the value in
  a signed `PIC S9(9)` result slot. It clamps processing to the declared length,
  rejects non-digit characters or values outside the ±999,999,999 range, and
  reports success or failure through a trailing numeric status flag while
  preserving the original buffer. Callers reference the helper through
  `std::atoi`.
- `CBLC-ATOL` extends `CBLC-ATOI` to emit signed `PIC S9(18)` results suitable
  for widened integer domains. It accepts the same calling convention, enforces
  digit-only input, rejects values outside the ±999,999,999,999,999,999 range,
  and reports status via the trailing numeric slot. Callers reference the helper
  through `std::atol`.
- `CBLC-ATOLL` mirrors the above helpers but widens the trailing result slot to
  `PIC S9(36)` so callers can capture extended integral domains required by the
  transpiler. The routine trims leading spaces, honors an optional sign, scans
  digits within the caller-declared length (up to 255 characters), rejects
  trailing non-space content, and reports overflow or invalid input through the
  status flag while leaving the input buffer untouched. Callers reference the
  helper through `std::atoll`.
- `CBLC-STRTOD` converts caller-supplied alphanumeric buffers (up to 255
  characters) into floating results using COBOL's `FUNCTION NUMVAL`. The helper
  trims leading and trailing spaces, validates optional signs, decimal points,
  and scientific-notation exponents, and refuses embedded spaces or invalid
  characters. It respects the caller-declared length, caps scanning at 255
  characters, writes the parsed floating value into the trailing `USAGE COMP-2`
  slot, and reports domain or range errors through a trailing numeric status
  flag. Callers reference the helper through `std::strtod`.
- `CBLC-ABS` accepts a caller-supplied signed integral operand by reference
  along with trailing slots for the absolute-value result (`PIC S9(36) COMP-5`)
  and a numeric status flag. The helper copies the operand into the result,
  negates it when the value is negative, and uses `ON SIZE ERROR` handling to
  detect overflow for the minimum representable value. On overflow it zeroes the
  result and returns status `1`; otherwise it writes the magnitude and returns
  status `0`. Callers reference the helper through `std::abs`.
- `CBLC-FABS` mirrors `CBLC-ABS` for floating operands. Callers pass the source
  `USAGE COMP-2` value, a trailing floating result slot, and a numeric status
  flag by reference. The helper computes the absolute value via `FUNCTION ABS`,
  stores the magnitude, and reports success with status `0`. Callers reference
  the helper through `std::fabs`.
- `CBLC-FLOOR` rounds a caller-supplied floating operand (`USAGE COMP-2`) toward
  negative infinity. Callers provide the source value, a trailing result slot,
  and a numeric status flag by reference. The helper copies the integer part of
  the operand into the result, decrements by one for negative values with
  fractional components, and writes status `1` when it had to adjust the value
  (otherwise `0`). Callers reference the helper through `std::floor`.
- `CBLC-CEIL` rounds a caller-supplied floating operand (`USAGE COMP-2`) toward
  positive infinity. Callers provide the operand, a trailing result slot, and a
  numeric status flag by reference. The helper copies the integer part of the
  operand into the result, increments by one for positive values with fractional
  components, and writes status `1` when an adjustment occurs (otherwise `0`).
  Callers reference the helper through `std::ceil`.
- `CBLC-EXP` widens a caller-supplied floating operand (`USAGE COMP-2`) through
  `FUNCTION EXP` to produce the mathematical exponential. Callers provide the
  operand, a trailing floating result slot, and a numeric status flag by
  reference. The helper initializes status to `0`, stores the computed
  exponential in the result, and raises status `1` while zeroing the output when
  COBOL signals a size error (overflow). Callers reference the helper through
  `std::exp`.
- `CBLC-LOG` computes the natural logarithm of a caller-supplied floating
  operand (`USAGE COMP-2`). Callers provide the operand, a trailing floating
  result slot, and a numeric status flag by reference. The helper rejects
  non-positive inputs by returning status `1` with a zeroed result, otherwise it
  invokes `FUNCTION LOG` and reports overflow through the same status flag.
  Callers reference the helper through `std::log`.
- `CBLC-SIN` accepts a floating operand (`USAGE COMP-2`) by reference together
  with trailing result and status slots. The helper sets status to `0`, applies
  `FUNCTION SIN` to produce the result, and records status `1` with a zeroed
  output if COBOL signals a size error during evaluation. Callers reference the
  helper through `std::sin`.
- `CBLC-COS` mirrors `CBLC-SIN` but computes `FUNCTION COS` for the supplied
  operand. It writes the cosine into the result slot, maintains a `0` status for
  successful evaluations, and switches the status to `1` while zeroing the
  result when a size error occurs. Callers reference the helper through
  `std::cos`.
- `CBLC-TAN` evaluates `FUNCTION TAN` for a caller-provided floating operand,
  storing the result and returning status `0` when finite. Inputs that cause
  COBOL to raise a size error (for example, values near odd multiples of
  `PI/2`) yield a zeroed result and status `1`. Callers reference the helper
  through `std::tan`.
- `CBLC-TOUPPER` mutates a caller-supplied alphanumeric buffer (up to 255
  characters) in place, converting any lowercase ASCII letters to uppercase.
  Callers pass the buffer by reference, the declared length by value, and a
  trailing numeric status slot by reference. The helper scans no more than the
  declared length (clamped to 255), stops on NUL bytes, updates letters via
  `INSPECT ... CONVERTING`, and leaves the status flag at `0` to report
  success.
- `CBLC-TOLOWER` mirrors `CBLC-TOUPPER` but converts uppercase ASCII letters to
  lowercase while leaving other characters untouched. It respects the declared
  length (capped at 255), stops on NUL bytes, and returns status `0` in the
  trailing slot.
- `CBLC-ISDIGIT` accepts a single-character alphanumeric operand by reference
  along with a trailing numeric slot. It moves the operand into a working
  storage buffer, checks whether the value falls within the ASCII `'0'`–`'9'`
  range, and writes `1` to the result when it does (otherwise `0`). The helper
  leaves the operand unmodified so callers can reuse the character.
- `CBLC-ISALPHA` shares the same calling convention as `CBLC-ISDIGIT` but
  recognizes uppercase and lowercase ASCII letters. It reports `1` for values in
  the `'A'`–`'Z'` or `'a'`–`'z'` ranges and `0` for any other character without
  altering the input.
- `CBLC-POWEROF` raises a caller-supplied base to a caller-supplied exponent.
  Both operands are passed by reference as `USAGE COMP-2` values along with a
  trailing floating result slot and numeric status flag. The helper rejects
  zero bases paired with non-positive exponents and negative bases paired with
  fractional exponents by zeroing the result and returning status `1`. Valid
  inputs compute `base ** exponent`, storing the floating result and reporting
  status `0` (size errors or overflow also return status `1`). Callers reference
  the helper through `std::pow`.
- `CBLC-SQRT` accepts a floating operand (`USAGE COMP-2`) by reference along
  with a trailing floating result slot and numeric status flag. The helper
  rejects negative operands by zeroing the result and returning status `1`,
  otherwise computing the square root via `FUNCTION SQRT` and writing the
  result with status `0`. Callers reference the helper through `std::sqrt` and
  are expected to widen narrower numeric inputs before invocation.
- Callers reference standard library helpers through the `std::` prefix (for
  example, `std::strlen`, `std::strnlen`, `std::strcmp`, `std::strcpy`, `std::strncpy`, `std::strcat`, or `std::sqrt`), which resolves to the COBOL
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
