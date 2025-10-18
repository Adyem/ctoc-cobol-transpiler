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
- `runtime_string_blank` and `runtime_string_copy_checked` provide bounds-
  checked operations for fixed-width caller buffers, blanking destinations with
  spaces, copying no more than the provided source length, and reporting the
  number of characters moved plus any truncation.

All string buffers expand automatically using CMA-backed growth and guarantee a
null-terminated `data` pointer so they remain compatible with libft routines.

## Memory Helpers (`runtime_memory`)

Generated code frequently moves raw bytes between overlapping buffers when
reshaping COBOL records. The memory helper module exposes
`runtime_memory_copy_checked`, which validates source and destination lengths,
refuses to overrun the destination, and falls back to `memmove` semantics when
regions overlap. Callers that supply zero lengths are treated as no-ops so the
helper can guard optional fields without branching in generated code.

## Sorting Helpers (`runtime_sort`)

Many COBOL workflows manipulate indexed record sets. The sorting module pairs a
`t_runtime_record_key` description with helpers that operate on `t_runtime_record`
arrays:

- `runtime_record_compare_keys` evaluates two records against one or more key
  segments, supports ascending or descending order, and pads short records with
  spaces so variable-length entries remain comparable.
- `runtime_record_sort` performs a stable insertion sort over caller-supplied
  record arrays using the provided keys, moving only the lightweight record
  structures while leaving the underlying buffers in place.
- `runtime_record_search_all` implements a binary search equivalent to COBOL's
  `SEARCH ALL`, returning whether a key is present and reporting the insertion
  point for new records.

These helpers guard null pointers, reject empty key spans, and make it easy for
generated code to provide deterministic ordering without pulling in additional
runtime dependencies.

## CSV and Line I/O Helpers (`runtime_csv`)

Text integrations frequently exchange comma-separated values or line-oriented
records. The CSV module introduces a handful of utilities that build on the
existing file helpers:

- `runtime_csv_parse_line` splits a line into pre-initialized
  `t_runtime_string` fields, honoring ANSI CSV quoting rules (including doubled
  quotes) and optional trailing commas.
- `runtime_csv_format_line` emits a CSV row from a field array, quoting values
  that contain commas, spaces, quotes, or newlines while
  escaping embedded quotes.
- `runtime_line_read_fixed` and `runtime_line_write_fixed` read and write
  fixed-length records by blank-padding shorter inputs and returning the exact
  byte window requested.
- `runtime_line_read_variable` and `runtime_line_write_variable` consume or
  produce delimited records (typically newline-terminated), skipping carriage
  returns and signalling end-of-file through an optional flag.

The helpers reuse `runtime_file` descriptors, making them safe to mix with other
runtime I/O routines.

## Encoding Helpers (`runtime_encoding`)

COBOL environments often operate on EBCDIC data while the transpiler emits
ASCII-compatible CBL-C. The encoding module introduces explicit transcoding
points so generated programs can normalize buffers at the boundaries:

- `runtime_encoding_reset` restores the default CCSID 037 tables, ensuring
  callsites start from a known mapping before applying overrides.
- `runtime_encoding_get_active` copies the currently active tables into a
  caller-provided `t_runtime_encoding_table` so native integrations can inspect
  or persist the mapping.
- `runtime_encoding_set_active` copies caller-supplied tables into the runtime,
  enabling alternate CCSIDs or patched character maps.
- `runtime_encoding_transcode_to_ascii` and
  `runtime_encoding_transcode_to_ebcdic` translate buffers using the active
  tables, validate source/destination lengths, and report how many bytes were
  written.

The helper enforces explicit destination lengths to guard fixed-width COBOL
fields and rejects null pointers when a non-zero length is requested.

## Collation Helpers (`runtime_collation`)

String comparisons now flow through a dedicated collation layer. By default the
module performs locale-independent ASCII comparisons so diagnostics and runtime
semantics remain deterministic. Advanced hosts can register a custom bridge via
`runtime_collation_set_bridge`, supplying a callback that receives both operands
and writes the comparison result. The active bridge can be inspected with
`runtime_collation_get_bridge` or cleared with `runtime_collation_clear_bridge`
to fall back to the ASCII behavior.

Generated helpers call `runtime_collation_compare` directly, so any registered
bridge automatically applies to `runtime_string_compare` and standard library
subprograms that depend on lexicographic ordering.

## Standard Library Subprograms

Generated programs rely on a small catalog of COBOL subprograms to bridge
between CBL-C semantics and the runtime helpers. The current catalog covers the
following helpers:

- `CBLC-STRLEN` accepts a by-reference alphanumeric buffer sized to the
  largest caller observed during compilation (defaulting to 255 characters
  when no usage is recorded), the caller-declared length passed by value, and a trailing
  by-reference numeric slot that receives the computed length. The subprogram
  scans at most the caller-provided length, exits early on NUL bytes, trims
  trailing spaces, and writes the resulting character count into the return
  slot.
- `CBLC-STRLEN-STRING` accepts a by-reference group containing a `PIC 9(4)
  COMP` length field alongside an alphanumeric buffer sized to the largest
  string operand observed for the build (again defaulting to 255 characters)
  plus a trailing numeric slot for the result. The helper simply moves the
  caller-maintained length field into the return slot so string objects retain
  their internal length metadata when passed across module boundaries.
- `CBLC-STRNLEN` accepts the same parameters as `CBLC-STRLEN` plus an explicit
  request limit passed by value. It caps the scan to the minimum of the declared
  length, requested length, and 255-character ceiling, exits early on NUL bytes,
  trims trailing spaces within that window, and writes the resulting character
  count into the trailing numeric slot.
- `CBLC-STRNLEN-STRING` accepts a by-reference string group containing the
  caller-maintained length field, the backing buffer (up to 255 characters),
  and a by-value request length. It compares the stored length and request,
  clamps the result to the smaller bound, and copies the effective length into
  the trailing numeric slot without scanning the buffer.
- `CBLC-STRCMP` compares two alphanumeric buffers (each up to 255 characters)
  supplied by reference along with their caller-declared lengths passed by
  value. It computes the lexicographic ordering consistent with ANSI COBOL
  string comparisons and populates a signed numeric result in the trailing
  return slot (`-1`, `0`, or `1`) while honoring the provided length limits.
- `CBLC-STRCMP-STRING` accepts two string groups by reference (each with a
  length field and 255-character buffer) plus a signed numeric result slot. It
  compares the buffers up to the smaller stored length, applies the same
  lexicographic ordering rules as `CBLC-STRCMP`, and caps comparisons at 255
  characters to preserve safety.
- `CBLC-STRCPY` copies from a caller-supplied source buffer (up to 255
  characters) into a destination buffer passed by reference along with their
  respective declared lengths. The subprogram blanks the destination up to its
  declared size, copies characters until either length limit is reached, and
  sets a numeric status flag in the trailing return slot (`0` for success,
  `1` when truncation occurs).
- `CBLC-STRCPY-STRING` performs the same copy semantics for runtime string
  groups that contain an explicit length field and backing buffer. Both the
  destination and source arrive by reference, along with a numeric status slot.
  The helper clears the destination buffer, copies up to the stored source
  length capped at 255 characters, updates the destination's length field to
  the number of characters copied, and raises the status flag when the source
  length exceeds the helper's safety ceiling.
- `CBLC-STRNCPY` performs a bounded copy between caller-supplied alphanumeric
  buffers (each up to 255 characters) using their declared lengths plus an
  explicit request length. The routine blanks the destination, copies up to the
  minimum of the declared lengths and request, pads with spaces when the source
  is shorter, and writes a status flag to the trailing return slot (`0` for
  success, `1` when the request exceeds the available source or destination
  space).
- `CBLC-STRNCPY-STRING` mirrors the bounded copy for runtime string groups. The
  destination and source arrive with their maintained lengths, while the
  request length is passed by value alongside a numeric status slot. The helper
  blanks the destination buffer, copies up to the minimum of the stored source
  length, the request, and the 255-character ceiling, updates the destination
  length field to the actual number of characters copied, and sets the status
  flag whenever either the source length or request exceeds the supported
  bounds.
- `CBLC-MEMCMP` compares two caller-supplied buffers (each up to 255 characters)
  using their declared lengths, an explicit byte-count limit, and a trailing
  signed result slot. The helper clamps the comparison to the smallest of the
  declared lengths, the caller-provided count, and the 255-byte ceiling, never
  reads past those limits, and writes `-1`, `0`, or `1` into the result
  depending on the ordering of the examined bytes.
- `CBLC-MEMCMP-STRING` performs the same bounded comparison for runtime string
  groups. Each operand arrives by reference with an explicit length and backing
  buffer, while the caller still supplies the byte-count limit by value along
  with a signed result slot. The helper caps all lengths at 255 characters,
  compares the buffers up to the smallest effective bound, and reports the
  ordering through the trailing numeric result (`-1`, `0`, or `1`).
- `CBLC-STRCAT` appends two caller-supplied alphanumeric buffers (each up to
  255 characters) into a destination buffer passed by reference along with its
  declared length. The helper scans the destination to find the existing
  content length, appends the left operand followed by the right operand while
  respecting the destination limit, reports truncation through a numeric status
  flag, and writes the resulting character count into the trailing return slot.
- `CBLC-STRCAT-STRING` concatenates two runtime string groups into a destination
  string group. All three arrive by reference alongside a numeric status slot.
  The routine bases its capacity on the destination's 255-character buffer,
  appends the left and right buffers in order while capping growth at that
  limit, updates the destination's stored length to the number of characters
  written, and raises the status flag when truncation occurs.
- `CBLC-ATOI` converts a caller-supplied alphanumeric buffer (up to 255
  characters) that is passed by reference alongside a declared length supplied
  by value. The helper trims leading spaces, accepts an optional sign, validates
  that the remaining characters are decimal digits, and accumulates the value in
  a signed `PIC S9(9)` result slot. It clamps processing to the declared length,
  rejects non-digit characters or values outside the ±999,999,999 range, and
  reports success or failure through a trailing numeric status flag while
  preserving the original buffer. Callers reference the helper through
  `std::atoi`.
- `CBLC-ATOI-STRING` applies the same validation and conversion logic to runtime
  string groups. The source arrives by reference with an explicit length and
  255-byte buffer, while the routine emits a signed `PIC S9(9)` result and
  numeric status flag by reference. The helper caps processing at the recorded
  string length (up to 255 characters), preserves the source buffer, and reports
  malformed or out-of-range input through the trailing status slot.
- `CBLC-ATOL` extends `CBLC-ATOI` to emit signed `PIC S9(18)` results suitable
  for widened integer domains. It accepts the same calling convention, enforces
  digit-only input, rejects values outside the ±999,999,999,999,999,999 range,
  and reports status via the trailing numeric slot. Callers reference the helper
  through `std::atol`.
- `CBLC-ATOL-STRING` mirrors `CBLC-ATOL` for runtime string groups. The source
  string is supplied by reference with its length and 255-character buffer, and
  the routine writes the widened numeric result plus a status flag by reference.
  It respects the stored length, enforces digit-only content (after trimming an
  optional sign), and flags overflow or invalid characters without altering the
  caller's buffer.
- `CBLC-ATOLL` mirrors the above helpers but widens the trailing result slot to
  `PIC S9(36)` so callers can capture extended integral domains required by the
  transpiler. The routine trims leading spaces, honors an optional sign, scans
  digits within the caller-declared length (up to 255 characters), rejects
  trailing non-space content, and reports overflow or invalid input through the
  status flag while leaving the input buffer untouched. Callers reference the
  helper through `std::atoll`.
- `CBLC-ATOLL-STRING` performs the same wide-range conversion for runtime string
  groups. The string arrives with its recorded length and buffer, and the helper
  emits a `PIC S9(36)` result and status flag by reference. It observes the
  stored length limit, enforces digit-only input (after handling an optional
  sign), detects overflow, and surfaces malformed data through the trailing
  status field while preserving the input characters.
- `CBLC-STRTOD` converts caller-supplied alphanumeric buffers (up to 255
  characters) into floating results using COBOL's `FUNCTION NUMVAL`. The helper
  trims leading and trailing spaces, validates optional signs, decimal points,
  and scientific-notation exponents, and refuses embedded spaces or invalid
  characters. It respects the caller-declared length, caps scanning at 255
  characters, writes the parsed floating value into the trailing `USAGE COMP-2`
  slot, and reports domain or range errors through a trailing numeric status
  flag. Callers reference the helper through `std::strtod`.
- `CBLC-STRTOD-STRING` extends the conversion to runtime string groups. The
  source string is passed by reference with its length and 255-byte buffer, and
  the helper emits the parsed floating result plus a numeric status flag by
  reference. It observes the stored length, applies the same validation rules as
  `CBLC-STRTOD`, and preserves the input buffer while reporting errors through
  the trailing status slot.
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
- `CBLC-ROUNDED` applies COBOL `ROUNDED` clause semantics to a caller-supplied
  floating operand (`USAGE COMP-2`). Callers pass the operand, a trailing result
  slot, and a numeric status flag by reference. The helper copies the integer
  portion of the operand, detects fractional components, and applies banker-style
  rounding (ties to even) before writing the final result. Status `1` indicates
  the helper adjusted the value, while status `0` reports that the operand was
  already integral. Callers reference the helper through `std::round`.
- `CBLC-BANKER-ROUND` rounds a caller-supplied floating operand (`USAGE COMP-2`)
  to a requested scale. Callers pass the operand, a `PIC S9(4) COMP-5` scale
  parameter indicating the number of digits to preserve to the right of the
  decimal point, a trailing result slot, and a numeric status flag. The helper
  computes a scaling factor, widens the operand to that precision, and applies
  banker-style rounding (ties to even). When rounding occurs it reports status
  `1`, status `0` signals that no adjustment was necessary, and status `2`
  signals either an invalid scale (less than `0` or greater than `18`) or a COBOL
  size error during scaling. Callers reference the helper through
  `cblc::banker_round`.
- `CBLC-DATE-YYYYMMDD` accepts an eight-character `PIC X(8)` buffer by
  reference and returns the parsed year, month, day, packed (`PIC 9(8) COMP-3`)
  encoding, and serial (`PIC S9(9) COMP-5`) day count through trailing result
  slots. The helper validates that every character is numeric, enforces legal
  month/day ranges with leap-year checks, zeroes all result fields when
  validation fails, and reports status codes (`0` success, `1` non-digit input,
  `2` invalid month, `3` invalid day) so callers can branch on specific errors.
- `CBLC-DATE-DURATION` consumes two serial day counts (`PIC S9(9) COMP-5`) from
  `CBLC-DATE-YYYYMMDD`, computes the absolute day difference, and writes a
  signed comparison flag (`-1`, `0`, or `1`) alongside a success status. Equal
  inputs leave the duration and comparison slots at zero so callers can detect
  matching dates without extra arithmetic.
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
- `CBLC-TOUPPER-STRING` performs the same in-place conversion for runtime string
  groups. The caller passes the string by reference, including its recorded
  length and 255-byte buffer, along with a numeric status slot. The helper caps
  traversal at the stored length, converts lowercase ASCII characters to
  uppercase within the buffer, stops at the first low-value terminator, and
  leaves the string length unchanged while reporting success via the trailing
  status flag.
- `CBLC-TOLOWER` mirrors `CBLC-TOUPPER` but converts uppercase ASCII letters to
  lowercase while leaving other characters untouched. It respects the declared
  length (capped at 255), stops on NUL bytes, and returns status `0` in the
  trailing slot.
- `CBLC-TOLOWER-STRING` mirrors the lowering behavior for runtime string groups.
  The caller supplies the string by reference, including its stored length and
  255-character buffer, along with a numeric status slot. The helper processes
  characters up to the recorded length (or the first low-value terminator),
  converts uppercase ASCII bytes to lowercase in place, preserves the length
  field, and reports completion through the trailing status flag.
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
- `CBLC-MIN` accepts two floating operands (`USAGE COMP-2`) along with a result
  slot and numeric status flag. It evaluates `FUNCTION MIN` to select the
  smaller operand, stores the result, and reports status `0`. Any size error
  zeroes the result and returns status `1`. Callers reach the helper through
  `std::fmin` and should widen integral inputs before invocation.
- `CBLC-MAX` mirrors `CBLC-MIN` but uses `FUNCTION MAX` to return the larger
  operand. It reports status `0` when the computation succeeds and propagates a
  status `1` with a zeroed result if COBOL signals a size error. Callers access
  the helper via `std::fmax`.
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
