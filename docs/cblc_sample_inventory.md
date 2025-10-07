# CBL-C Sample Inventory

This document captures the reference CBL-C snippets that exercise the currently defined surface area of the language. Each
sample lives in `samples/cblc` and is registered in `samples/cblc/manifest.txt` so automated checks can ensure the examples stay
in sync with the documentation. Executable statements appear inside named `function` blocks so the transpiler can emit matching
COBOL paragraphs, and every sample includes a `function main()` entrypoint that invokes the showcased routine.

## Sample Coverage

### `samples/cblc/copy_file.cblc`
- **Purpose:** Demonstrates the baseline file copy loop used throughout the design doc and ensures the runtime string buffer path
  is represented in fixtures.
- **Constructs:** `file` declarations, scalar `char` buffers, `function` definitions (`process_file`, `main`), `open`/`close`
  pairs, `while` loops, `read`/`write` built-ins, and string literals.

### `samples/cblc/filter_prefix.cblc`
- **Purpose:** Captures filtering logic against line records to highlight conditional evaluation inside iterative file
  processing.
- **Constructs:** `function` definitions (`filter_prefix`, `main`), `if` statements nested within loops, `starts_with` string
  predicate, and reuse of the `read`/`write` built-ins.

### `samples/cblc/record_writer.cblc`
- **Purpose:** Provides a minimal record declaration coupled with scalar assignments so the COBOL generator can verify DATA
  DIVISION layout requirements.
- **Constructs:** `record` blocks with nested field declarations, `function` definitions (`write_records`, `main`), scalar
  variables instantiated from record types, string assignments to record members, and `write` operations for structured records.

### `samples/cblc/record_summary.cblc`
- **Purpose:** Mirrors the COBOL record summarization flow so the reverse pipeline can validate counter and accumulator recovery.
- **Constructs:** `record` types with scalar members, integer state tracked in global variables, `while` loops over `read` calls,
  and conditional aggregation guarded by `starts_with`.

### `samples/cblc/reverse_constructs.cblc`
- **Purpose:** Serves as the golden CBL-C output for the reverse emitter integration test that exercises COBOL control flow and
  file I/O recovery.
- **Constructs:** Uppercase identifier normalization, `open`/`close` pairs, negated `while` loops, nested `if`/`else` blocks,
  `read` statements that capture buffers, and `write` calls forwarding the recovered record.

### `samples/cblc/reverse_normalization.cblc`
- **Purpose:** Captures the expected normalized output when COBOL paragraphs include lowercase identifiers, leading-zero
  numerics, and inconsistent spacing.
- **Constructs:** Multiple `function` blocks separated by a single blank line, uppercase identifier emission with collapsed
  underscores, canonical numeric literals, and normalized string literal quoting.

### `samples/cblc/reverse_control_flow.cblc`
- **Purpose:** Locks down the recovered CBL-C for nested conditionals and loops so the reverse emitter preserves complex
  control-flow structure.
- **Constructs:** `if` statements with negated conditions, `while` loops sourced from `PERFORM UNTIL` and `PERFORM VARYING`,
  counter initialization and increment patterns, and trailing `return ;` statements for each recovered function.

## Maintenance Checklist

1. Add a new `.cblc` file under `samples/cblc` when introducing language features that need sample coverage.
2. Append the file path to `samples/cblc/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
