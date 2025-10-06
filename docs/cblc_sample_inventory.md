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

## Maintenance Checklist

1. Add a new `.cblc` file under `samples/cblc` when introducing language features that need sample coverage.
2. Append the file path to `samples/cblc/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
