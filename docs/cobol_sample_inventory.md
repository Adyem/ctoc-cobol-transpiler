# COBOL Sample Inventory

This catalog enumerates the COBOL fixtures that drive validation of the reverse pipeline. Every program lives in
`samples/cobol` and is registered in `samples/cobol/manifest.txt` so automated checks can keep the documentation and
filesystem synchronized.

## Sample Coverage

### `samples/cobol/copy_file.cob`
- **Purpose:** Establishes a baseline file copy loop for the COBOL input corpus that mirrors the canonical CBL-C example.
- **Constructs:** `FILE-CONTROL` entries for input/output handles, sequential `READ`/`WRITE` operations, and a `PERFORM UNTIL`
  loop with `AT END` handling.

### `samples/cobol/filter_prefix.cob`
- **Purpose:** Demonstrates record filtering with substring comparisons to exercise conditional evaluation over COBOL
  structures.
- **Constructs:** Group level records, substring `IF` comparisons, iterative `READ` logic, and conditional `WRITE`
  statements.

### `samples/cobol/record_writer.cob`
- **Purpose:** Captures structured record emission with elementary items so the forward translator can validate DATA DIVISION
  layouts.
- **Constructs:** Group data items, literal assignments, arithmetic updates, and sequential file output.

## Maintenance Checklist

1. Add a new `.cob` file under `samples/cobol` when introducing COBOL-side language features that need sample coverage.
2. Append the file path to `samples/cobol/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
