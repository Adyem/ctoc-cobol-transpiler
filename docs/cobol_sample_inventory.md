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

### `samples/cobol/record_summary.cob`
- **Purpose:** Aggregates accepted records and total amounts to exercise numeric accumulation with conditional gating.
- **Constructs:** Sequential file input, status-driven conditional logic, `ADD` statements updating counters, and stored totals
  in WORKING-STORAGE.

### `samples/cobol/reverse_constructs.cob`
- **Purpose:** Drives the COBOL→CBL-C reverse emitter against the constructs it currently recovers so we can enforce the output
  shape with golden fixtures.
- **Constructs:** `PERFORM UNTIL` loops, `READ ... INTO` file operations, guarded `IF`/`ELSE` blocks, `WRITE ... FROM` output,
  and `MOVE` statements updating sentinel flags.

### `samples/cobol/reverse_normalization.cob`
- **Purpose:** Exercises identifier, literal, and layout normalization paths so the reverse emitter can produce idiomatic CBL-C
  even from loosely formatted COBOL.
- **Constructs:** Hyphenated identifiers, lowercase working-storage names, string literals with mixed casing, numeric literals
  that include leading zeros, multiple procedure paragraphs, and STOP statements.

### `samples/cobol/reverse_control_flow.cob`
- **Purpose:** Expands the reverse fixtures with combined conditional branches and looping constructs so complex control flow
  sequencing stays stable under round-trip testing.
- **Constructs:** `IF NOT` conditionals, `PERFORM UNTIL` loops, `PERFORM VARYING` counters, sequential `MOVE` assignments, and
  STOP statements spread across multiple procedure paragraphs.

## Maintenance Checklist

1. Add a new `.cob` file under `samples/cobol` when introducing COBOL-side language features that need sample coverage.
2. Append the file path to `samples/cobol/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
