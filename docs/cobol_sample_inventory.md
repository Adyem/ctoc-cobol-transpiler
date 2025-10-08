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
- **Constructs:** `IF NOT` conditionals, `PERFORM UNTIL` loops that update sentinel values through `MOVE`, `PERFORM VARYING`
  counters, sequential `MOVE` assignments, numeric `DISPLAY` output, and STOP statements spread across multiple procedure
  paragraphs.

### `samples/cobol/return_numeric.cob`
- **Purpose:** Demonstrates value-returning helpers expressed as separate COBOL subprograms that accept arguments by reference
  and populate a trailing return slot.
- **Constructs:** `CALL` statements with `USING BY REFERENCE` arguments, subprogram definitions with `LINKAGE SECTION`
  parameters, arithmetic implemented through `COMPUTE`, and `GOBACK` exits for the invoked routine.

### `samples/cobol/return_boolean.cob`
- **Purpose:** Captures conditional return logic routed through a referenced slot so boolean results can surface alongside other
  state updates.
- **Constructs:** Multi-argument `CALL` sequences, remainder-driven `DIVIDE` operations, conditional `MOVE` assignments that set
  flag bytes, and subprogram linkages that mirror the return buffer contract.

### `samples/cobol/return_character.cob`
- **Purpose:** Provides a character-oriented example where the subprogram writes to the trailing buffer so callers can reuse
  their own storage for results.
- **Constructs:** Single-argument `CALL` statements, working-storage defaults that seed returned values, linkage section buffer
  declarations, and `MOVE` statements that copy data into the supplied return slot.

### `samples/cobol/multi_module_main.cob`
- **Purpose:** Illustrates the forward pipeline's multi-file support where the primary program calls into a helper emitted from another translation unit.
- **Constructs:** Working-storage counters, `CALL` statements targeting external subprograms, `PERFORM`ed helper paragraphs, arithmetic updates, and formatted numeric displays.

### `samples/cobol/multi_module_worker.cob`
- **Purpose:** Supplies the companion subprogram that receives calls from the multi-module main routine so the CLI can emit multiple COBOL artifacts in a single run.
- **Constructs:** Standalone program IDs, literal-initialized working-storage messages, console output, and `GOBACK` termination for callable modules.

### `samples/cobol/numeric_precision.cob`
- **Purpose:** Anchors the widened numeric arithmetic and comparison behavior expected from the forward compiler across `LONG`, `COMP-1`, and `COMP-2` analogs.
- **Constructs:** Signed `PIC S9(9)` and `PIC S9(12)` integers, scaled decimal (`S9V9(4)`) fields, `COMPUTE` statements combining addition, subtraction, and multiplication, relational comparisons (`>=`, `>`, `=`, `NOT =`), and diagnostic `DISPLAY` calls for each branch.

### `samples/cobol/integration_showcase.cob`
- **Purpose:** Serves as an end-to-end scenario that exercises the current forward pipeline by reading transactions, partitioning them into accepted and rejected logs, and reporting summary totals.
- **Constructs:** Multiple file descriptors, sequential `READ` with `AT END` handling, arithmetic `ADD` updates, structured `IF`/`ELSE` blocks, repeated `PERFORM` paragraphs, and formatted `DISPLAY` output.


## Maintenance Checklist

1. Add a new `.cob` file under `samples/cobol` when introducing COBOL-side language features that need sample coverage.
2. Append the file path to `samples/cobol/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
