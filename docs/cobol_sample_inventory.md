# COBOL Sample Inventory

This catalog enumerates the COBOL fixtures that drive validation of the reverse pipeline. Most programs live in
`samples/cobol` and are registered in `samples/cobol/manifest.txt` so automated checks can keep the documentation and
filesystem synchronized. The quick-start example pairs its COBOL artifact with the accompanying makefile in
`samples/example_project` so the entire demo fits inside a single directory.

## Sample Coverage

### `samples/cobol/minimal_program.cob`
- **Purpose:** Supplies a compact program that exercises the parser and reverse emitter without relying on file I/O constructs.
- **Constructs:** Minimal division headers, a single WORKING-STORAGE scalar, sequential `MOVE` statements, and a `STOP RUN` terminator.

### `samples/example_project/hello_make_demo.cob`
- **Purpose:** Serves as the COBOL counterpart to the example project greeting demo so both directions of the pipeline share a reference implementation.
- **Constructs:** WORKING-STORAGE scalar buffer, literal `MOVE`, `DISPLAY`, and `STOP RUN` terminator.

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
- **Constructs:** Level-01 flag and record buffers, `PERFORM UNTIL` loops, `READ ... INTO` file operations, guarded `IF`/`ELSE`
  blocks, `WRITE ... FROM` output, and `MOVE` statements updating sentinel flags.

### `samples/cobol/reverse_normalization.cob`
- **Purpose:** Exercises identifier, literal, and layout normalization paths so the reverse emitter can produce idiomatic CBL-C
  even from loosely formatted COBOL.
- **Constructs:** Hyphenated identifiers, lowercase working-storage names that normalize into flag/numeric/buffer declarations,
  string literals with mixed casing, numeric literals that include leading zeros, multiple procedure paragraphs, and STOP
  statements.

### `samples/cobol/reverse_control_flow.cob`
- **Purpose:** Expands the reverse fixtures with combined conditional branches and looping constructs so complex control flow
  sequencing stays stable under round-trip testing.
- **Constructs:** Level-01 flag and counter storage, `IF NOT` conditionals, `PERFORM UNTIL` loops that update sentinel values
  through `MOVE`, `PERFORM VARYING` counters, sequential `MOVE` assignments, numeric `DISPLAY` output, and STOP statements
  spread across multiple procedure paragraphs.

### `samples/cobol/reverse_numeric_scalars.cob`
- **Purpose:** Introduces decimal WORKING-STORAGE items and additional boolean suffixes so the reverse emitter can recover
  floating-point variables and broaden flag detection beyond `-FLAG`.
- **Constructs:** Level-01 `PIC S9V9(4)` and `PIC S9(12)V9(6)` fields, display-mode numeric counters, status bytes ending in
  `-IND`, literal `MOVE` statements, and `STOP RUN` termination.

### `samples/cobol/reverse_numeric_widths.cob`
- **Purpose:** Adds wide integer and scaled decimal `PIC` clauses so the reverse pipeline emits canonical `long`, `long long`,
  `float`, and `double` declarations.
- **Constructs:** Level-01 `PIC S9(12)` and `PIC S9(19)` items with matching assignments, literal `MOVE` statements that drive
  widened integer and floating declarations, and a terminating `STOP RUN.` paragraph.

### `samples/cobol/reverse_group_items.cob`
- **Purpose:** Validates that a level-01 group without a PIC clause survives reverse translation by materializing a record
  declaration and mirrored scalars in the generated CBL-C.
- **Constructs:** A group containing alphanumeric and decimal children, a separate status byte with a VALUE default, literal
  `MOVE` statements targeting the group members, and a closing `STOP RUN.` paragraph.

### `samples/cobol/reverse_value_defaults.cob`
- **Purpose:** Exercises VALUE clauses on supported scalars so the reverse emitter can recover default-initialized booleans,
  numerics, and text buffers directly into CBL-C.
- **Constructs:** Level-01 flag, integer, and decimal items with VALUE defaults, an alphanumeric buffer populated through a
  literal VALUE clause, and a bare `STOP RUN` procedure that leaves the recovered storage untouched.

### `samples/cobol/reverse_string_length_metadata.cob`
- **Purpose:** Captures a callee that advertises a narrow `PIC X(5)` buffer while the caller provides a wider region so the
  reverse pipeline can honor the recorded caller length when regenerating CBL-C declarations.
- **Constructs:** Level-01 alphanumeric item with a `PIC X(5)` clause, literal `MOVE` into the buffer, and a terminating `STOP
  RUN.` paragraph.

### `samples/cobol/reverse_copybook.cob`
- **Purpose:** Extends the reverse fixtures with a `COPY` directive and character heuristics so shared working-storage and
  single-byte codes translate into the right CBL-C surface area.
- **Constructs:** Simple `COPY` include resolved through the registered copybook table, a single-character code that remains a
  scalar, buffer identifiers that keep their array form, and a minimal `STOP RUN` procedure body.

### `samples/cobol/reverse_comments.cob`
- **Purpose:** Verifies that inline `*>` comments in WORKING-STORAGE and PROCEDURE DIVISION survive lexing so the reverse
  pipeline can re-emit them near the reconstructed statements.
- **Constructs:** Consecutive comment lines annotating level-01 declarations, inline trailing comments after `MOVE`
  statements, paragraph header notes, and straightforward `DISPLAY`/`STOP RUN` control flow that keeps the focus on comment
  preservation.

### `samples/cobol/reverse_comment_paragraphs.cob`
- **Purpose:** Exercises the comment iterator across paragraph boundaries so remarks that bracket nested statements surface in
  the regenerated CBL-C at the correct indentation level.
- **Constructs:** 77-level boolean declarations with defaults, inline `MOVE` commentary, conditional `IF` blocks containing
  annotated statements, and paragraph transitions that include remarks between routines.

### `samples/cobol/reverse_comment_inline_control.cob`
- **Purpose:** Verifies that inline remarks placed immediately before COBOL statements survive translation when they appear ahead of control flow constructs.
- **Constructs:** Boolean status flag storage, comments that precede `IF` branching and nested statements within both branches, literal `DISPLAY` calls, and a trailing remark ahead of the terminating `STOP RUN`.

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

### `samples/cobol/project_scheduler/project_scheduler_loader.cob`
- **Purpose:** Mirrors the backlog loader in the multi-file CBL-C project by converting string durations into a numeric total with an optional phase adjustment.
- **Constructs:** `FUNCTION NUMVAL` conversions, string working-storage slots, conditional `ADD` updates, linkage parameters for returning computed totals, and `GOBACK` termination for reuse as a subprogram.

### `samples/cobol/project_scheduler/project_scheduler_metrics.cob`
- **Purpose:** Implements the prioritization math for the scheduler sample so the COBOL output demonstrates floating-point arithmetic and intrinsic function usage.
- **Constructs:** `COMPUTE` statements targeting `PIC S9V9(4)` accumulators, `FUNCTION ABS`, `FUNCTION SQRT`, literal string metadata, `BY VALUE`/`BY REFERENCE` linkage parameters, and `GOBACK` flow control.

### `samples/cobol/project_scheduler/project_scheduler_presenter.cob`
- **Purpose:** Coordinates calls into the loader and metrics subprograms while assembling a concatenated status line that echoes the multi-file control flow.
- **Constructs:** External `CALL` statements, `STRING` concatenation, equality checks against alphanumeric fields, formatted numeric displays, and repeated `GOBACK` termination.

### `samples/cobol/project_scheduler/project_scheduler_main.cob`
- **Purpose:** Provides the entrypoint program that kicks off the presenter helper so the full multi-module workflow can be executed from a single invocation.
- **Constructs:** Minimal working-storage, `CALL` into the presenter subprogram, and `STOP RUN` termination.

### `samples/cobol/abs_magnitude_report.cob`
- **Purpose:** Highlights the intrinsic `FUNCTION ABS` usage on signed totals so the forward compiler can validate unary absolute-value expressions on numeric and floating targets.
- **Constructs:** Signed integer and scaled-decimal working-storage items, `COMPUTE` statements that subtract and apply `FUNCTION ABS`, literal initialization of deltas, and `DISPLAY` calls that report raw and absolute magnitudes.

### `samples/cobol/numeric_precision.cob`
- **Purpose:** Anchors the widened numeric arithmetic and comparison behavior expected from the forward compiler across `LONG`, `COMP-1`, and `COMP-2` analogs.
- **Constructs:** Signed `PIC S9(9)` and `PIC S9(12)` integers, scaled decimal (`S9V9(4)`) fields, `COMPUTE` statements combining addition, subtraction, and multiplication, relational comparisons (`>=`, `>`, `=`, `NOT =`), and diagnostic `DISPLAY` calls for each branch.

### `samples/cobol/integration_showcase.cob`
- **Purpose:** Serves as an end-to-end scenario that exercises the current forward pipeline by reading transactions, partitioning them into accepted and rejected logs, and reporting summary totals.
- **Constructs:** Multiple file descriptors, sequential `READ` with `AT END` handling, arithmetic `ADD` updates, structured `IF`/`ELSE` blocks, repeated `PERFORM` paragraphs, and formatted `DISPLAY` output.


### `samples/cobol/floating_point_mix.cob`
- **Purpose:** Mirrors the floating-point CBL-C sample with COMP-1 style calculations that blend seasonal readings and yearly projections.
- **Constructs:** `COMPUTE` statements combining decimal values, boolean-style flag bytes represented as characters, and formatted `DISPLAY` output for intermediate totals.

### `samples/cobol/mixed_numeric_types.cob`
- **Purpose:** Provides integer-width coverage matching the mixed numeric CBL-C example so signed accumulators at multiple scales have COBOL fixtures.
- **Constructs:** Signed `PIC S9(n)` fields at varying lengths, additive `COMPUTE` logic, conditional flag updates, and numeric `DISPLAY` buffers.

### `samples/cobol/textual_priority_mix.cob`
- **Purpose:** Demonstrates combining textual buffers with numeric counters and flag bytes to track scheduling decisions.
- **Constructs:** Alphanumeric working-storage items, nested `IF` statements governing flag updates, and sequential `DISPLAY` operations for strings, characters, and zero-padded numbers.

## Maintenance Checklist

1. Add a new `.cob` file under `samples/cobol` when introducing COBOL-side language features that need sample coverage.
2. Append the file path to `samples/cobol/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
