# Insurance Risk Snapshot Sample

This sample keeps the showcase small and focused while exercising the
newer CBL-C features that lower cleanly into COBOL. The three programs
demonstrate classes, structs, `private`/`public` access control,
`const`, and builtin `string` methods.

## Layout

- `Makefile` — transpiles the CBL-C programs, emits the standard
  library catalog, compiles helper modules, and runs the generated
  executables with the correct `COB_LIBRARY_PATH`.
- `message_showcase.cblc` — a class-based underwriting snapshot. It uses
  private state plus public methods, and builds the program title with
  `string.append(...)` / `string.len()`.
- `message_showcase_banner.cblc` — a struct-based status program. It
  uses grouped data, string field assignment, and string append to build
  a compact summary line.
- `message_showcase_metrics.cblc` — a metrics sample that combines
  `const` values, a class with computed accessors, and string helpers.
- `stdlib/` — populated by the `standard-library` CLI direction so the
  helper COBOL modules used by the showcase are emitted next to the
  sample.
- `*.cob` — generated COBOL output for each CBL-C source.
- `message_showcase*` — executables produced from the generated COBOL.

## Scenario Overview

1. `message_showcase.cblc` loads an underwriting snapshot through a
   class API and reports derived totals.
2. `message_showcase_banner.cblc` uses a struct to present active
   modules and a compact summary row.
3. `message_showcase_metrics.cblc` computes reserve and payout metrics
   from class methods and `const` inputs.
4. The sample still emits and compiles the COBOL standard-library
   helpers needed for the builtin string operations.

## Usage

1. Run `make ensure_environment` to install dependencies used by the
   showcase.
2. Run `make transpile` to regenerate COBOL sources from the CBL-C
   programs.
3. Run `make stdlib` to emit the standard library catalog and compile
   the helper modules required by the sample.
4. Run `make compile` to build the COBOL executables with `cobc`.
5. Run `make run` to execute the three showcase programs in sequence.
6. Run `make verify` to capture the showcase output in
   `actual_output.log` and diff it against `EXPECTED_OUTPUT.txt`.
7. Run `make clean` to remove generated COBOL, executables, helper
   modules, emitted standard library files, and environment sentinels.
