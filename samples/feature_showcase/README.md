# Insurance Risk Snapshot Sample

This sample reshapes the showcase project into an insurance-focused
snapshot that can be adapted for banking and risk-operations demos. It
exercises a multi-program workflow built from three CBL-C translation
units that are compiled to COBOL with the transpiler.

## Layout

- `Makefile` — orchestrates transpiling the CBL-C sources, emitting the
  standard-library catalog, compiling helper modules, exporting
  `COB_LIBRARY_PATH`, and running the programs in sequence.
- `message_showcase.cblc` — the underwriting snapshot. It walks through
  staged status updates, produces an audit banner, publishes key policy
  and claims metrics, and demonstrates repeated uses of `std::strcpy`
  and `std::strlen` to stage shared buffers before displaying them.
- `message_showcase_banner.cblc` — a companion program that highlights
  which insurance analytics modules are active, builds summary rows via
  `std::strcpy`, captures the latest summary length with `std::strlen`,
  and reports the module count.
- `message_showcase_metrics.cblc` — a focused metrics program that
  calculates loss ratio, reserve gaps, and average paid claim values
  while also constructing the trend note and recording its length with
  the standard-library helpers.
- `stdlib/` — populated by the `standard-library` CLI direction so the
  full catalog of COBOL helpers (plus compiled modules for
  `CBLC-ABS`, `CBLC-STRCAT`, `CBLC-STRCPY`, and `CBLC-STRLEN`) can be
  produced alongside the demo when desired.
- `STDLIB SHOWCASE` — running `make run` lists the curated helper source
  and module pairs so you can verify the COBOL and dynamically loadable
  artifacts were emitted next to the insurance workflow.
- `*.cob` — COBOL emitted by the transpiler for the CBL-C programs. They
  are regenerated during each build.
- Executables named `message_showcase*` — binaries produced from the
  generated COBOL programs.

## Scenario Overview

1. The underwriting snapshot program prints an operational header,
   syncs policy counters, copies data across shared buffers, and reports
   claims-driven metrics.
2. The banner program introduces department-specific stages and reports
   how many analytics modules are active.
3. The metrics program performs the loss-ratio and reserve calculations
   separately so totals can be reviewed in isolation.
4. Each program exercises the `std::strcpy` and `std::strlen` helpers so
   the emitted COBOL explicitly demonstrates how to integrate standard
   library calls (`CALL 'CBLC-STRCPY'`/`CALL 'CBLC-STRLEN'`) with
   translated workloads.
5. The transpiler emits the COBOL standard library catalog alongside the
   generated programs, showcasing how to package the helpers for
   insurance or banking workloads that need them.

## Usage

1. Run `make ensure_environment` to install dependencies used by the
   showcase (cobc and the transpiler binary).
2. Run `make transpile` to regenerate COBOL sources from the CBL-C
   programs.
3. Run `make stdlib` to emit the entire standard library catalog and
   compile the helper modules required by the sample.
4. Run `make compile` to build the COBOL executables with `cobc`. This
   target also depends on the helper modules so subsequent runs do not
   miss the standard-library calls.
5. Run `make run` to execute the three showcase programs in sequence.
   The recipe exports `COB_LIBRARY_PATH` so the compiled standard
   library helpers are visible to each program.
6. Run `make verify` to capture the showcase output in
   `actual_output.log` and diff it against `EXPECTED_OUTPUT.txt` for a
   quick regression check.
7. Run `make clean` to remove generated COBOL, executables, helper
   modules, the emitted standard library, and environment sentinels when
   you are finished testing.

## Expected Output

After executing the compiled programs in sequence, compare the console
output against `EXPECTED_OUTPUT.txt` (or use `make verify`) to confirm
the executables produced by the transpiler and standard-library emission
match the validated sample results.
