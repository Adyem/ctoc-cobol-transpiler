# Feature Showcase Project

This sample exercises a multi-program workflow built from three CBL-C
translation units and a COBOL disk I/O helper. The Makefile transpiles
all CBL-C sources, emits the COBOL standard library catalog for
reference, and compiles every program so the executables can be run in
sequence.

## Layout

- `Makefile` — orchestrates the build. It transpiles the three CBL-C
  programs, generates the full standard library into the `stdlib/`
  directory, seeds `showcase_transactions.dat`, compiles all programs,
  and exposes a `make run` target that executes the generated
  executables. Cleanup removes the generated COBOL sources, executables,
  seeded disk files, and the emitted standard library catalog.
- `message_showcase.cblc` — the original single-module walkthrough that
  performs staged string and numeric operations.
- `message_showcase_banner.cblc` — a companion program that focuses on
  banner updates and displays module-specific status information.
- `message_showcase_metrics.cblc` — a companion program that performs
  the arithmetic progression independently so the totals can be
  reviewed.
- `message_showcase_io.cob` — COBOL disk I/O sample derived from the
  integration showcase. It reads seeded transactions, writes success and
  failure logs, and reports aggregate metrics.
- `stdlib/` — populated by the `standard-library` CLI direction so the
  full catalog of COBOL helpers is produced alongside the demo when
  `make` runs.
- `showcase_transactions.dat` / `showcase_success.txt` /
  `showcase_failure.txt` — seeded and generated files used by the I/O
  helper. All are removed by `make clean`.
- `*.cob` — COBOL emitted by the transpiler for the CBL-C programs. They
  are regenerated during each build.
- Executables named `message_showcase*` — binaries produced from the
  generated COBOL programs.

## Scenario Overview

1. The main showcase program reproduces the staged walkthrough from the
   original sample.
2. The banner program introduces its own stages and reports how many
   helper steps executed.
3. The metrics program performs the arithmetic progression separately so
   the totals can be verified in isolation.
4. The Makefile emits the COBOL standard library catalog alongside the
   transpiled programs, showcasing how to package the helpers for
   projects that need them.
5. The COBOL I/O helper prepares an output log, reads the seeded disk
   input, reports how many lines were processed, and writes categorized
   results to disk.

## Usage

1. Run `make transpile` to regenerate the COBOL sources for the CBL-C
   programs.
2. Run `make stdlib` if you want to emit just the standard library
   catalog without rebuilding the executables.
3. Run `make` (or `make compile`) to transpile, generate the standard
   library, and build the executables in one step.
4. Run `make run` to seed the disk input, execute every program, and
   view the staged output.
5. Run `make clean` to delete generated COBOL, executables, seeded files,
   and the standard library catalog.

The makefile keeps the same environment bootstrapping helpers as the
other samples so the transpiler and `cobc` toolchain are ready when the
showcase is built.

## Expected Output

After running `make run`, compare the console output against
`EXPECTED_OUTPUT.txt` to confirm the executables produced by the
transpiler and standard-library emission match the validated sample
results.
