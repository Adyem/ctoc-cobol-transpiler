# Fuzzing the COBOL → CBL-C Pipeline

The repository ships with a lightweight fuzzing harness that exercises the COBOL lexer and parser using a mix of randomly
generated programs and mutations derived from the sample corpus. Use the helper script to uncover crashers before they reach
continuous integration or production workloads.

## Quick Start

Build the transpiler and run the default fuzzing mix with:

```
make fuzz
```

This command generates the release binary (if it is not already present) and launches `scripts/fuzz_transpiler.py` with 50
iterations per strategy. The script runs two complementary fuzzers:

- **Grammar fuzzer:** emits short COBOL programs that follow a constrained grammar and randomize identifiers, picture clauses,
  and simple statements (`MOVE`, `DISPLAY`, etc.).
- **Mutation fuzzer:** chooses an existing sample from `samples/cobol/` and applies random insertions, deletions, and keyword
  replacements to stress error-recovery paths.

Both fuzzers invoke the transpiler with `--direction cobol-to-cblc`, capturing diagnostics so the harness can distinguish
expected parser errors from hard crashes.

## Customization

Control the fuzzing behaviour with the following Make variables:

- `FUZZ_ITERATIONS` – number of iterations per mode (default: `50`).
- `FUZZ_MODE` – choose a single strategy (`grammar` or `mutation`) or run both with `all` (default: `all`).
- `FUZZ_ARGS` – extra arguments forwarded verbatim to the Python harness (for example, `--seed 123 --keep-failures`).

Example:

```
make fuzz FUZZ_ITERATIONS=200 FUZZ_MODE=mutation FUZZ_ARGS="--seed 7 --timeout 3"
```

You can also call the script directly:

```
python3 scripts/fuzz_transpiler.py --iterations 100 --mode grammar --seed 42
```

Pass `--keep-failures` to retain crashing inputs under `build/fuzz_failures/` alongside the stdout/stderr logs. The directory is
created on demand.

## When to Run

- After touching lexer or parser code.
- When adjusting COBOL syntax recovery, error handling, or CLI option parsing.
- Before shipping changes to the COBOL samples bundled with the repository.

Recording seeds for problematic inputs allows deterministic reproduction:

```
python3 scripts/fuzz_transpiler.py --mode mutation --iterations 1 --seed 917
```

A non-zero exit code of 139 (or any negative return code) indicates the transpiler crashed. Investigate the preserved fixture in
`build/fuzz_failures/` and add a regression test once fixed.
