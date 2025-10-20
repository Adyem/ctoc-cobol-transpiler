# Continuous Integration Workflow

This project provides GNU Make targets that mirror the checks run by our continuous integration configuration. Developers should
invoke these targets locally before sending changes for review to ensure the same coverage that CI enforces.

## Primary Entry Point

Run the aggregate pipeline with:

```
make ci
```

This command performs a clean rebuild, executes the automated test suite, and runs repository lint checks.

## Build Verification

The build stage is exposed through:

```
make ci-build
```

This target invokes `make fclean` to clear previous outputs, builds the optimized release configuration (`OPT_LEVEL=2`), and
builds the debug configuration via `make debug`. Both outputs must succeed for the target to pass.

> **Note:** Builds default to reproducible outputs. The Makefile exports `SOURCE_DATE_EPOCH` and applies GCC/GNU Make prefix-map
> flags so binaries omit absolute paths and timestamps. Set `REPRODUCIBLE=0` on the command line if you need to inspect local
> debug paths or embed environment-specific metadata during troubleshooting.

## Test Execution

Execute the full automated test suite with:

```
make ci-test
```

This wraps the existing `make test` flow to maintain consistent naming with the other CI actions.

## Lint Checks

Run repository linting with:

```
make ci-lint
```

This delegates to the `lint` target, which launches `scripts/lint_sources.py`. The script scans project sources and
documentation for trailing whitespace or tab characters, failing if any violations are detected.

## Fuzzing Smoke Tests

Before merging parser or lexer changes, run the lightweight fuzzing harness:

```
make fuzz
```

The target builds the release binary (if needed) and invokes `scripts/fuzz_transpiler.py` to run both grammar-based and
mutation-based fuzzers against the COBOL → CBL-C pipeline. See [`docs/fuzzing.md`](fuzzing.md) for customization options and
workflow guidance.

## Coverage Enforcement

Continuous integration enforces minimum coverage thresholds to guard against regressions in test depth. Run the dedicated target
locally before pushing changes:

```
make coverage
```

This rule rebuilds the test suite with coverage instrumentation, executes the tests, and then launches
`scripts/coverage_report.py`. The helper aggregates `gcov` results for the production sources (excluding the test harness) and
fails if the aggregate line coverage drops below 60% or the branch coverage drops below 65%. Adjust the thresholds during
experimentation by passing `COVERAGE_LINE_THRESHOLD` or `COVERAGE_BRANCH_THRESHOLD` on the command line. Use
`COVERAGE=1` with other build targets when you need to inspect raw `.gcda` files or run the coverage script manually.
