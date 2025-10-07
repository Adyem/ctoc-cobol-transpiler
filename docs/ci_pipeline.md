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
