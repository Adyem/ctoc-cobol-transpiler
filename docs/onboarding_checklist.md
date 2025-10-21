# Developer Onboarding Checklist

This checklist helps new contributors install dependencies, set up the repository, and validate the toolchain before modifying the transpiler.

## Local Build
- [x] Generate optimized binaries with `make all` and debug builds with `make debug`.
- [x] Confirm artifacts are created in the project root (e.g., `ctoc_cobol_transpiler`).
- [x] Use `make clean` between configuration changes to remove stale objects (requires initialized `libft`).

## Test Execution
- [x] Build the automated test runner with `make tests`.
- [x] Execute `make test` to run the full suite (requires initialized `libft`).
- [x] Confirm `cobc` is installed by the toolchain step above or run `make install_cobc` if the binary is missing before executing COBOL-backed tests.
- [x] Run forward CBL-C→COBOL tests by default—the Makefile now exports `CTOC_ENABLE_FORWARD_TRANSLATION=1`; use `make test FORWARD_TRANSLATION=0` if you need to skip them temporarily.
- [x] Exercise the new CBL-C→C backend at least once (for example, `./ctoc_cobol_transpiler --direction cblc-to-c --input samples/cblc/minimal_program.cblc --output build/minimal_program.c`) to confirm helper-backed C emits successfully.
- [x] Investigate and resolve any failing tests before committing changes (current suite passes with `make test`).

## Documentation & Workflow
- [x] Read `docs/contributing.md` for coding standards and review policies.
- [x] Skim `docs/runtime_api_reference.md` to understand available runtime helpers.
- [x] Review `docs/getting_started.md` for an end-to-end tour of the toolchain and recent feature additions.
- [x] Update relevant guides when adding new features or modifying workflows (see the new IDE integration guide and VS Code grammar under `docs/ide_integration.md` and `editors/`).

### Coding Standards Highlights
- Follow 4-space indentation, Allman braces, and `snake_case` identifiers across C++ and runtime code.
- Avoid `for` loops, ternary operators, and `switch` statements in favor of `while`-driven control flow.
- Return from void functions using `return ;` and wrap non-void values in parentheses (`return (value);`).
- Keep class interfaces in headers and implementations in `.cpp` files, mirroring the pattern described in `docs/contributing.md`.

### Runtime Helper Primer
- `runtime_scalar` covers integer and character primitives, providing arithmetic, comparison, and formatting helpers that guard against overflow.
- `runtime_string` manages dynamic COBOL strings, including trimming, comparison, concatenation, and length-safe copy routines.
- `runtime_memory` and `runtime_sort` manipulate raw buffers and record sets without overrunning caller-provided storage.
- `runtime_csv`, `runtime_encoding`, and `runtime_collation` supply high-level integrations for structured text, EBCDIC ↔ ASCII transcoding, and customizable comparison semantics.
- The `CBLC-*` standard library subprograms expose COBOL entrypoints (e.g., `CBLC-STRLEN`, `CBLC-STRCPY`, `CBLC-STRNLEN`) that wrap the runtime helpers with caller-declared length safety.

Keep this checklist updated as the project evolves so newcomers can get productive quickly.
