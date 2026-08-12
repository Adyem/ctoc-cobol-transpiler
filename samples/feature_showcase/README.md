# CBL-C Feature Showcase

This sample builds one executable, `message_showcase`, from a set of imported
CBL-C translation units. It models a small insurance policy snapshot: text is
prepared, a ledger object calculates the report, claim batches are validated,
scores are adjusted, and the final report is displayed. The source
is intentionally ordinary application code; the language features are used as
part of that workflow rather than presented as disconnected demonstrations.

The workflow exercises imports, shared constants, structs, a class with
`private` / `public` members, constructor and destructor lifecycle code,
out-of-class methods, builtin `string` values and methods, arrays, pointers,
mutable and read-only references, reference-backed module-count updates, typed
exception handling for claim validation, and nested scopes.

## Layout

- `Makefile` — transpiles all imported CBL-C modules in one compiler
  invocation, emits the standard library catalog, compiles the required helper
  modules, and builds the single generated executable with the correct
  `COB_LIBRARY_PATH`.
- `message_showcase_main.cblc` — the small entrypoint that imports each
  showcase module and orchestrates the run.
- `message_showcase_constants.cblc` — shared string literals and constant
  values used by the text and ledger examples.
- `message_showcase_text.cblc` — a small `string` example with readable output.
- `message_showcase_ledger.cblc` — the policy snapshot workflow. It combines
  constructor initialization, destructor cleanup, out-of-class methods, structs,
  strings, exception-based claim validation, and a small malloc/free-backed
  scoring buffer.
- `message_showcase_memory.cblc` — a short array and pointer example.
- `message_showcase_refs.cblc` — a separate reference-parameter helper used by
  the entrypoint so both arguments are passed by reference.
- `message_showcase_main.cblc` — the entrypoint coordinates the workflow.
- `EXPECTED_OUTPUT.txt` — the transcript used by `make verify`.
- `stdlib/` — populated by the `standard-library` CLI direction.
- `cobol/message_showcase*.cob` — generated COBOL output.
- `message_showcase` — executable produced from the generated COBOL.

## Usage

1. Run `make ensure_environment` to build repo-level tools and install
   dependencies used by the showcase.
2. Run `make transpile` to regenerate COBOL from the imported CBL-C modules.
3. Run `make stdlib` to emit and compile standard-library helper modules.
4. Run `make compile` to build the single COBOL executable with `cobc`.
5. Run `make run` to execute `message_showcase`.
6. Run `make verify` to diff the transcript against
   `EXPECTED_OUTPUT.txt`.
7. Run `make clean` to remove generated COBOL, the executable, helper
   modules, emitted standard-library files, and environment sentinels.
