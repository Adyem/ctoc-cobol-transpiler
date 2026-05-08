# CBL-C Feature Showcase

This sample builds one executable, `message_showcase`, from a few imported
CBL-C source files. It is meant to be a compact, readable starting point for
the language rather than a long business demo. The sample still exercises the
core features that lower cleanly into COBOL: imports, structs, a class with
`private` / `public` members, constructor and destructor lifecycle code,
out-of-class method bodies, builtin `string` values and methods, arrays, and
basic pointer access.

## Layout

- `Makefile` — transpiles all imported CBL-C modules in one compiler
  invocation, emits the standard library catalog, compiles the required helper
  modules, and builds the single generated executable with the correct
  `COB_LIBRARY_PATH`.
- `message_showcase.cblc` — the small entrypoint that imports each showcase
  module.
- `message_showcase_text.cblc` — a small `string` example with readable output.
- `message_showcase_ledger.cblc` — the expanded class example. It combines
  constructor initialization, destructor cleanup, out-of-class methods, structs,
  strings, and a small malloc/free-backed scoring buffer.
- `message_showcase_memory.cblc` — a short array and pointer example.
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
