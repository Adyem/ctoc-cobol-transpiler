# CBL-C Feature Showcase

This sample builds one executable, `message_showcase`, from a few imported
CBL-C source files. It models a small renewal-risk report while exercising the
current CBL-C features that lower into COBOL cleanly: classes with
`private` / `public` members, constructors and initializer lists, `const`
values, structs and nested fields, imports, arrays, functions without the old
`function` keyword, builtin `string` construction / assignment / methods,
folded `std::strlen(...)` literal calls, and scalar pointer allocation /
indexing / arithmetic.

## Layout

- `Makefile` — transpiles all imported CBL-C modules in one compiler
  invocation, emits the standard library catalog, compiles the required helper
  modules, and builds the single generated executable with the correct
  `COB_LIBRARY_PATH`.
- `message_showcase.cblc` — the small entrypoint that imports each showcase
  module.
- `message_showcase_text.cblc` — builtin `string` and folded
  `std::strlen(...)` examples.
- `message_showcase_portfolio.cblc` — structs, nested fields, classes,
  constructors, `const`, methods, and imported parameter flow.
- `message_showcase_memory.cblc` — arrays and pointer allocation/indexing.
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
