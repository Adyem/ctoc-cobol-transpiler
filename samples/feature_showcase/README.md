# CBL-C Feature Showcase

This sample builds one executable, `message_showcase`, from one CBL-C
source file. The program is intentionally compact, but it exercises the
current CBL-C features that lower into COBOL cleanly: classes with
`private` / `public` members, constructors and initializer lists, `const`
values, structs and nested fields, arrays, functions without the old
`function` keyword, builtin `string` methods, folded `std::strlen(...)`
literal calls, and scalar pointer allocation / indexing / arithmetic.

## Layout

- `Makefile` — transpiles `message_showcase.cblc`, emits the standard
  library catalog, compiles the required helper modules, and builds the
  single generated executable with the correct `COB_LIBRARY_PATH`.
- `message_showcase.cblc` — the single source program for the showcase.
- `EXPECTED_OUTPUT.txt` — the transcript used by `make verify`.
- `stdlib/` — populated by the `standard-library` CLI direction.
- `message_showcase.cob` — generated COBOL output.
- `message_showcase` — executable produced from the generated COBOL.

## Usage

1. Run `make ensure_environment` to build repo-level tools and install
   dependencies used by the showcase.
2. Run `make transpile` to regenerate COBOL from the CBL-C program.
3. Run `make stdlib` to emit and compile standard-library helper modules.
4. Run `make compile` to build the single COBOL executable with `cobc`.
5. Run `make run` to execute `message_showcase`.
6. Run `make verify` to diff the transcript against
   `EXPECTED_OUTPUT.txt`.
7. Run `make clean` to remove generated COBOL, the executable, helper
   modules, emitted standard-library files, and environment sentinels.
