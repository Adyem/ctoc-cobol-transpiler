# Example Project: Hello Make Demo

This sample demonstrates how to use the CTOC COBOL transpiler from a
standalone makefile. The project converts a small CBL-C source into
COBOL and compiles the result with `cobc`.

## Layout

- `Makefile` — orchestrates the build by compiling the transpiler if it
  has not been created yet, running the transpiler, and invoking `cobc`.
- `hello_make_demo.cblc` — CBL-C entrypoint that displays a greeting.

## Usage

1. Run `make` inside this directory. The makefile will:
   - initialize the `libft` submodule if it has not been checked out,
   - attempt to install GnuCOBOL via the root `make install_cobc` target
     when `cobc` is missing,
   - build the transpiler if required, and
   - transpile the sample before compiling it with `cobc`.
   The makefile automatically appends `.exe` when running on Windows so
   both the transpiler binary and the sample executable resolve
   correctly.
2. Inspect the generated `hello_make_demo.cob` and
   `hello_make_demo{.exe}` artifacts that now live alongside the source
   so you can review the transpiled output directly.
3. Execute `make run` to launch the resulting binary.
4. Use `make distclean` to remove the generated artifacts and cached
   setup files if you want to reset the directory.

> **Note:** Installing GnuCOBOL requires package manager access. If the
> automatic installation fails, install it manually and rerun `make`.

If the transpiler exits successfully but does not produce the expected
COBOL file, the makefile aborts with an explanatory message so you can
inspect the transpiler logs before retrying.

The makefile keeps the sample isolated from the root build while still
reusing the shared transpiler binary and shared setup steps.
