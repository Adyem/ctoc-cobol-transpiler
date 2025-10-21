# CLI Usage Examples

The `ctoc_cobol_transpiler` binary provides a single entry point for converting between CBL-C and COBOL source files and for generating the bundled standard library programs. This guide collects ready-to-run invocations that illustrate the required flags, optional formatting controls, diagnostics knobs, and library build workflows exercised by the command-line interface.

## Required options

Translation runs must specify the transformation direction alongside at least one input and output path:

```
ctoc_cobol_transpiler --direction cobol-to-cblc --input samples/cobol/copy_file.cob --output build/copy_file.cblc
```

* `--direction` accepts `cblc-to-cobol`, `cblc-to-c`, `cobol-to-cblc`, or `standard-library` and determines which pipeline the tool assembles. Each conversion path selects the appropriate frontend/parser and backend/emitter pair. The reverse path now lifts level 01/77 scalars (`PIC X(n)`, `PIC 9(n)`, and decimal patterns such as `PIC S9V9(n)`/`USAGE COMP-2`) into CBL-C declarations alongside the paragraph bodies reconstructed from `MOVE`, `IF`, `PERFORM`, file I/O, and `STOP` statements while distinguishing flag suffixes from single-character buffers, re-emitting registered `COPY` includes as explicit `copy` directives, and preserving VALUE clause defaults on those recovered scalars. Level 01 group items without PIC clauses emit `record` definitions with mirrored group variables and subordinate fields so structured data survives the round-trip. The CBL-C→C backend emits portable C with helper functions that mirror the COBOL runtime so you can diff behavior against native builds without a COBOL toolchain.
* `--input` points at the source file that the pipeline reads. Repeat the flag to queue additional translation units.
* `--output` names the artifact written by the code generator. Provide one `--output` per input so the files can be emitted side by side.

When converting several modules in a single invocation, pair each input with a matching output in the order they appear:

```
ctoc_cobol_transpiler --direction cobol-to-cblc \
  --input src/records.cob --output build/records.cblc \
  --input src/formatters.cob --output build/formatters.cblc
```

The tool rejects mismatched counts to ensure every generated COBOL file has a designated destination.

To emit portable C instead of COBOL, switch the direction while keeping the same input/output pairing:

```
ctoc_cobol_transpiler --direction cblc-to-c \
  --input src/runtime.cblc --output build/runtime.c
```

## Building the standard library

Use the `standard-library` direction to emit every cataloged helper as an individual COBOL program. The generator writes each routine to the current directory unless `--output-dir` redirects the artifacts:

```
ctoc_cobol_transpiler --direction standard-library --output-dir build/std
```

This mode ignores `--input` and `--output` flags, iterates across the registry returned by `transpiler_standard_library_get_entries`, and produces `<PROGRAM-ID>.cob` files for distribution alongside translated projects.

## Optional layout and destination controls

```
ctoc_cobol_transpiler --direction cobol-to-cblc --input samples/cobol/copy_file.cob --output build/copy_file.cblc --output-dir dist --format pretty --layout normalize
```

* `--output-dir` overrides the directory that hosts generated files. When omitted, the tool writes next to the provided `--output` path.
* `--format` accepts `default`, `minimal`, or `pretty` to toggle whitespace and alignment policies for COBOL emission. `pretty` feeds generated COBOL through the canonical formatter so braces, indentation, and operator spacing remain consistent across runs.
* `--layout` accepts `normalize` or `preserve` to control the regenerated CBL-C layout when translating from COBOL. `normalize` routes output through the formatter, while `preserve` copies the recovered structure verbatim so existing spacing can be reviewed without modification.

## AST visualization exports

```
ctoc_cobol_transpiler --direction cobol-to-cblc --input samples/cobol/copy_file.cob --output build/copy_file.cblc --dump-ast build/ast_graphs
```

* `--dump-ast <auto|directory>` enables Graphviz `.dot` generation for each parsed translation unit. Pass a directory to collect the graphs in a dedicated folder or `auto` to place the visualization alongside the emitted source file with a `.dot` suffix.
* Each node label includes the AST node kind plus the captured token lexeme (when available) so you can trace COBOL statements through parsing and semantic analysis when debugging pipeline changes.

## Diagnostics and help

```
ctoc_cobol_transpiler --direction cobol-to-cblc --input input.cob --output output.cblc --diagnostics verbose
```

* `--diagnostics` switches the verbosity tier: `silent` hides non-fatal notes, `normal` shows errors and warnings, and `verbose` streams per-stage logging.
* `--warnings-as-errors` elevates every warning to an error so strict builds fail fast when potential issues surface.
* `--help` prints a summary of options and exits successfully without running the pipeline.

### Interpreting reported issues

The parser tracks recoverable statement errors and resumes scanning at the next statement or paragraph boundary. You will see diagnostics for every invalid statement in one run instead of only the first failure.【F:parser.cpp†L165-L204】【F:parser.cpp†L553-L607】

Semantic analysis records string widths and emits `TRANSPILE_ERROR_SEMANTIC_STRING_TRUNCATION` when a MOVE literal or identifier exceeds the target buffer so you can adjust the field length before retrying the build.【F:transpiler_semantics.cpp†L562-L601】

When compiling multiple translation units, the transpiler context rejects subprogram registrations that would narrow an alphanumeric parameter. The CLI surfaces `TRANSPILE_ERROR_DATA_ITEM_PARAMETER_TRUNCATION` in that situation to guard against inadvertent truncation across module boundaries.【F:transpiler_context.cpp†L1143-L1183】

## Environment variable shortcut

The `CTOC_DEFAULT_DIRECTION` environment variable supplies a fallback value for `--direction`. When exported, the flag becomes optional:

```
export CTOC_DEFAULT_DIRECTION=cobol-to-cblc
ctoc_cobol_transpiler --input samples/cobol/record_writer.cob --output build/record_writer.cblc
```

Unset or change the variable to swap the default compilation direction.

### Controlling parallel compilation throughput

The CBL-C→COBOL pipeline emits each module in parallel. By default it launches one worker per available hardware thread and
caps the pool at the number of translation units scheduled for emission. Override the default by exporting `CTOC_PARALLELISM`
with a positive integer to force a specific worker count (set the value to `1` to fall back to sequential generation during
debugging):

```
export CTOC_PARALLELISM=4
ctoc_cobol_transpiler --direction cblc-to-cobol --input src/runtime.cblc --output build/runtime.cob
```

Each worker validates the generated COBOL locally; failures surface through the usual diagnostic stream, and the CLI retains the
same deterministic output order regardless of the pool size.

## End-to-end sample workflow

1. Transpile the sample program while enabling verbose diagnostics (the CLI creates missing output directories automatically):
   ```
   ctoc_cobol_transpiler --direction cobol-to-cblc --input samples/cobol/minimal_program.cob --output build/minimal_program.cblc --diagnostics verbose
   ```
2. Inspect the resulting CBL-C file and review the emitted diagnostics to confirm the pipeline completed successfully:
   ```
   cat build/minimal_program.cblc
   ```
   The minimal sample produces a short entrypoint so you can quickly verify the translation:
   ```cblc
   char RESULT[3];

   function void MAIN()
   {
       RESULT = "42";
       RESULT = RESULT;
       return;
   }
   ```

Refer to `design_doc.txt` §6 for architectural notes on the CLI and related tooling expectations.
