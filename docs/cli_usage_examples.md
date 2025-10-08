# CLI Usage Examples

The `ctoc_cobol_transpiler` binary provides a single entry point for converting between CBL-C and COBOL source files. This guide collects ready-to-run invocations that illustrate the required flags, optional formatting controls, and diagnostics knobs exercised by the command-line interface.

## Required options

Every run must specify the transformation direction alongside at least one input and output path:

```
ctoc_cobol_transpiler --direction cblc-to-cobol --input samples/cblc/copy_file.cblc --output build/copy_file.cob
```

* `--direction` accepts `cblc-to-cobol` or `cobol-to-cblc` and determines which frontend/parser and backend/emitter pair the pipeline assembles.
* `--input` points at the source file that the pipeline reads. Repeat the flag to queue additional translation units.
* `--output` names the artifact written by the code generator. Provide one `--output` per input so the files can be emitted side by side.

When converting several modules in a single invocation, pair each input with a matching output in the order they appear:

```
ctoc_cobol_transpiler --direction cblc-to-cobol \
  --input src/records.cblc --output build/records.cob \
  --input src/formatters.cblc --output build/formatters.cob
```

The tool rejects mismatched counts to ensure every generated COBOL file has a designated destination.

## Optional layout and destination controls

```
ctoc_cobol_transpiler --direction cobol-to-cblc --input samples/cobol/copy_file.cob --output build/copy_file.cblc --output-dir dist --format pretty
```

* `--output-dir` overrides the directory that hosts generated files. When omitted, the tool writes next to the provided `--output` path.
* `--format` accepts `default`, `minimal`, or `pretty` to toggle whitespace and alignment policies in emitted COBOL.

## Diagnostics and help

```
ctoc_cobol_transpiler --direction cblc-to-cobol --input input.cblc --output output.cob --diagnostics verbose
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
export CTOC_DEFAULT_DIRECTION=cblc-to-cobol
ctoc_cobol_transpiler --input samples/cblc/record_writer.cblc --output build/record_writer.cob
```

Unset or change the variable to swap the default compilation direction.

## End-to-end sample workflow

1. Choose a staging directory for generated files:
   ```
   mkdir -p build
   ```
2. Transpile the sample program while enabling verbose diagnostics:
   ```
   ctoc_cobol_transpiler --direction cblc-to-cobol --input samples/cblc/filter_prefix.cblc --output build/filter_prefix.cob --diagnostics verbose
   ```
3. Inspect the resulting COBOL file and review the emitted diagnostics to confirm the pipeline completed successfully.

Refer to `design_doc.txt` §6 for architectural notes on the CLI and related tooling expectations.
