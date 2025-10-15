# CBL-C ↔ COBOL Transpiler

The CTOC COBOL Transpiler lets you author business logic in a lightweight C-style language called **CBL-C** and emit standard-compliant COBOL that can be compiled with GnuCOBOL. It can also ingest COBOL that falls within the supported dialect and regenerate the equivalent CBL-C so teams can iterate in whichever syntax they prefer.

## Key Capabilities

- Convert between `.cblc` and `.cob` sources while preserving file I/O declarations, record layouts, and procedure structure.
- Track COBOL picture clauses for integers and floating-point numbers, producing consistent type descriptors across translation units.
- Recover from parser errors to report multiple syntax issues in one run, and emit semantic diagnostics when alphanumeric data would be truncated or when subprogram bindings narrow parameters.
- Capture bidirectional source maps so diagnostics and debugging tools can jump between generated COBOL and originating CBL-C statements.
- Package reusable helpers in separate translation units and share data through explicit function parameters, mirroring the way you would compose modules in C.

## Quick Start

1. Prepare the toolchain and bundled runtime:
   ```
   make initialize
   make all
   make test
   ```
2. Transpile a sample COBOL program and inspect the generated CBL-C:
   ```
   ./ctoc_cobol_transpiler --direction cobol-to-cblc \
       --input samples/cobol/minimal_program.cob \
       --output build/minimal_program.cblc \
       --diagnostics verbose
   ```
   The CLI creates the `build/` directory automatically when it does not already exist, so you can point `--output` at a fresh staging path.
3. View the emitted CBL-C (or feed it through `cblc_formatter` for pretty-printing):
   ```
   cat build/minimal_program.cblc
   ```

Additional walkthroughs live in [`docs/getting_started.md`](docs/getting_started.md), while [`docs/cli_usage_examples.md`](docs/cli_usage_examples.md) catalogues common flag combinations.

> **Note:** The CLI currently supports the COBOL → CBL-C direction. Attempting to run with `--direction cblc-to-cobol` reports a diagnostic until the forward pipeline is implemented.

## Example: Filtering Lines in CBL-C

The minimal quick start sample produces a tiny CBL-C entrypoint, but more
interesting COBOL modules translate cleanly as well. For example, the filter
pipeline below originates from `samples/cobol/filter_prefix.cob`:

```cblc
file in "input.txt";
file out "filtered.txt";
char line[128];

function void filter_prefix() {
    open(in, "r");
    open(out, "w");
    while (read(in, line)) {
        if (starts_with(line, "ERR")) {
            write(out, line);
        }
    }
    close(in);
    close(out);
}

function void main() {
    filter_prefix();
}
```

Running the quick start command above generates a CBL-C translation that mirrors the same file I/O and filtering logic so you can continue development in the higher-level syntax.

## Example: Sharing Helpers Across Files

```text
ctoc_cobol_transpiler --direction cblc-to-cobol \
    --input metrics_main.cblc --output build/metrics_main.cob \
    --input metrics_worker.cblc --output build/metrics_worker.cob
```

With the command above you can compile multiple CBL-C translation units in one invocation. A main module can `import "metrics_worker.cblc";` and call helpers like `add_sale(total, amount)` to update running totals while keeping the bookkeeping logic in a dedicated file. See [`docs/cblc_multi_file_arguments.md`](docs/cblc_multi_file_arguments.md) for full listings that pass integers and booleans between modules.

## Explore Further

- Consult [`design_doc.txt`](design_doc.txt) for architectural goals and the language surface area.
- Review [`docs/runtime_api_reference.md`](docs/runtime_api_reference.md) when writing CBL-C that interacts with the provided runtime helpers.
- Use [`compiler_feature_tracker.md`](compiler_feature_tracker.md) to follow the roadmap and discover which capabilities ship next.

Whether you're modernizing existing COBOL or prototyping new workflows in CBL-C, the transpiler provides repeatable builds, actionable diagnostics, and modular composition patterns so you can stay productive in either language.

### Current CBL-C → COBOL Coverage

The reverse pipeline now recovers top-level WORKING-STORAGE scalars and level 01 group items alongside the procedural skeleton. Items with `PIC X(n)` or `PIC 9(n)` clauses translate into CBL-C declarations—flag-style names (including `-FLAG`, `-SWITCH`, and `-IND` suffixes) become `bool` variables, single-character fields stay as `char` scalars unless their identifiers read like buffers (`RECORD`, `BUFFER`, `TEXT`, `NAME`, etc.), and wider alphanumeric pictures expand into fixed-length `char[]`. Numeric pictures choose between `int`/`long long` or `float`/`double` based on precision, COBOL `COMP-1`/`COMP-2` usage clauses feed the same floating-point recovery, and VALUE clauses on supported scalars become direct initializers in the emitted CBL-C. When a group appears without a PIC clause the emitter now lifts its children into a `record` declaration, mirrors the COBOL group variable, and still surfaces each subordinate field as a standalone scalar so existing procedure bodies remain valid. Registered `COPY` statements are emitted inline with a leading comment so shared working-storage shows up next to the local declarations. Paragraph bodies continue to lift `MOVE`, `IF`, `PERFORM`, `READ`, `WRITE`, `DISPLAY`, and `STOP` statements into idiomatic control structures so the emitted CBL-C runs immediately.【F:transpiler_cobol_reverse.cpp†L1-L2174】【F:samples/cblc/reverse_group_items.cblc†L1-L19】【F:samples/cblc/reverse_copybook.cblc†L1-L11】【F:samples/cblc/reverse_value_defaults.cblc†L1-L8】

Richer numeric pictures (including COMP-3 and signed packed decimals beyond the current heuristics) still fall back to diagnostics. ENVIRONMENT and FILE sections are skipped entirely today, compound group VALUE defaults are ignored, nested group levels beyond a single tier remain unsupported, and only paragraph-level procedures are generated. Consult the CLI examples for the latest snapshot of supported constructs and plan migrations accordingly.【F:transpiler_cobol_reverse.cpp†L1-L2174】【F:docs/cli_usage_examples.md†L17-L36】
