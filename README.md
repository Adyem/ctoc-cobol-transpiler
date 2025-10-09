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
2. Transpile a sample program and inspect the generated COBOL:
   ```
   mkdir -p build
   ./ctoc_cobol_transpiler --direction cblc-to-cobol \
       --input samples/cblc/filter_prefix.cblc \
       --output build/filter_prefix.cob \
       --diagnostics verbose
   ```
3. Compile the COBOL output with `cobc` or feed it back into the transpiler with `--direction cobol-to-cblc` to round-trip the source.

Additional walkthroughs live in [`docs/getting_started.md`](docs/getting_started.md), while [`docs/cli_usage_examples.md`](docs/cli_usage_examples.md) catalogues common flag combinations.

## Example: Filtering Lines in CBL-C

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

Running the quick start command above generates a COBOL program that opens the same files, loops over every record, and writes any line beginning with `ERR` to the output file.

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
