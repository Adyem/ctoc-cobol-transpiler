# CBL-C → COBOL gap analysis

The current command-line driver wires a single pipeline stage named `cblc-to-cobol`. When
that stage executes it only dispatches to `transpiler_stub_cblc_to_cobol`, which tokenises a
restricted subset of declarations and writes an ad-hoc COBOL program. None of the real
compiler subsystems — the parser, semantic analyser, COBOL IR builders, or formatter — are
invoked. As a result the project only emits COBOL for the tiny demo grammar hard-coded in
the stub.

## What the driver does today

* `main.cpp` registers exactly one pipeline stage when the caller requests CBL-C→COBOL
  translation. The stage iterates over source/target pairs and calls
  `transpiler_stub_cblc_to_cobol` directly.【F:main.cpp†L6-L24】
* `transpiler_stub_cblc.cpp` reads the raw text, scans for global `char` buffers and
  `function void` blocks, and writes a handcrafted IDENTIFICATION / DATA / PROCEDURE
  skeleton. That is the entirety of the code generation path shipped in the binary
  today.【F:transpiler_stub_cblc.cpp†L362-L456】

## Implementation checklist

To make the transpiler "spit out" COBOL for real CBL-C programs we still need to thread the
existing compiler infrastructure through the pipeline. The following checklist breaks that
effort into discrete steps that can be implemented and validated one by one:

- [ ] **Parse the CBL-C input.**
  - [ ] Load the requested source text and create a `t_parser` for each translation unit.
  - [ ] Invoke `parser_parse_program` to build an AST and persist it alongside the
        `t_runtime_file` metadata already tracked by the driver.
  - [ ] Replace the direct call to `transpiler_stub_cblc_to_cobol` with a pipeline stage that
        hands parsed units to the downstream passes.
- [ ] **Run semantic analysis.**
  - [ ] Use the existing `transpiler_semantics` entry points to analyse the parsed ASTs.
  - [ ] Merge the resulting scopes into the `t_transpiler_context` so downstream stages know
        about data items, files, copybooks, and entry points.
  - [ ] Add regression coverage to ensure semantic errors are surfaced through the CLI
        diagnostics instead of falling back to stub output.
- [ ] **Lower into the COBOL IR.**
  - [ ] Implement a lowering pass that walks the verified AST and emits structured
        paragraphs, statements, and control-flow nodes via `transpiler_cobol_procedure`.
  - [ ] Ensure the lowering phase records the data division constructs required by
        `transpiler_codegen_build_file_sections`.
  - [ ] Capture unit tests that exercise representative CBL-C features (globals, arithmetic,
        branching, file IO) and validate the resulting IR.
- [ ] **Emit formatted COBOL.**
  - [ ] Invoke `transpiler_codegen_build_file_sections` and
        `transpiler_codegen_build_procedure_division` to produce the final source text.
  - [ ] Write the generated program to the caller's `--output` path and surface any IO
        failures through the diagnostics subsystem.
  - [ ] Replace the stubbed stage registration in `main.cpp` with the real pipeline once all
        previous tasks are complete.

Until those checklist items are implemented, any invocation of
`ctoc_cobol_transpiler --direction cblc-to-cobol` will keep relying on the stub and therefore
only succeed for the trivial patterns it can recognise.
