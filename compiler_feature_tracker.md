# COBOL↔CBL-C Compiler Feature Tracker

This document tracks the functional surface we need for the round-trip transpiler. Use the checkboxes to mark features as they
are completed; keep completed items grouped separately from the remaining work to simplify status reviews.

## Completed Features

- [x] Establish core pipeline/context/diagnostics infrastructure to host future compiler stages.
- [x] Provide runtime string helpers for trimming, comparison, and numeric conversion consumed by code generation.
- [x] Implement file I/O shims (open/read/write/close) that align with COBOL runtime expectations.
- [x] Establish token definitions, keyword tables, and trivia handling for the lexer.
- [x] Implement a libft-based lexer that produces token streams without relying on the C++ standard library.
- [x] Design AST node structures and ownership model shared by the parser and later stages.
- [x] Specify supported command-line options and environment variables for the driver.
- [x] Build a command-line driver that accepts source paths and target direction (CBL-C→COBOL or COBOL→CBL-C).
- [x] Add configuration handling for output directories, formatting options, and diagnostics levels.
- [x] Inventory the existing CBL-C language samples and document required tokens and constructs.
- [x] Define the authoritative CBL-C grammar (expressions, statements, declarations, file directives).

## Pending Features

### Core Language Frontend
- [ ] Build a parser that constructs an AST for the supported CBL-C subset.
- [ ] Provide semantic analysis for type checking, scope resolution, and file/record validation.
- [ ] Surface semantic diagnostics through the existing diagnostics subsystem.

### COBOL Code Generation (CBL-C → COBOL)
- [ ] Determine the minimum COBOL dialect requirements and document unsupported features.
- [ ] Map file declarations to ENVIRONMENT/DATA DIVISION blocks with inferred record sizes.
- [ ] Encode data type mappings and formatting helpers for elementary items and groups.
- [ ] Emit procedural COBOL for control flow (IF, PERFORM UNTIL, PERFORM VARYING) matching CBL-C semantics.
- [ ] Generate COBOL paragraphs for user-defined functions or reusable blocks.
- [ ] Support arithmetic, comparisons, and string operations using libft helpers where necessary.
- [ ] Implement formatting and indentation rules for generated COBOL source.

### Reverse Pipeline (COBOL → CBL-C)
- [ ] Capture representative COBOL samples and expected CBL-C outputs for parity testing.
- [ ] Parse the ANSI-85 subset of COBOL targeted by the forward compiler.
- [ ] Recover higher-level constructs (loops, conditionals, file I/O) from procedural COBOL into CBL-C syntax.
- [ ] Normalize identifiers, literal formats, and layout during re-emission to produce idiomatic CBL-C.
- [ ] Validate round-trip fidelity with golden input/output fixtures.

### Runtime & Support Library
- [ ] Audit existing runtime helpers for compatibility with libft memory allocation patterns.
- [ ] Finalize libft-backed scalar, string, and record runtime helpers for generated programs.
- [ ] Document runtime APIs consumed by generated code for future maintenance.

### Tooling & CLI
- [ ] Integrate logging and error reporting consistent with libft capabilities.
- [ ] Package CLI usage examples and documentation for the design doc.

### Testing & Quality Gates
- [ ] Define baseline sample programs for both translation directions.
- [ ] Establish golden-file tests for representative snippets covering both translation directions.
- [ ] Add round-trip tests to ensure COBOL emitted from CBL-C re-parses to the original program.
- [ ] Integrate continuous integration scripts (make targets) that build, run tests, and lint the codebase.
- [ ] Document contribution guidelines and coding standards for future collaborators.
- [ ] Add developer onboarding checklist capturing setup, build, and testing steps.

### Stretch / Future Enhancements
- [ ] Extend the grammar to support advanced numeric picture clauses, OCCURS tables, and paragraph factoring.
- [ ] Offer alternate backends (e.g., direct C output) sharing the same AST and semantic pipeline.
- [ ] Explore performance optimizations (incremental recompilation, caching intermediate representations).
- [ ] Investigate IDE integration hooks (language server, syntax highlighting, code completion).
- [ ] Prototype visualization tooling for pipeline stages and intermediate representations.
