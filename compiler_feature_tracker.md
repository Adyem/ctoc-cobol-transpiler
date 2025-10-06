# COBOL↔CBL-C Compiler Feature Tracker

This document tracks the functional surface we need for the round-trip transpiler. Use the checkboxes to mark features as they
are completed; keep completed items grouped separately from the remaining work to simplify status reviews.

## Completed Features

- [x] Establish core pipeline/context/diagnostics infrastructure to host future compiler stages.

## Pending Features

### Core Language Frontend
- [ ] Define the authoritative CBL-C grammar (expressions, statements, declarations, file directives).
- [ ] Implement a libft-based lexer that produces token streams without relying on the C++ standard library.
- [ ] Build a parser that constructs an AST for the supported CBL-C subset.
- [ ] Provide semantic analysis for type checking, scope resolution, and file/record validation.

### COBOL Code Generation (CBL-C → COBOL)
- [ ] Map file declarations to ENVIRONMENT/DATA DIVISION blocks with inferred record sizes.
- [ ] Emit procedural COBOL for control flow (IF, PERFORM UNTIL, PERFORM VARYING) matching CBL-C semantics.
- [ ] Generate COBOL paragraphs for user-defined functions or reusable blocks.
- [ ] Support arithmetic, comparisons, and string operations using libft helpers where necessary.

### Reverse Pipeline (COBOL → CBL-C)
- [ ] Parse the ANSI-85 subset of COBOL targeted by the forward compiler.
- [ ] Recover higher-level constructs (loops, conditionals, file I/O) from procedural COBOL into CBL-C syntax.
- [ ] Normalize identifiers, literal formats, and layout during re-emission to produce idiomatic CBL-C.

### Runtime & Support Library
- [ ] Finalize libft-backed scalar, string, and record runtime helpers for generated programs.
- [ ] Implement file I/O shims (open/read/write/close) that align with COBOL runtime expectations.
- [ ] Provide utility routines for string trimming, comparison, and numeric conversion as required by codegen.

### Tooling & CLI
- [ ] Build a command-line driver that accepts source paths and target direction (CBL-C→COBOL or COBOL→CBL-C).
- [ ] Add configuration handling for output directories, formatting options, and diagnostics levels.
- [ ] Integrate logging and error reporting consistent with libft capabilities.

### Testing & Quality Gates
- [ ] Establish golden-file tests for representative snippets covering both translation directions.
- [ ] Add round-trip tests to ensure COBOL emitted from CBL-C re-parses to the original program.
- [ ] Integrate continuous integration scripts (make targets) that build, run tests, and lint the codebase.
- [ ] Document contribution guidelines and coding standards for future collaborators.

### Stretch / Future Enhancements
- [ ] Extend the grammar to support advanced numeric picture clauses, OCCURS tables, and paragraph factoring.
- [ ] Offer alternate backends (e.g., direct C output) sharing the same AST and semantic pipeline.
- [ ] Explore performance optimizations (incremental recompilation, caching intermediate representations).
