# Getting Started with the CBL-C ↔ COBOL Transpiler

This guide explains how to prepare your environment, exercise the command-line interface, and take advantage of the latest language- and diagnostics-related improvements. Use it alongside the design and reference documents when evaluating the toolchain or onboarding new teammates.

## Prerequisites

1. Install a C++17-capable compiler toolchain (clang or GCC) and GNU Make.
2. Install GnuCOBOL so the generated COBOL sources can be validated and executed with `cobc`.
3. Clone the repository with submodules enabled: `git clone --recurse-submodules`.
4. Inside the checkout, run `make initialize` once to build the bundled `libft` runtime helpers.

Refer to `docs/onboarding_checklist.md` for a full environment audit before committing changes.

## Quick Start Workflow

1. Build the transpiler and its tests:
   ```
   make initialize
   make all
   make test
   ```
2. Transpile a sample program while collecting verbose diagnostics:
   ```
   mkdir -p build
   ./ctoc_cobol_transpiler --direction cblc-to-cobol \
       --input samples/cblc/filter_prefix.cblc \
       --output build/filter_prefix.cob \
       --diagnostics verbose
   ```
3. Inspect `build/filter_prefix.cob` and run it with `cobc` if you want to confirm the generated output.

The `docs/cli_usage_examples.md` file contains additional flag combinations, and `docs/cblc_multi_file_arguments.md` demonstrates how to compile several CBL-C translation units together.

## New Language and Diagnostics Features

Recent development cycles added several capabilities that are immediately available when you use the current toolchain:

### Parser Error Recovery

The parser now records recoverable syntax errors and resynchronizes at statement and paragraph boundaries so a single typo no longer masks every following issue in the file.【F:parser.cpp†L165-L204】【F:parser.cpp†L553-L607】 The CLI still exits with a non-zero status if any errors were seen, but the diagnostic stream now highlights every problematic statement in one run.

### Expanded Numeric Picture Support

The COBOL type descriptor helpers were extended to describe long, long long, float, and double data items with standardized picture clauses and scales.【F:transpiler_cobol_types.cpp†L301-L356】 Generated code can therefore rely on consistent picture formatting across translation units without redefining custom descriptors.

### String Truncation Diagnostics

Semantic analysis computes literal and identifier lengths and emits a dedicated error when a MOVE would truncate an alphanumeric target.【F:transpiler_semantics.cpp†L562-L601】 The error message reports both the source and target widths so you can resize fields or adjust literals with immediate feedback.

### Read-only Data Item Enforcement

Level-78 WORKING-STORAGE constants are now registered as immutable bindings, and semantic analysis rejects MOVE statements that attempt to overwrite them.【F:transpiler_semantics.cpp†L459-L577】 The diagnostics stream reports `TRANSPILE_ERROR_SEMANTIC_IMMUTABLE_TARGET` with the offending identifier so you can redirect the write to a mutable buffer or drop the stray assignment.【F:tests/semantics_tests.cpp†L483-L544】

### Subprogram Parameter Length Checks

The transpiler context tracks caller-observed buffer widths and rejects subprogram registrations that would narrow an alphanumeric parameter, preventing accidental truncation at call boundaries.【F:transpiler_context.cpp†L1143-L1183】 When triggered, the diagnostics stream includes the offending item name alongside caller and callee lengths.

### Bidirectional Source Maps

Source maps now capture the relationship between generated CBL-C output and the originating COBOL statements so tooling can surface diagnostics against either language. The transpiler context exposes helpers to register spans for each translation unit, enumerate the table, and query in both directions when you need to locate the corresponding code snippet.【F:transpiler_context.cpp†L1198-L1313】 Regression tests cover successful registration, reverse lookups, and invalid span rejection so the context stays reliable as new stages adopt the mapping APIs.【F:tests/transpiler_context_tests.cpp†L1432-L1572】

## Where to Learn More

* `docs/runtime_api_reference.md` documents the libft-backed runtime helpers that generated programs link against.
* `docs/cobol_dialect_requirements.md` captures the supported COBOL subset.
* `design_doc.txt` describes the architecture and planned surface area for future releases.

Keeping these documents up to date ensures users understand the toolchain and the protections added in recent iterations.
