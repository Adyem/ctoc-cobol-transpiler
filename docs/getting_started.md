# Getting Started with the CBL-C ↔ COBOL Transpiler

This guide explains how to prepare your environment, exercise the command-line interface, and take advantage of the latest language- and diagnostics-related improvements. Use it alongside the design and reference documents when evaluating the toolchain or onboarding new teammates.

## Prerequisites

1. Install a C++17-capable compiler toolchain (clang or GCC) and GNU Make.
2. Install GnuCOBOL so the generated COBOL sources can be validated and executed with `cobc`.
3. Clone the repository with submodules enabled: `git clone --recurse-submodules`.
4. Inside the checkout, run `make initialize` once to build the bundled `libft` runtime helpers.

Refer to `docs/onboarding_checklist.md` for a full environment audit before committing changes. If you need platform-specific setup help, see `docs/platform_bootstrap.md` for macOS, Linux, and Windows Subsystem for Linux instructions.

## Quick Start Workflow

1. Build the transpiler and its tests:
   ```
   make initialize
   make all
   make test
   ```
2. Transpile a sample COBOL program while collecting verbose diagnostics:
   ```
   ./ctoc_cobol_transpiler --direction cobol-to-cblc \
       --input samples/cobol/minimal_program.cob \
       --output build/minimal_program.cblc \
       --diagnostics verbose
   ```
   Missing output directories are created on demand, so you can stream results to a brand-new staging area without pre-creating folders.
3. Inspect `build/minimal_program.cblc` (or pipe it through `cblc_formatter`) if you want consistent indentation:
   ```
   cat build/minimal_program.cblc
   ```

4. When you need a native baseline for debugging, emit the same CBL-C inputs as portable C:
   ```
   ./ctoc_cobol_transpiler --direction cblc-to-c \
       --input samples/cblc/minimal_program.cblc \
       --output build/minimal_program.c
   ```
   The generated C pulls in lightweight helper routines (`cblc_string_assign_literal`, `cblc_display_*`, etc.) so you can build and execute the translation with any C toolchain while preserving the runtime semantics exercised by the COBOL backend.

The `docs/cli_usage_examples.md` file contains additional flag combinations. Forward CBL-C→COBOL coverage now runs automatically because the test harness exports `CTOC_ENABLE_FORWARD_TRANSLATION=1`. If you are iterating on unrelated components and want to skip the COBOL toolchain stage temporarily, invoke `make test FORWARD_TRANSLATION=0` (or set `CTOC_ENABLE_FORWARD_TRANSLATION=0`) to restore the previous opt-in behavior.

### Standard Library Status Codes

Every generated standard-library subprogram now shares the same trailing status enumeration so callers can branch on results without remembering helper-specific codes. The trailing slot reports one of the following values:

| Status literal | Meaning | When it occurs |
| --- | --- | --- |
| `TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_SUCCESS` (`0`) | Success | The helper completed without validation problems. |
| `TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_INVALID_ARGUMENT` (`1`) | Invalid argument | Callers provided malformed input, such as non-digit characters to `CBLC-ATOI`. |
| `TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_RANGE_ERROR` (`2`) | Range error | The helper detected overflow or a value that could not fit in the target. |
| `TRANSPILE_STANDARD_LIBRARY_STATUS_LITERAL_DOMAIN_ERROR` (`3`) | Domain error | Mathematical domain checks failed (for example, a negative operand passed to `CBLC-SQRT`). |

The [`docs/abi_spec.md`](abi_spec.md) appendix documents the ABI expectations for these slots, and the individual generator templates in `transpiler_standard_library_*.cpp` import the literals from `t_transpiler_standard_library_status`. When you add a new helper, reuse the shared enum so runtime diagnostics remain consistent.【F:docs/abi_spec.md†L18-L59】【F:cblc_transpiler.hpp†L1017-L1044】

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
* `docs/ide_integration.md` explains how to wire editor support (VS Code, Vim, Emacs) into daily workflows using the bundled grammar and automation tips.

Keeping these documents up to date ensures users understand the toolchain and the protections added in recent iterations.
