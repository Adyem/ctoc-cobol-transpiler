# COBOL↔CBL-C Compiler Feature Tracker

This document tracks the functional surface we need for the round-trip transpiler. Use the checkboxes to mark features as they
are completed; keep completed items grouped separately from the remaining work to simplify status reviews.

## Completed Features

- [x] Establish core pipeline/context/diagnostics infrastructure to host future compiler stages.
- [x] Provide runtime string helpers for trimming, comparison, and numeric conversion consumed by code generation.
- [x] Finalize libft-backed scalar, string, and record runtime helpers for generated programs.
- [x] Document runtime APIs consumed by generated code for future maintenance.
- [x] Document contribution guidelines and coding standards for future collaborators.
- [x] Add a developer onboarding checklist covering setup, build, and test steps.
- [x] Implement file I/O shims (open/read/write/close) that align with COBOL runtime expectations.
- [x] Establish token definitions, keyword tables, and trivia handling for the lexer.
- [x] Implement a libft-based lexer that produces token streams without relying on the C++ standard library.
- [x] Design AST node structures and ownership model shared by the parser and later stages.
- [x] Build a parser that constructs an AST for the supported CBL-C subset.
- [x] Specify supported command-line options and environment variables for the driver.
- [x] Build a command-line driver that accepts source paths and target direction (CBL-C→COBOL or COBOL→CBL-C).
- [x] Add configuration handling for output directories, formatting options, and diagnostics levels.
- [x] Accept multiple translation units per invocation, mapping each input CBL-C/Cobol file to a corresponding output while enforcing a single `main` entrypoint.
- [x] Integrate logging and error reporting consistent with libft capabilities.
- [x] Inventory the existing CBL-C language samples and document required tokens and constructs.
- [x] Define the authoritative CBL-C grammar (expressions, statements, declarations, file directives).
- [x] Define baseline sample programs for both translation directions.
- [x] Capture representative COBOL samples and expected CBL-C outputs for parity testing.
- [x] Determine the minimum COBOL dialect requirements and document unsupported features.
- [x] Support value-returning functions by threading a trailing by-reference return slot into generated call sequences.
- [x] Capture `void main()` entrypoint metadata and track argument mirroring requirements for COBOL.
- [x] Provide semantic analysis for type checking, scope resolution, and file/record validation.
- [x] Surface semantic diagnostics through the existing diagnostics subsystem.
- [x] Package CLI usage examples and documentation for the design doc.
- [x] Map file declarations to ENVIRONMENT/DATA DIVISION blocks with inferred record sizes.
- [x] Encode data type mappings and formatting helpers for elementary items and groups.
- [x] Emit procedural COBOL for control flow (IF, PERFORM UNTIL, PERFORM VARYING) matching CBL-C semantics.
- [x] Generate COBOL paragraphs for user-defined functions or reusable blocks.
- [x] Support arithmetic, comparisons, and string operations using libft helpers where necessary.
- [x] Implement formatting and indentation rules for generated COBOL source.
- [x] Establish golden-file tests for representative snippets covering both translation directions.
- [x] Add round-trip tests to ensure COBOL emitted from CBL-C re-parses to the original program.
- [x] Parse the ANSI-85 subset of COBOL targeted by the forward compiler.
- [x] Audit existing runtime helpers for compatibility with libft memory allocation patterns.
- [x] Recover higher-level constructs (loops, conditionals, file I/O) from procedural COBOL into CBL-C syntax.
- [x] Validate round-trip fidelity with golden input/output fixtures.
- [x] Normalize identifiers, literal formats, and layout during re-emission to produce idiomatic CBL-C.
- [x] Integrate continuous integration scripts (make targets) that build, run tests, and lint the codebase.

## Pending Features

### Core Language Frontend
- [ ] Model extended numeric picture clauses covering `PIC 9(18)` (long) and `PIC 9(36)` (long long) ranges.
- [ ] Define floating-point elementary item support and map `PIC V9` patterns to internal numeric types.
- [ ] Extend semantic analysis to validate operator compatibility for the expanded numeric domains.
- [ ] Capture length-aware alphanumeric items that retain the caller's declared size when passed by reference.
- [ ] Ensure subprogram parameter binding preserves caller string lengths even when callee accepts larger buffers.
- [ ] Enforce type-safety checks for alphanumeric parameters so callees advertise buffers at least as large as each caller-provided `PIC X`, `PIC X(n)`, or `PIC X(255)` argument, emitting diagnostics when the callee's storage would truncate caller data.
- [ ] Surface diagnostics when conversions between declared string lengths would truncate caller data.

### Numeric Operator Support

#### Integral Operators
- [ ] Addition (`+`): supports operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (mapped to `long`), and `PIC 9(36)` (mapped to `long long`); promotes mixed-width operands to the widest participating type, performs two's-complement math with overflow diagnostics, and emits COBOL that preserves sign and scale.
- [ ] Subtraction (`-`): handles `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens to the widest type before subtraction, emits two's-complement semantics with overflow checks, and maintains COBOL sign/scale fidelity.
- [ ] Multiplication (`*`): covers operands typed as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`); widens both operands before multiplying, reports overflow, and generates COBOL statements that retain sign and scaling.
- [ ] Division (`/`): accepts `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens before division, ensures two's-complement quotient semantics with overflow/zero diagnostics, and preserves COBOL sign/scale.
- [ ] Modulo (`MOD`): supports modulus operations over `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, promotes to the widest width, enforces two's-complement remainder semantics, and emits COBOL preserving sign expectations.
- [ ] Unary plus (`+`): accepts `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, normalizes operand sign via widening to the widest encountered type, and returns the widened representation without altering value.
- [ ] Unary minus (`-`): negates operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens prior to negation, diagnoses overflow on minimum values, and preserves COBOL sign handling.
- [ ] Absolute value (`ABS`): handles `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens before evaluating magnitude, diagnoses overflow on minimum values, and emits COBOL preserving sign/scale.
- [ ] Equality comparison (`==`): evaluates operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands to the broadest width, mirrors two's-complement equality rules, and allows comparison interop with floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) and alphanumeric (`PIC X`, `PIC X(n)`) items by dispatching to the libft/standard `strcmp` helpers for string-aware semantics.
- [ ] Equality comparison (`=`): compares operands typed as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and emits COBOL comparisons matching two's-complement semantics.
- [ ] Inequality comparison (`<>`): supports mixed operands among `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and mirrors COBOL inequality semantics.
- [ ] Less-than comparison (`<`): handles ordering between `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens operands, and emits COBOL preserving signed ordering.
- [ ] Less-or-equal comparison (`<=`): supports comparisons among `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and preserves COBOL signed ordering.
- [ ] Greater-than comparison (`>`): covers operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and emits COBOL preserving signed ordering.
- [ ] Greater-or-equal comparison (`>=`): compares `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens operands, and preserves COBOL signed ordering.

#### Floating Operators
- [ ] Addition (`+`): accepts operands declared as `float`, `double`, `PIC V9(n)` (single precision), and `PIC V9(18)` (double precision) alongside any integral width (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, respects COBOL precision/scale, and propagates overflow/underflow diagnostics.
- [ ] Subtraction (`-`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands mixed with integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, preserves COBOL precision/scale semantics, and reports overflow/underflow.
- [ ] Multiplication (`*`): handles `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands with optional integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, respects COBOL precision/scale, and propagates overflow/underflow diagnostics.
- [ ] Division (`/`): accepts `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands, optionally mixed with integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, applies COBOL precision/scale rules, and reports overflow/underflow and division-by-zero conditions.
- [ ] Unary plus (`+`): normalizes sign for `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (and coerced integrals), preserving magnitude while tracking special values for diagnostics.
- [ ] Unary minus (`-`): negates `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (and coerced integrals), carrying forward special value diagnostics and COBOL precision expectations.
- [ ] Absolute value (`ABS`): accepts `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (or coerced integrals), returns magnitude while preserving scale metadata and propagating special value diagnostics.
- [ ] Equality comparison (`=`): compares `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), respecting COBOL precision/scale rounding and surfacing special value diagnostics.
- [ ] Inequality comparison (`<>`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), honoring COBOL rounding expectations and diagnostics for special values.
- [ ] Less-than comparison (`<`): handles `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), applying COBOL ordering rules with diagnostic propagation.
- [ ] Less-or-equal comparison (`<=`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), honoring COBOL ordering and diagnostics for special values.
- [ ] Greater-than comparison (`>`): compares `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), respecting COBOL ordering and diagnostics for special values.
- [ ] Greater-or-equal comparison (`>=`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), preserving COBOL ordering semantics and diagnostics.
- [ ] Equality comparison (`==`): aligns `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands with integral (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`) and alphanumeric (`PIC X`, `PIC X(n)`) participants, normalizes operands by widening or coercion, honors COBOL rounding expectations, and defers string comparisons to libft/standard `strcmp` helpers while surfacing special-value diagnostics.
- [ ] ROUNDED helper: applies COBOL `ROUNDED` clause semantics to `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` results, coercing integrals when present, and ensures banker-style rounding ties are honored.
- [ ] Banker-style rounding helper: offers explicit banker-rounding support for `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` results (with coerced integrals), aligning with COBOL precision/scale and propagating overflow/underflow diagnostics.

#### Assignment Semantics
- [ ] Integral widening assignments: allow implicit promotion from smaller integral types into `long`/`long long`, tracking two's-complement semantics and flagging overflow diagnostics when widening cannot be represented.
- [ ] Floating-to-integral assignments: require explicit conversion steps when assigning floating results into integral targets, truncating toward zero per COBOL rules and surfacing truncation diagnostics.
- [ ] Decimal scale preservation: when moving between floating and fixed-point items, preserve declared decimal scaling factors and emit diagnostics when scaling adjustments would truncate or overflow.

#### Conversion Warning Coverage
- [ ] `float` conversion warnings: flag assignments or implicit coercions from `float` and `PIC V9(n)` items into integral (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`/`long`, `PIC 9(36)`/`long long`), `double`/`PIC V9(18)`, boolean, or alphanumeric (`PIC X`, `PIC X(n)`) targets, issuing truncation or precision-loss warnings.
- [ ] `double` conversion warnings: surface diagnostics when `double` or `PIC V9(18)` values flow into integral widths, `float`/`PIC V9(n)`, boolean, or alphanumeric targets, capturing precision or magnitude loss.
- [ ] Integral conversion warnings: report when `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), or `PIC 9(36)` (`long long`) values assign into floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`), boolean, or alphanumeric items, highlighting sign/overflow risks.
- [ ] Boolean conversion warnings: emit diagnostics when boolean operands convert to or from numeric (`PIC 9`, `PIC 9(n)`, `PIC 9(18)`, `PIC 9(36)`, `float`, `double`, `PIC V9(n)`, `PIC V9(18)`) or alphanumeric types, ensuring callers handle non-zero truthiness semantics explicitly.

#### Diagnostic Controls
- [ ] Warning escalation flag: add a compiler/CLI flag that promotes all warnings (including conversion, overflow, and string-size diagnostics) to errors for strict build configurations.

### Standard Library Subprogram Catalog

- [ ] `strlen` helper: provide a generated subprogram that accepts `PIC X`, `PIC X(n)`, or `PIC X(255)` arguments, returns the caller-allocated length slot (passed as the trailing by-reference argument) typed as `PIC 9(9)` or wider, and guarantees the callee respects the original caller buffer size when computing the length.
- [ ] `strcmp` helper: emit a subprogram that compares two alphanumeric operands (`PIC X`, `PIC X(n)`, `PIC X(255)`), returning a signed integral result (`PIC 9`, `PIC 9(4)`, or `PIC 9(9)`) via the trailing return slot, using lexicographic ordering consistent with libft/standard semantics while honoring caller-provided buffer lengths.
- [ ] `sqrt` helper: implement a numeric subprogram that accepts operands declared as `float`, `double`, `PIC V9(n)`, `PIC V9(18)`, or integral widths up to `PIC 9(36)`, widens to floating precision, and writes the square root into the trailing return slot preserving COBOL scale/precision with diagnostic coverage for negative inputs.
- [ ] `powerof` helper: add exponentiation support that handles integral bases (`PIC 9` through `PIC 9(36)`) and floating operands (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`), coercing arguments to the widest applicable type and materializing the result via the trailing return slot while surfacing overflow and precision diagnostics.
- [ ] `strcpy` helper: generate a subprogram that copies from a source `PIC X`, `PIC X(n)`, or `PIC X(255)` into a destination buffer of equal or greater declared size, returning a `PIC 9(9)` status flag via the trailing slot to signal truncation while honoring caller-provided lengths during the copy.
- [ ] `strncpy` helper: expose a bounded copy routine that accepts source and destination alphanumeric operands (same variants as `strcpy`) plus a `PIC 9(9)` length argument, ensuring the callee stops after the requested characters, pads with spaces when necessary, and reports truncation through the trailing return slot.
- [ ] `strnlen` helper: provide a length computation that accepts any alphanumeric operand (`PIC X`, `PIC X(n)`, `PIC X(255)`), caps scanning at an explicit `PIC 9(9)` limit, and writes the resulting length into the trailing return slot while respecting the caller's original buffer declaration.
- [ ] `strcat` helper: concatenate two alphanumeric operands (`PIC X`, `PIC X(n)`, `PIC X(255)`) into a destination buffer of sufficient size, surfacing truncation warnings when the destination declaration cannot hold the combined length, and materialize the resulting size via the trailing return slot.
- [ ] `memcmp` helper: compare binary or alphanumeric buffers represented as `PIC X`, `PIC X(n)`, or `PIC X(255)` plus an explicit `PIC 9(9)` byte-count, returning a signed integral result (`PIC 9`, `PIC 9(4)`, or `PIC 9(9)`) and guaranteeing no reads beyond the caller-specified length.
- [ ] `toupper`/`tolower` helpers: supply character-case normalization that accepts single-character `PIC X` items or varying-length alphanumeric buffers (`PIC X(n)`, `PIC X(255)`), mutates them in-place respecting caller-provided sizes, and returns a success flag (`PIC 9`) through the trailing slot.
- [ ] `isdigit`/`isalpha` helpers: furnish classification routines for single-character alphanumeric operands (`PIC X`), writing boolean (`PIC 9`) results through the trailing slot while avoiding locale dependencies.
- [ ] `atoi`/`atol` helpers: convert numeric strings (`PIC X`, `PIC X(n)`, `PIC X(255)`) into integral results (`PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), return conversion status via a `PIC 9` flag, and emit overflow/truncation diagnostics.
- [ ] `strtod` helper: parse alphanumeric operands (`PIC X`, `PIC X(n)`, `PIC X(255)`) into floating representations (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`), populate the trailing return slot with the parsed value, and report range errors through diagnostics.
- [ ] `fabs`/`abs` helpers: deliver absolute-value subprograms for integral (`PIC 9` through `PIC 9(36)`) and floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) operands, writing results into the trailing return slot and propagating overflow diagnostics for minimum values.
- [ ] `floor`/`ceil` helpers: provide rounding primitives that accept floating operands (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) and optionally integral inputs, producing results in the same type domain within the trailing return slot while flagging scale adjustments.
- [ ] `exp`/`log` helpers: expose exponential and natural-log functions for floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) operands with integral widening support, materializing the result through the trailing slot and emitting diagnostics for out-of-domain arguments or overflow.
- [ ] `sin`/`cos`/`tan` helpers: implement trigonometric routines that accept floating operands (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) and optional integral inputs coerced to floating, returning floating results and tracking domain/precision diagnostics.
- [ ] Additional numeric helpers: track future candidates such as `cbrt`, `hypot`, `min`/`max`, or `clamp` that should accept the same integral and floating domains defined above, ensuring the standard library surface mirrors common CBL-C idioms once return-slot plumbing lands.

### Reverse Pipeline (COBOL → CBL-C)

- [ ] Recover long, long long, and floating-point picture clauses into canonical CBL-C type annotations.
- [ ] Reconstruct string length metadata so regenerated CBL-C declarations reflect original caller sizes.
- [ ] Emit operator forms that maintain precision across widened numeric ranges.

### Tooling & CLI

### Testing & Quality Gates

- [ ] Add golden samples exercising long, long long, and floating numeric arithmetic plus comparison operators.
- [ ] Add integration tests verifying subprogram calls respect original string lengths across translations.

### Stretch / Future Enhancements
- [ ] Extend the grammar to support advanced numeric picture clauses, OCCURS tables, and paragraph factoring.
- [ ] Offer alternate backends (e.g., direct C output) sharing the same AST and semantic pipeline.
- [ ] Explore performance optimizations (incremental recompilation, caching intermediate representations).
- [ ] Investigate IDE integration hooks (language server, syntax highlighting, code completion).
- [ ] Prototype visualization tooling for pipeline stages and intermediate representations.
