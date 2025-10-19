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
- [x] Add golden samples exercising long, long long, and floating numeric arithmetic plus comparison operators.
- [x] Imports/modules: enforce per-file symbol visibility, support `import "x.cblc"`, and guarantee deterministic module initialization order.
- [x] Warning escalation flag: add a compiler/CLI flag that promotes all warnings (including conversion, overflow, and string-size diagnostics) to errors for strict build configurations.
- [x] Model extended numeric picture clauses covering `PIC 9(18)` (long) and `PIC 9(36)` (long long) ranges.
- [x] Parser error recovery: resynchronize on `;`/`}` to continue after errors and report multiple issues per pass.
- [x] Define floating-point elementary item support and map `PIC V9` patterns to internal numeric types.
- [x] Extend semantic analysis to validate operator compatibility for the expanded numeric domains.
- [x] Capture length-aware alphanumeric items that retain the caller's declared size when passed by reference.
- [x] Surface diagnostics when conversions between declared string lengths would truncate caller data.
- [x] Ensure subprogram parameter binding preserves caller string lengths even when callee accepts larger buffers.
- [x] Enforce type-safety checks for alphanumeric parameters so callees advertise buffers at least as large as each caller-provided `PIC X`, `PIC X(n)`, or `PIC X(255)` argument.
- [x] Provide a `CBLC-STRLEN` standard library subprogram that honors caller-declared lengths, trims trailing spaces, and returns the character count through a by-reference numeric slot.
- [x] Provide a `CBLC-STRNLEN` standard library subprogram that accepts any alphanumeric operand, caps scanning at an explicit `PIC 9(9)` limit, and writes the resulting length into the trailing return slot while respecting the caller's original buffer declaration.
- [x] Provide a `CBLC-STRCMP` standard library subprogram that compares two caller-supplied alphanumeric buffers using their declared lengths and writes a signed result into the trailing return slot.
- [x] Provide a `CBLC-STRCPY` standard library subprogram that copies between caller-supplied alphanumeric buffers using their declared lengths, blanks the destination before copying, and reports truncation through a trailing status slot.
- [x] Provide a `CBLC-STRNCPY` standard library subprogram that performs bounded copies between caller alphanumeric buffers using declared lengths plus an explicit request count, blanks the destination, pads with spaces, and reports truncation through a trailing status slot.
- [x] Provide a `CBLC-STRCAT` standard library subprogram that concatenates two caller-supplied alphanumeric buffers using their declared lengths, respects the destination limit, reports truncation, and records the resulting size in the trailing return slot.
- [x] Provide a `CBLC-MEMCMP` standard library subprogram that compares caller-supplied buffers using declared lengths plus an explicit byte-count, clamps reads to the narrowest limit, and writes a signed ordering result without overrunning inputs.
- [x] Provide a `CBLC-POWEROF` standard library subprogram that raises caller-supplied operands to a caller-supplied exponent, widens to floating precision, rejects invalid domains, and reports status via a trailing numeric slot.
- [x] Provide a `CBLC-STRTOD` standard library subprogram that parses caller-supplied alphanumeric operands into floating results, honors declared lengths, and reports invalid input or range errors through a trailing status slot.
- [x] Provide `CBLC-ABS` and `CBLC-FABS` standard library subprograms that return magnitudes for integral (`PIC S9(36) COMP-5`) and floating (`USAGE COMP-2`) operands, clamp to caller-declared ranges, and report overflow via trailing status slots.
- [x] Provide `CBLC-FLOOR` and `CBLC-CEIL` standard library subprograms that round floating (`USAGE COMP-2`) operands toward their respective infinities, return the adjusted value through a trailing slot, and report fractional adjustments via a status flag.
- [x] Provide a `CBLC-ROUNDED` standard library subprogram that applies COBOL `ROUNDED` clause semantics to floating operands, performs banker-style tie handling, returns the nearest integral result through a trailing slot, and reports fractional adjustments via a status flag.
- [x] Provide a `CBLC-BANKER-ROUND` standard library subprogram that rounds floating operands to a caller-specified scale using banker-style tie handling, enforces valid scales, propagates size errors through a dedicated status code, and returns the adjusted value through a trailing slot.
- [x] Provide `CBLC-ATOI`, `CBLC-ATOL`, and `CBLC-ATOLL` standard library subprograms that convert caller-supplied alphanumeric buffers into signed numeric results, enforce digit-only input, clamp to declared widths, and report conversion status through a trailing numeric slot.
- [x] Provide a `CBLC-SQRT` standard library subprogram that widens numeric operands to floating precision, rejects negative inputs, and returns the computed root via a trailing slot alongside a status flag.
- [x] Provide `CBLC-MIN` and `CBLC-MAX` standard library subprograms that accept floating operands by reference, store the smaller or larger value in trailing result slots, propagate size errors via status flags, and expose the helpers through `std::fmin` and `std::fmax` registry entries.
- [x] Provide `CBLC-TOUPPER` and `CBLC-TOLOWER` standard library subprograms that convert caller buffers between upper and lower case in place while respecting declared lengths and reporting status via a trailing numeric slot.
- [x] Provide `CBLC-ISDIGIT` and `CBLC-ISALPHA` standard library subprograms that classify single-character operands without locale dependencies and report boolean results through trailing numeric slots.
- [x] Dates/times: add `CBLC-DATE-YYYYMMDD` and `CBLC-DATE-DURATION` helpers that validate `YYYYMMDD` buffers, surface packed/serial outputs, and compute durations with signed comparisons.
- [x] Memory & safety: add bounds-checked string routines and a checked `memcpy` that falls back to `memmove`.
- [x] EBCDIC/ASCII boundaries: introduce explicit transcoding points with configurable CCSID selection.
- [x] Collation/locale: provide locale-independent string comparison with an optional locale bridge.
- [x] Sorting/search: provide keyed record sorting, SEARCH ALL wrappers, and comparator helpers.
- [x] CSV/line I/O: support parsing and emitting fixed or variable-length records.
- [x] Provide `CBLC-EXP` and `CBLC-LOG` standard library subprograms that compute exponentials and natural logarithms for floating operands, clamp invalid domains or overflow via trailing status slots, and expose the helpers through `std::exp` and `std::log`.
- [x] Provide `CBLC-SIN`, `CBLC-COS`, and `CBLC-TAN` standard library subprograms that compute trigonometric results for floating operands (and widened integral inputs), apply COBOL intrinsic functions, and surface status codes for overflow or other domain issues through trailing numeric slots.
- [x] Visibility rules: surface diagnostics for public/private types, fields, and functions, enforcing access at semantic analysis time.
- [x] Source maps: retain CBL-C ↔ COBOL span mappings to power diagnostics and debugging outputs.
- [x] Linter/formatter: produce a canonical, deterministic CBL-C pretty-printer for consistent diffs.
- [x] Document the runtime ABI, including calling conventions, return-slot rules, record layout, and alignment guidance for native integrations.
- [x] Multiplication (`*`): covers operands typed as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`); widens both operands before multiplying, reports overflow, and generates COBOL statements that retain sign and scaling.
- [x] Division (`/`): accepts `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens before division, ensures two's-complement quotient semantics with overflow/zero diagnostics, and preserves COBOL sign/scale.
- [x] `float` conversion warnings: flag assignments or implicit coercions from `float` and `PIC V9(n)` items into integral (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`/`long`, `PIC 9(36)`/`long long`), `double`/`PIC V9(18)`, boolean, or alphanumeric (`PIC X`, `PIC X(n)`) targets, issuing truncation or precision-loss warnings.
- [x] `double` conversion warnings: surface diagnostics when `double` or `PIC V9(18)` values flow into integral widths, `float`/`PIC V9(n)`, boolean, or alphanumeric targets, capturing precision or magnitude loss.
- [x] Integral conversion warnings: report when `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), or `PIC 9(36)` (`long long`) values assign into floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`), boolean, or alphanumeric items, highlighting sign/overflow risks.
- [x] Dead-code detection: flag unreachable statements that follow terminating `STOP RUN` directives or reside in branches proven impossible by constant conditions.

## Pending Features

### Core Language / Semantics
- [x] Const/immutability: track const bindings and read-only fields, emitting diagnostics on attempted writes.
- [x] Copybook interop: support COPY includes, manage name collisions, and propagate declared lengths through the pipeline.

### Core Language Frontend

### Numeric Operator Support

#### Integral Operators

- [x] Addition (`+`): supports operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (mapped to `long`), and `PIC 9(36)` (mapped to `long long`); promotes mixed-width operands to the widest participating type, performs two's-complement math with overflow diagnostics, and emits COBOL that preserves sign and scale.
- [x] Subtraction (`-`): handles `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens to the widest type before subtraction, emits two's-complement semantics with overflow checks, and maintains COBOL sign/scale fidelity.
- [x] Modulo (`MOD`): supports modulus operations over `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, promotes to the widest width, enforces two's-complement remainder semantics, and emits COBOL preserving sign expectations.
- [x] Unary plus (`+`): accepts `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, normalizes operand sign via widening to the widest encountered type, and returns the widened representation without altering value.
- [x] Unary minus (`-`): negates operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens prior to negation, diagnoses overflow on minimum values, and preserves COBOL sign handling.
- [x] Absolute value (`ABS`): handles `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens before evaluating magnitude, diagnoses overflow on minimum values, and emits COBOL preserving sign/scale.
- [x] Equality comparison (`==`): evaluates operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands to the broadest width, mirrors two's-complement equality rules, and allows comparison interop with floating (`float`, `double`, `PIC V9(n)`, `PIC V9(18)`) and alphanumeric (`PIC X`, `PIC X(n)`) items by dispatching to the libft/standard `strcmp` helpers for string-aware semantics.
- [x] Equality comparison (`==`): compares operands typed as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and emits COBOL comparisons matching two's-complement semantics.
- [x] Inequality comparison (`<>`): supports mixed operands among `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and mirrors COBOL inequality semantics.
- [x] Less-than comparison (`<`): handles ordering between `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens operands, and emits COBOL preserving signed ordering.
- [x] Less-or-equal comparison (`<=`): supports comparisons among `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and preserves COBOL signed ordering.
- [x] Greater-than comparison (`>`): covers operands declared as `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`), widens operands, and emits COBOL preserving signed ordering.
- [x] Greater-or-equal comparison (`>=`): compares `PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)` (`long`), and `PIC 9(36)` (`long long`) operands, widens operands, and preserves COBOL signed ordering.

#### Floating Operators
- [x] Addition (`+`): accepts operands declared as `float`, `double`, `PIC V9(n)` (single precision), and `PIC V9(18)` (double precision) alongside any integral width (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, respects COBOL precision/scale, and propagates overflow/underflow diagnostics.
- [x] Subtraction (`-`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands mixed with integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, preserves COBOL precision/scale semantics, and reports overflow/underflow.
- [x] Multiplication (`*`): handles `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands with optional integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, respects COBOL precision/scale, and propagates overflow/underflow diagnostics.
- [x] Division (`/`): accepts `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands, optionally mixed with integrals (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`), coerces integrals to floating, applies COBOL precision/scale rules, and reports overflow/underflow and division-by-zero conditions.
- [x] Unary plus (`+`): normalizes sign for `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (and coerced integrals), preserving magnitude while tracking special values for diagnostics.
- [x] Unary minus (`-`): negates `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (and coerced integrals), carrying forward special value diagnostics and COBOL precision expectations.
- [x] Absolute value (`ABS`): accepts `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (or coerced integrals), returns magnitude while preserving scale metadata and propagating special value diagnostics.
- [x] Equality comparison (`==`): compares `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), respecting COBOL precision/scale rounding and surfacing special value diagnostics.
- [x] Inequality comparison (`<>`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), honoring COBOL rounding expectations and diagnostics for special values.
- [x] Less-than comparison (`<`): handles `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), applying COBOL ordering rules with diagnostic propagation.
- [x] Less-or-equal comparison (`<=`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), honoring COBOL ordering and diagnostics for special values.
- [x] Greater-than comparison (`>`): compares `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), respecting COBOL ordering and diagnostics for special values.
- [x] Greater-or-equal comparison (`>=`): supports `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands (with coerced integrals), preserving COBOL ordering semantics and diagnostics.
- [x] Equality comparison (`==`): aligns `float`, `double`, `PIC V9(n)`, and `PIC V9(18)` operands with integral (`PIC 9`, `PIC 9(4)`, `PIC 9(9)`, `PIC 9(18)`, `PIC 9(36)`) and alphanumeric (`PIC X`, `PIC X(n)`) participants, normalizes operands by widening or coercion, honors COBOL rounding expectations, and defers string comparisons to libft/standard `strcmp` helpers while surfacing special-value diagnostics.

#### Assignment Semantics
- [x] `=` assignment operator: parse identifier `=` expression statements in CBL-C, lower them through the existing MOVE infrastructure, and ensure single `=` lexemes never tokenize as comparisons.
- [x] Numeric and floating `=` assignments: allow arithmetic results to flow into numeric and floating targets, propagating overflow, truncation, and special-value diagnostics while preserving variable immutability guarantees.
- [x] Alphanumeric and boolean `=` assignments: accept string and boolean operands, enforce declared-length safety, and honor read-only bindings so assignments cannot clobber protected storage.
- [x] Integral widening assignments: allow implicit promotion from smaller integral types into `long`/`long long`, tracking two's-complement semantics and flagging overflow diagnostics when widening cannot be represented.
- [x] Floating-to-integral assignments: require explicit conversion steps when assigning floating results into integral targets, truncating toward zero per COBOL rules and surfacing truncation diagnostics.
- [x] Decimal scale preservation: when moving between floating and fixed-point items, preserve declared decimal scaling factors and emit diagnostics when scaling adjustments would truncate or overflow.

#### Conversion Warning Coverage
- [x] Boolean conversion warnings: emit diagnostics when boolean operands convert to or from numeric (`PIC 9`, `PIC 9(n)`, `PIC 9(18)`, `PIC 9(36)`, `float`, `double`, `PIC V9(n)`, `PIC V9(18)`) or alphanumeric types, ensuring callers handle non-zero truthiness semantics explicitly.
- [x] Warning groups/flags: add `-Wconversion`, `-Woverflow`, `-Wstring-trunc`, `-Wshadow`, `-Wunused`, and `-Werror` controls.
- [x] Unused/uninitialized analysis: warn on unused variables, unread writes, and definite-assignment gaps.

### Standard Library Subprogram Catalog


### Data / Encoding / I/O

- [ ] Indexed/relative files: add SELECT/ORGANIZATION handling, key definitions, READ NEXT traversal, and locking controls.

### Codegen / Backends

- [ ] C backend: emit portable C from the shared IR to simplify native testing.
- [ ] Deterministic builds: scrub timestamps and paths to guarantee reproducible output.
- [ ] Parallel compilation: build a translation-unit DAG, cache per-file outputs, and support incremental rebuilds.

### Runtime / Stdlib

- [ ] Error model: define uniform status enums and propagate them through trailing return slots.

### Diagnostics & Tooling
- [x] Pretty diagnostics: include line snippets, caret ranges, and suggestion text in diagnostics.

### Reverse Pipeline (COBOL → CBL-C)

- [ ] Comment preservation: carry comments as trivia and re-emit them near original anchors.
- [x] Layout fidelity knobs: provide "normalize" and "preserve" modes for regenerated source.
- [ ] Copybook reconstruction: prefer re-emitting COPY directives instead of fully expanded fields when possible.
- [ ] Recover long, long long, and floating-point picture clauses into canonical CBL-C type annotations.
- [ ] Reconstruct string length metadata so regenerated CBL-C declarations reflect original caller sizes.
- [ ] Emit operator forms that maintain precision across widened numeric ranges.

### Tooling & CLI

### Testing & Quality Gates

- [ ] Fuzzing: add grammar-based and mutation fuzzers for the lexer and parser to guard against crashes.
- [x] Property tests: introduce round-trip and normalization idempotence suites.
- [ ] Differential tests: compare runtime results between COBOL outputs and the alternative C backend.
- [ ] Stress suites: cover huge records, deep nesting, long lines, and wide numerics.
- [ ] Coverage in CI: enforce line and branch coverage thresholds as part of the release gates.
- [x] Add integration tests verifying subprogram calls respect original string lengths across translations.

### Stretch / Future Enhancements
- [ ] Extend the grammar to support advanced numeric picture clauses, OCCURS tables, and paragraph factoring.
- [ ] Offer alternate backends (e.g., direct C output) sharing the same AST and semantic pipeline.
- [ ] Explore performance optimizations (incremental recompilation, caching intermediate representations).
- [ ] Investigate IDE integration hooks (language server, syntax highlighting, code completion).
- [ ] Prototype visualization tooling for pipeline stages and intermediate representations.
