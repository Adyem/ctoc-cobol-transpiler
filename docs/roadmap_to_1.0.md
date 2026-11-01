# CBL-C Roadmap to a Cohesive 1.0 Language and Standard Library

**Status:** design proposal; not a claim that the listed work is implemented
**Planning baseline:** repository state and documentation initially reviewed 2026-09-19
**Last backend review:** pass/artifact DAG, artifact-versus-validation cache identity, expectation-validation separation, module-IR/output-parser budgets, canonical renderer bytes/source maps, activation-layout boundaries, target-profile/tool-invocation separation, immutable toolchain and response-file inputs, canonical bounded tool-environment evidence and joint launch ceilings, compile-result arbitration, and bounded diagnostic capture rechecked 2026-10-16
**Target:** a stable, portable CBL-C-to-COBOL language and native CBL-C standard library

## 1. Purpose and release thesis

CBL-C 1.0 should be a language in which a programmer can write and maintain a
complete, multi-file application without relying on C++ implementation files,
platform-specific COBOL extensions, undocumented runtime behavior, or a
different language for ordinary library facilities.

The product has two deliberately separate implementation layers:

1. **Compiler infrastructure** may use C++ for lexing/parsing, semantic
   analysis, template resolution, lowering, COBOL emission, source mapping,
   compiler diagnostics, and compiler-only process/build integration.
2. **Program runtime behavior and the public standard library** are authored
   in CBL-C (`.cblc`) and compiled through the normal CBL-C-to-COBOL pipeline.
   Runtime semantics for generated programs must execute as generated COBOL,
   not as hidden C++ helpers. C++ may embed, index, build, or register canonical
   `.cblc` units, but must not contain a second implementation of a public
   library API or COBOL text templates intended to substitute for native
   CBL-C source. C++ test harnesses are test tooling only and cannot supply
   product behavior.

The 1.0 goal is a cohesive and specified language, not source or ABI
compatibility with C++ and not a byte-for-byte reproduction of the C++ standard
library. C++ is a useful requirements catalogue: CBL-C should provide the
commonly expected ownership, sequence, associative, unordered-associative, and
container-adaptor families, but define each contract in terms that can be
implemented portably as generated COBOL.

The roadmap is dependency ordered. Resource lifetime and generic class
instantiation precede containers; iterators and algorithms are shared
foundations rather than optional polish; file, text, and error APIs follow the
same result/exception and portability rules as the language core.

**Source naming decision and migration status:** the owning text class is
spelled lowercase `string`; the competing compiler-managed primitive/pseudo-
class of that spelling is removed, not retained as an alias. Raw character
storage remains `char[]` or supported character pointers. The current source
tree has adopted this declaration and removed the separate string data-kind
and native string-helper registry paths. This does not mean all migration work
is complete: the latest worktree search found no reference to the removed
`CBLC-STRCMP-STRING` symbol in checked-in `.cob`, `.cblc`, or JSON files, but
absence of that spelling does not prove generated-output freshness or ABI
correctness. Phase 0 must regenerate the checked-in showcase outputs from their
`.cblc` sources and validate their manifests and COBOL call sets. Any remaining
legacy helpers are for raw character-buffer interoperability and must be
named/documented as such; they must not be used as an alternate representation
or implementation of the owning `string` class.

Examples, fixtures, and lifecycle tables throughout this roadmap use the
canonical lowercase source types `string` and `vector<string>`. This editorial
normalization does not imply that lifecycle lowering, generic class
instantiation, or generated COBOL execution for the class is complete.

This is a deliberate source-language break, not a dual-spelling period. Keep
the completed spelling/data-kind migration intact: do not reintroduce a
synthetic compiler `string` type, a capitalized `String` alias, or hidden
helper path for class behavior. Finish the remaining class semantic and
runtime work in this order:

1. Make the owning class's source and canonical semantic declaration lowercase
   `string`, with a private `char*`-style owned buffer, explicit length and
   capacity, and type-directed construct/copy/move/assign/destroy operations.
   `char[]` remains fixed raw storage; it is not a dynamically resizable or
   owning string substitute. Keep null-terminated interop distinct from
   length-delimited text.
2. Make canonical class declarations visible to semantic and template analysis
   before parsing user specializations. Resolve `string` uniformly in local,
   member, parameter, return, pointer, and template-argument positions, and
   lower methods/lifetimes through normal class dispatch.
3. The old fixed-buffer `string` declaration, literal representation, and
   string-specific source/ABI path have been removed rather than retained in a
   compatibility adapter. Keep the remaining low-level APIs explicitly
   typed/named as raw `char[]` or character-pointer interfaces, with documented
   length and ownership rules; these are not a second `string` representation.
   Audit every native `.cblc` public API and test against that split, and reject
   former buffer-shaped `string` syntax with a clear diagnostic.
4. The old source declaration/parser path, synthetic pseudo-class, and native
   `*-STRING` helper registry entries are removed. Preserve regression tests
   proving they cannot return. Complete the ABI inventory for remaining raw
   character-buffer helpers; do not recreate a legacy owning-string adapter or
   alias as part of that work.
5. Prove construction, mutation, copies, moves, exceptions, and destruction in
   compiled and executed COBOL, including `vector<string>` and nested
   specializations. Translation markers and successful parsing alone do not
   satisfy this gate.

## 2. Current baseline and evidence-backed gaps

The repository already has a useful foundation:

- `docs/cblc_language_standard.md` is identified as the normative language
  specification and says the supported forward product is CBL-C to COBOL.
- `standard library/README.md` says `.cblc` sources are canonical and embedded
  at compiler build time. A C++ embedder/registry is infrastructure, not the
  public implementation language.
- The library includes a managed `string` class and a templated `vector<T>`,
  but their current contracts are prototype-level. The README explicitly says
  non-trivial element destruction, move semantics, and constructor/destructor
  dispatch remain planned. `vector.cblc` allocates `new T[capacity]`, writes
  live elements by assignment, relocates by assignment, and deletes the block
  without a type-directed live-element destruction loop; `count` and capacity
  do not represent per-slot object lifetime. Its storage and relocation fields
  are public. This is not a `vector<string>` or arbitrary RAII guarantee.
  `vector<string>` remains unsupported until generated COBOL executes with
  exact constructor/destructor, rollback, and error assertions.
- The test suite contains both translation/structure tests and GnuCOBOL-backed
  compile/run tests, but the focused vector tests are weaker than their names
  suggest. [`tests/standard_library/standard_library_vector_tests.cpp`](../tests/standard_library/standard_library_vector_tests.cpp)
  transpiles the scalar vector and checks emitted markers; its vector/string
  test transpiles `vector<string>` and checks generated record/offset text. The
  latter does not invoke `cobc` or run the generated executable. It also
  intentionally exercises the prototype's clamped `insert` positions, ignored
  invalid `erase`, and negative-size-to-zero behavior. Keep those assertions
  only as labeled legacy characterization where needed; replace them in the
  native 1.0 contract tests with named exceptions and unchanged-state checks.
  Add distinct compile-and-execute tests before claiming `vector<string>` works.
  The selected 1.0 naming rule is that lowercase `string` is the
  standard-library owning class; the separate compiler-managed string
  representation is removed from the source language. Raw character storage
  uses `char[]`/supported character pointers. This is a source-language and
  ABI migration, not a spelling-only rename: the parser must stop consuming
  `string` as a primitive, and literal/buffer conversions and legacy COBOL
  string ABI handling must be routed through explicit class operations or a
  versioned internal adapter. The existing `vector<string>` test now resolves
  the canonical class, but translation still fails before COBOL generation; it
  must be made into a compile-and-execute class-specialization test. Until
  canonical standard-library class declarations participate in generic semantic
  analysis and generated COBOL compiles and executes with the lifecycle call
  graph validated, it is not evidence for the Phase 2 gate. A local class
  spelling alone is insufficient.
- `docs/template_implementation_plan.md` describes bounded monomorphization,
  deterministic instantiation, class templates, and explicit unsupported
  areas. This is a platform for containers, but not by itself a full generic
  programming model.
- `standard library/string.cblc` still exposes its backing `string value`
  field publicly and implements byte-oriented helpers without the proposed
  UTF-8 validation invariant. Its `atoi`/`strtod` methods can return zero for
  malformed input. This conflicts with the 1.0 `string` contract: private
  representation, validated UTF-8, strict parsing, and strong mutation
  guarantees. Treat full `string` as a dedicated subsystem phase, not routine
  library cleanup.
- Exception lowering has generated-COBOL state and cleanup support, but its
  implementation plan still identifies incomplete cases such as pointer
  fields, arrays in throwable payloads, user-defined destructor dispatch,
  constructor forwarding, some inheritance behavior, and the complete payload
  ABI. The implemented subset is not yet the universal lifetime invariant
  required by 1.0. Each limitation must either be implemented for an in-scope
  type or rejected deterministically if that type/feature is explicitly out of
  scope; a bounded exception demo is not a general RAII proof. The
  status-parameter migration plan explicitly requires aborting if another
  exception is thrown while one is already being propagated or cleaned up.
- The project has multi-file modules, file I/O, a generated-program ABI, and a
  cross-platform intent, but the repository documents different maturity
  levels for several of these surfaces. COBOL compilation/execution coverage
  exists for selected programs, but the current tests do not establish the
  required Windows/POSIX and compiler-profile conformance matrix.
- As of the planning baseline (2026-09-19), the repository's `.github`
  directory contains only `CODEOWNERS`; the live `main` branch likewise has
  no `.github/workflows` directory. There is therefore no repository workflow
  evidence for automated baseline, Windows/POSIX, or COBOL-compiler testing.
  This was checked in the local checkout and against the [live `.github`
  listing](https://github.com/Adyem/ctoc-cobol-transpiler/tree/main/.github);
  the workflow-directory API lookup returned 404. Add the first CI workflow
  as part of Phase 0 rather than treating local test presence as
  cross-platform proof.
- `docs/build_status.md` is dated 2025-10-25 and reports a failed `libft`
  submodule checkout at a missing commit. That report is historical, not a
  current build result: this checkout has no `.gitmodules` file or root
  `libft` gitlink, and the current Makefile refers to `imported` sources.
  Re-run and record initialization, build, and test commands against the
  current tree; do not silently rewrite history or treat the old report as
  evidence either of current failure or current success.

Use this as the planning snapshot, not as a release certification:

| Area | Current evidence-based reading | What must be proven next |
| --- | --- | --- |
| Front end | Substantial compiler structure and test coverage for a bounded language. | Stable semantic rules, diagnostics, and unsupported-feature rejection across the documented source surface. |
| COBOL output | Significant emitter and selected compile/run integrations exist. | Manifest agreement, complete compiler-profile coverage, and execution of the generated artifact for each runtime claim. |
| Templates | Useful bounded-instantiation groundwork exists. | Structural canonical types, recursive lifecycle dispatch, and the depth-16 nested proof. |
| Exceptions | A bounded lowering subset and explicit abort-on-second-exception behavior exist; payload/lifetime support is incomplete. | Complete payload ABI and type-directed cleanup for every throwable shape promised in 1.0; prove propagation, rollback, and fatal double-exception behavior. |
| Standard library | Native `.cblc` helpers and a scalar/value-like vector prototype exist. | Replace prototype vector semantics with explicit live-element lifecycle; execute `vector<string>` and nested specializations before broadening containers. |
| `string` | A public-field wrapper with byte-oriented methods and permissive legacy parsing exists. | Private owning representation, valid UTF-8 invariant, strict parse errors, transactional mutation, and rerun of all vector/nested evidence. |
| Vector API behavior | Existing tests encode clamping/no-op behavior for invalid requests and pointer-returning indexing. | Change implementation and tests together to checked access, named errors, documented guarantees, and safe reference/borrow semantics. |
| ABI/status surface | Legacy result/status layouts coexist with native return values and typed parse results. | A catalogued split among public API statuses, compatibility adapters, compiler infrastructure, and the versioned Generated COBOL ABI, with each boundary's behavior explicit. |
| Lifetimes | Copy/move, destructor, payload, and container support are not yet a sufficient general RAII guarantee. | Phase 1 lifecycle kernel, Phase 3 nested proof, and Phase 4 exceptional cleanup/payload gate; no earlier phase claims universal RAII. |
| Portability | Portable intent and an ANSI-85 profile document exist; local tests use GnuCOBOL. | Fresh clean builds and required Windows/POSIX/compiler CI evidence; no portability claim based on intent alone. |

This snapshot intentionally separates “substantial code exists” from “the
feature is stable and proven.” Phase gates below are arranged around that
distinction.

**Current vector verification probe (2026-09-19):** the focused vector tests
currently prove source translation and selected generated markers only. An
attempt to compile and execute generated `vector<int>`/`vector<string>` COBOL
with the installed GnuCOBOL exposed unresolved output defects, including
fixed-format data names/lines exceeding the target's usable source width and
undeclared exception-payload temporaries (for example, the generated
`VECTOR-POP-BACK-ERROR-CODE`). The emitter's word-continuation marker has been
corrected where it wrapped at token boundaries, but this is not yet successful
compile evidence. Keep the runtime/lifecycle gate open until generated-name and
payload declarations are validated against an independent manifest and a real
COBOL compile-and-run test passes. Do not describe the vector as runtime-proven
on the basis of the current translation tests.

### 2.1 Prototype-to-1.0 contract migration

The prototype is not required to preserve behavior that the 1.0 contract
deliberately rejects. Change implementation, source-level tests, and generated
COBOL expectations as one migration; retain old behavior only in a clearly
named compatibility adapter when an existing external ABI actually requires
it. The current test suite must not make clamping or silent failure look like
the normative vector contract.

| Surface | Current prototype/ABI behavior | 1.0 behavior and migration rule |
| --- | --- | --- |
| Vector storage/lifetime | Capacity is allocated as `new T[capacity]`; writes and relocation use assignment; block deletion does not dispatch destruction for exactly the live range. | Allocate raw/opaque slots without invoking `T` construction for unused capacity; track successful construction of each live slot. Construct/copy/move only live elements, destroy them exactly once in reverse order, and keep size distinct from capacity. `reserve` alone must not change the number of live elements. |
| Vector encapsulation | `data`, relocation storage, count, and capacity are public implementation fields. | Make representation private. Public access goes through specified methods/iterators; implementation tests inspect state through behavior or test-only instrumentation, never by depending on writable internals. |
| `insert`/`erase` positions | Negative/past-end insertion is clamped; invalid erase is ignored. | Valid insertion positions are `[0,size]`; all other indices throw `out_of_range` without mutation. Erase requires a live element/range; invalid requests throw `out_of_range` or `iterator_error` for invalid iterators and leave the vector unchanged. |
| Negative sizes | Negative `resize`/`assign` requests become zero. | Reject outside-domain counts with `invalid_argument`; reject representable-domain overflow or requests above `max_size` with `length_error`; preserve prior contents on failure. |
| Indexed access | `operator[]` returns `T*` and has no iterator/invalidation contract. | `at` and `operator[]` are checked and expose the documented reference/borrow form; invalid indices throw `out_of_range`. `data()`, if retained, is a scoped borrow governed by the pointer/borrow contract. Define generation/owner checks for iterators in Phase 6; do not retain an unchecked `operator[]` variant. |
| string representation/parsing | Public `value` storage, byte loops, and malformed numeric text that can collapse to zero. | Private owned UTF-8 representation; validate all ingress/mutation; strict `parse_*` throws `parse_error`/`range_error`. Legacy C-shaped symbols may retain their pinned ABI only as explicitly labeled adapters, never as the safe `string` parsing contract. |
| Throw payload/lifecycle | Implemented exception subset rejects some managed layouts and does not yet provide the complete payload ABI. | Implement type-directed payload construction/copy/move/destruction for all 1.0-throwable types. Any deliberately unsupported language feature gets a stable semantic diagnostic and a negative test; unsupported layout must never silently fall back to byte copy. |

For ABI classification, Phase 0 must publish one row per catalogued entry and
record: source-level name/signature; canonical `.cblc` implementation; emitted
COBOL program name and ABI version; result-slot, exception-context, and legacy
status fields; failure policy; ownership/lifetime rule; and any adapter target.
Classify each API by its public status/contract, then classify the generated or
compiler-facing implementation boundary separately. A legacy-shaped API can
have a native `.cblc` implementation; it is not automatically an ABI adapter.

| Public API status | Contract and canonical source rule |
| --- | --- |
| Native `.cblc` public API | **Typed return/exception contract:** return a useful value or `void`; failures use typed exceptions or a deliberately designed result type. Canonical implementation is `.cblc` compiled through the normal pipeline. Result slots and exception context are ABI details, not meaningless source status outputs. |
| Legacy C-like helper | Preserve familiar C-like naming/semantics only when explicitly documented. It is not the safe typed API by default; zero-on-parse-error or other legacy behavior must be named, tested, and separated from strict methods. It may still be implemented natively in `.cblc`. |

| Implementation/ABI surface | Owner/artifact | Boundary rule |
| --- | --- | --- |
| Canonical `.cblc` implementation | Standard-library CBL-C source embedded and compiled through the normal pipeline. | Owns generated-program behavior for its API; no duplicate C++ implementation. |
| Versioned implementation interface | Standard-library catalog and ABI manifest. | Binds the public signature/failure/ownership `interface_version` independently from the generated COBOL `abi_version`, alongside the canonical implementation source and emitted program name. Generated callers and implementations must agree; source-contract and linkage changes follow separate explicit bump/migration rules. |
| Compatibility adapter | Named/versioned entry delegating to the canonical implementation. | May preserve a historical program name, argument layout, and status mapping only while required for compatibility; behavior and retirement policy are documented and parity-tested. |
| Generated COBOL ABI | Emitted `PROGRAM-ID`, `LINKAGE SECTION`, `PROCEDURE DIVISION USING`, result/context records, payload layouts, and call order. | Version and validate against the ABI manifest. Native order is logical arguments, optional result slot, then optional exception context; legacy status fields appear only for a declared compatibility adapter. |
| C++ registry/generator | Compiler-side standard-library registry, embedder, and generator. | May discover/register canonical `.cblc` units, embed their source, and generate registry/interface metadata. It must route units through normal CBL-C parsing and lowering; it may not contain COBOL templates or emit duplicate library/runtime routine bodies. |
| Compiler infrastructure/boundary glue | C++ compiler source and ABI marshaling code. | May parse, register, embed, lower, or marshal calls; may not implement behavior that generated programs observe as a second library/runtime implementation. |
| Generated COBOL runtime behavior | COBOL declarations/paragraphs emitted from compiler lowering or native `.cblc` source. | Performs exception propagation, allocation coordination, and lifecycle dispatch as inspectable COBOL; no platform unwinder or hidden C++ helper supplies semantics. |
| Test-only tooling | C++ test drivers, fault injectors, validators, and fixture runners. | May instrument/count/fail operations for tests; is not shipped as a program runtime dependency and is not evidence of execution unless the generated COBOL is actually run. |

Production C++ may remain in the compiler-infrastructure category and C++ test
harnesses remain test tooling, but neither may supply duplicate generated-
program semantics. The catalog must record both the API status and the
implementation/ABI surface for every entry; the validator must reject
unclassified entries and accidental public status parameters. Native public
APIs use meaningful return values, named errors/exceptions, or a deliberately
designed result value; anonymous status outputs remain only at a versioned
compatibility boundary.

The native ABI should use one versioned ordering—`[logical arguments]`
`[optional result slot]` `[optional exception context]`—with exception
context last. A compatibility adapter may retain its older result/status
ordering, but callers and helpers from different ABI versions must never be
combined in one link. This preserves return-by-value source semantics without
pretending COBOL `CALL` needs no result storage, and prevents a status field
from leaking back into the public CBL-C signature.

Before feature work, reconcile these documentation and implementation seams:

1. **Pin one product contract.** The language standard, compiler feature
   tracker, template and exception plans, standard-library README, runtime ABI,
   and COBOL dialect profile must agree on each feature's status. Some current
   pages describe features as complete while other pages call the same surface
   unsupported or planned. The old build-status page is dated 2025 and must not
   be treated as a current release gate without rerunning its checks.
2. **Audit ABI documentation.** `docs/abi_spec.md` needs a clean, tested
   description of return slots, exception context, ownership representation,
   string layout, and status compatibility. Its current prose contains stale
   assumptions and encoding artifacts. The document must match generated
   artifacts, not historical templates.
3. **Define one support matrix.** For each feature, identify parser,
   semantic, COBOL-lowering, runtime, module-boundary, portability, and test
   status. “Parser accepts it” is not the same as supported.
4. **Inventory the public library.** Record each API's public status (native
   `.cblc` typed return/exception API or legacy C-like helper) separately from
   its implementation/ABI surfaces (canonical `.cblc`, versioned
   implementation interface, compatibility adapter, Generated COBOL ABI,
   C++ registry/generator, compiler infrastructure, generated COBOL runtime
   behavior, or test tooling). Record the exact signature and mark
   unimplemented methods honestly.
5. **Resolve COBOL profile contradictions.** `docs/cobol_dialect_requirements.md`
   describes an ANSI-85, mostly sequential-file surface, while other project
   tracking describes broader file support. Select and test a portable
   baseline/profile set; do not let a vendor extension become a silent
   language guarantee.

These are P0 release-readiness tasks: they prevent designing new features on
top of contradictory assumptions.

## 3. Design principles for 1.0

### 3.1 Specify observable behavior before adding syntax

Every feature record in the language standard should state: accepted and
rejected source forms; type and overload rules; value/reference semantics;
construction and destruction; evaluation order; failure behavior; COBOL
representation; module/ABI implications; portability assumptions; and
conformance tests. Generated COBOL is an implementation artifact, not a
replacement for source-language semantics.

### 3.2 Prefer defined, diagnosable behavior

CBL-C should not copy C or C++ undefined behavior merely for familiarity. Define
the behavior of arithmetic overflow, division by zero, invalid conversions,
out-of-range indexing, null dereference, invalidated iterators, failed
allocation, malformed text, and invalid file operations. Where recovery is
possible, use a typed result or exception; where the operation violates a
programmer contract, provide a defined checked failure in normal/debug builds
and a documented release policy. The compiler must never silently truncate an
integer, string, identifier, or exception payload.

Keep warnings for suspicious but defined conversions, and errors for
unrepresentable or ambiguous constructs. Do not overload an anonymous integer
status with multiple meanings.

### 3.3 Make object lifetime a compiler-level invariant

An object is cleaned up exactly once if and only if its construction
completed. Destruction order is reverse completion order. This applies to
ordinary scope exit, return, loop control leaving a scope, exceptions, failed
construction, container erase/clear/reallocation, and program shutdown where
supported. A partially built aggregate destroys only completed subobjects.
Destructors are non-throwing. A second exception during exception creation,
propagation, or cleanup aborts the program immediately through the specified
termination path; it never replaces the active exception.

The design should lower control-flow exits into explicit cleanup paths before
COBOL emission. This aligns with the repository's exception-plan direction and
keeps RAII independent of native stack unwinding or debug metadata.

### 3.4 One ownership vocabulary

The core model must distinguish owning values, unique owners, shared owners,
weak observers, borrowed references, and raw interop pointers. Copying an
owner must have an explicit meaning; copying a borrowed reference must not
silently acquire ownership. The compiler should reject use-after-move,
double-delete, invalid borrow escape, and deletion through the wrong allocator
family wherever its analysis can establish those facts.

### 3.5 Native CBL-C library source is canonical

Every public standard-library implementation must live in `.cblc` source and
use CBL-C classes, templates, functions, `new`, and `delete` according to the
specified source semantics. Embedded compiler copies are generated build
artifacts and must be mechanically derived from those files. A consistency
check must fail the build if an embedded/source representation diverges.
Compiler C++ can provide generic lowering machinery, but may not reimplement a
public `vector`, `string`, `map`, runtime exception operation, or other
generated-program behavior as an undocumented special case.

### 3.6 Portability by contract, not by hope

Keep host-specific details behind compiler/runtime adapters. Library source
must not depend on POSIX calls, Windows APIs, pointer byte order, host struct
layout, non-standard COBOL verbs, or a COBOL compiler's private calling
convention. The target matrix should identify a strict common profile and
optional capability profiles. Unsupported target capabilities must be
diagnosed at configuration or compile time.

### 3.7 C++ inspiration, not C++ accidental complexity

Adopt useful contracts—value semantics, unique/shared ownership, container
invalidation rules, ordered/unordered lookup, iterator-based algorithms, and
exception safety—but do not import all C++ overload-resolution rules, template
metaprogramming, allocator customization, undefined-behavior allowances, or
ABI. Prefer a smaller set of consistent rules that can be tested in generated
COBOL.

## 4. 1.0 product scope

### 4.1 Language core required for 1.0

The release must provide a stable, versioned specification and tested support
for:

- lexical rules, expressions, control flow, classes/records, visibility, and
  deterministic diagnostics;
- value-returning functions, constructors, destructors, copy construction,
  copy assignment, move construction, and move assignment with explicit
  self-assignment and moved-from rules;
- checked primitive conversions and documented integer/floating behavior;
- complete generic class/function instantiation for the forms needed by the
  library, including nested type arguments and deterministic diagnostics;
- references/borrows and pointer operations with explicit lifetime and null
  rules;
- exceptions with typed catches, rethrow, cross-module propagation, cleanup
  on every exit edge, and the explicit double-exception abort rule;
- reliable imports, module interfaces, separate compilation, and stable
  external entry/ABI rules;
- file and console I/O adequate for ordinary command-line programs; and
- a stable build invocation and documented target profile.

Features not meeting these criteria are either rejected with a dedicated
diagnostic or marked experimental; they must not be described as generally
supported.

### 4.2 Required 1.0 container families

“C++ container coverage” means the familiar conceptual families below, not
every overload in the C++ working draft. Names and semantics should be
documented in the CBL-C namespace and tested with scalar and managed element
types.

| Family | 1.0 types | Essential contract |
| --- | --- | --- |
| Fixed/contiguous sequence | `array<T, N>`, `vector<T>` | Fixed extent or contiguous growable storage; checked indexed access; value semantics; documented iterator/reference invalidation. |
| Double-ended/node sequences | `deque<T>`, `list<T>`, `forward_list<T>` | Documented insertion/erase complexity and stability; all inserted elements constructed and destroyed correctly. |
| Ordered associative | `set<T>`, `multiset<T>`, `map<K,V>`, `multimap<K,V>` | Strict weak ordering, uniqueness versus equivalent keys, deterministic ascending traversal, immutable stored map keys. |
| Unordered associative | `unordered_set<T>`, `unordered_multiset<T>`, `unordered_map<K,V>`, `unordered_multimap<K,V>` | Hash/equality consistency, uniqueness versus equivalent keys, bucket/rehash behavior and invalidation stated. |
| Adaptors | `stack<T>`, `queue<T>`, `priority_queue<T>` | Small policy wrappers with documented underlying container and comparator requirements. |
| Ownership | `unique_ptr<T>`, `shared_ptr<T>`, `weak_ptr<T>` | Unique transfer, shared control block, weak observation/lock, correct destruction and cycle-avoidance guidance. |

The C++ container catalogue itself includes sequence, associative,
unordered-associative, and adaptor categories; it also specifies shared
requirements such as iterators, allocator-aware element construction, and
invalidation behavior. CBL-C should borrow those *categories and explicit
contracts*, not promise exact C++ compatibility. See the current C++ working
draft's [container overview](https://eel.is/c++draft/containers),
[container requirements](https://eel.is/c++draft/container.requirements),
[allocator-aware requirements](https://eel.is/c++draft/container.alloc.reqmts),
[sequence requirements](https://eel.is/c++draft/sequence.reqmts),
[ordered map overview](https://eel.is/c++draft/map.overview), and
[unordered associative requirements](https://eel.is/c++draft/unord.req.general).

The standard should not expose a packed-bit `vector<bool>` specialization in
1.0. `vector<bool>` should mean ordinary individually addressable boolean
elements; this deliberately avoids proxy references and surprising
concurrency/aliasing behavior.

### 4.2.1 Proposed method inventory for the 1.0 classes

The following is the required *API-design checklist* for the classes above,
not a claim that these methods exist today. Signatures use conceptual types
such as `size_type`, `iterator`, `optional<T>`, and references; Phase 0 must
map them to syntax and ABI forms that CBL-C actually supports. Mutators with no
useful result return `void`. A lookup that can normally miss uses an optional
value/iterator or a documented `contains`/`count`, not an output status flag.
Every exceptional failure has a named error type and must follow the uniform
error policy in Sections 5 and 8.

| Class/family | Required public methods |
| --- | --- |
| `array<T,N>` | default/value construction as supported; copy/move construction and assignment; destructor; `size`, `empty`, `at`, `front`, `back`, `data`, `begin`, `end`, `fill`, `swap`; equality where `T` supports equality. `size` is always `N`. |
| `vector<T>` | default/count/value/range construction as supported; copy/move construction and assignment; destructor; `size`, `capacity`, `max_size`, `empty`, `reserve`, `shrink_to_fit`, `resize`, `clear`, `assign`, `at`, `operator[]`, `front`, `back`, `data`, `begin`, `end`, `push_back`, `emplace_back`, `pop_back`, `insert`, `erase`, `swap`. Both `operator[]` and `at` are checked in 1.0 and throw `out_of_range` for invalid indices. |
| `deque<T>` | lifecycle/value operations; `size`, `empty`, `at`, `operator[]`, `front`, `back`, `begin`, `end`, `push_front`, `push_back`, `emplace_front`, `emplace_back`, `pop_front`, `pop_back`, `insert`, `erase`, `clear`, `swap`. No `data()` promise because storage is not contiguous. |
| `list<T>` | lifecycle/value operations; `size`, `empty`, `front`, `back`, `begin`, `end`, `push_front`, `push_back`, `emplace_front`, `emplace_back`, `pop_front`, `pop_back`, `insert`, `erase`, `remove`, `remove_if`, `clear`, `swap`; `reverse` and `sort` may be member operations if generic iterator algorithms are not yet efficient. |
| `forward_list<T>` | lifecycle/value operations; `empty`, `front`, `before_begin`, `begin`, `end`, `push_front`, `emplace_front`, `pop_front`, `insert_after`, `erase_after`, `clear`, `reverse`, `remove_if`, `swap`. Decide explicitly whether `size()` is stored/O(1), computed/O(n), or omitted. |
| `set<T>` / `multiset<T>` | lifecycle/value operations; `size`, `empty`, `begin`, `end`, `insert`, `emplace`, `erase`, `clear`, `find`, `contains`, `count`, `lower_bound`, `upper_bound`, `equal_range`, `swap`. `set` has unique keys; `multiset` permits equivalent keys. |
| `map<K,V>` | lifecycle/value operations; `size`, `empty`, `begin`, `end`, `insert`, `emplace`, `erase`, `clear`, `find`, `contains`, `count`, `lower_bound`, `upper_bound`, `equal_range`, `at`, `operator[]`, `insert_or_assign`, `try_emplace`, `swap`. These unique-key-only APIs return `pair<iterator,bool>` for insert/emplace/try_emplace; `operator[]` inserts a default mapped value only when absent and may throw during insertion. |
| `multimap<K,V>` | lifecycle/value operations; `size`, `empty`, `begin`, `end`, `insert`, `emplace`, `erase`, `clear`, `find`, `contains`, `count`, `lower_bound`, `upper_bound`, `equal_range`, `swap`. Multi-key insert/emplace returns an iterator; it does not expose `at`, `operator[]`, `insert_or_assign`, or `try_emplace`. |
| `unordered_set<T>` / `unordered_multiset<T>` | lifecycle/value operations; `size`, `empty`, `begin`, `end`, `insert`, `emplace`, `erase`, `clear`, `find`, `contains`, `count`, `equal_range`, `reserve`, `rehash`, `bucket_count`, `load_factor`, `max_load_factor`, `swap`. Hash and equality objects/operations have documented requirements. |
| `unordered_map<K,V>` | unordered-set operations plus `at`, `operator[]`, `insert_or_assign`, and `try_emplace`; unique-key insert/emplace/try_emplace returns `pair<iterator,bool>`. Stored keys are immutable; mapped-value mutation is allowed. |
| `unordered_multimap<K,V>` | unordered-set operations plus mapped-value access through iterators and `equal_range`; multi-key insert/emplace returns an iterator. It does not expose `at`, `operator[]`, `insert_or_assign`, or `try_emplace`. Rehash invalidation and exception guarantees are specified per method. |
| `stack<T>` | `empty`, `size`, `top`, `push`, `emplace`, `pop`, `swap`. `pop` returns `void`; callers inspect `top` before popping. Empty `top`/`pop` follows the documented checked-error policy. |
| `queue<T>` | `empty`, `size`, `front`, `back`, `push`, `emplace`, `pop`, `swap`. Empty access/removal follows the documented checked-error policy. |
| `priority_queue<T>` | constructors from supported ranges; `empty`, `size`, `top`, `push`, `emplace`, `pop`, `swap`; comparator is explicit/defaulted by a documented `Less` capability. Priority ordering and tie behavior are documented. |
| `unique_ptr<T>` | default/null and ownership construction; move construction/assignment; destructor; `get`, `has_value`/boolean conversion, dereference/member access, `release`, `reset`, `swap`. Copy construction/assignment are rejected. Dereferencing empty ownership throws a defined null/ownership error. |
| `shared_ptr<T>` | empty/owning construction; copy/move construction/assignment; destructor; `get`, `has_value`/boolean conversion, dereference/member access, `use_count`, `reset`, `swap`, conversion from compatible `unique_ptr`; `make_shared<T>(...)`. Thread-safety contract must match the selected runtime profile. |
| `weak_ptr<T>` | construction/assignment from compatible `shared_ptr`/`weak_ptr`; `expired`, `use_count`, `lock`, `reset`, `swap`. `lock` returns an empty `shared_ptr` after expiration; it is not an error. |
| `pair<A,B>` | construction, copy/move/assignment/destruction; `first`, `second`; `swap`; equality and lexicographic ordering when component capabilities permit. For map entries, `first` is immutable. |
| `optional<T>` | empty/value construction; copy/move/assignment/destruction; `has_value`, boolean conversion, checked `value`, `value_or`, `reset`, `emplace`, dereference/access. Empty checked `value` raises `bad_optional_access`; absence itself is not an error. |
| `result<T,E>` (if retained) | success/error construction; `has_value`, checked `value`, checked `error`, `value_or`, `map`/`and_then` only if generic callable support is ready. The error alternative must never be silently discarded. |
| `variant<T...>` (if retained) | value construction, `index`, `holds_alternative`, checked `get`, `emplace`, `swap`; visiting is a later dependency unless callable constraints are ready. Invalid checked access raises a named `bad_variant_access`. |

Do not put a method into the public surface merely because it is easy to parse.
Before it is stable, specify its complexity, value category/copy behavior,
failure guarantee, invalidation effects, and behavior for empty/boundary input.

### 4.2.2 Expanded `string` class API

The current canonical source is `standard library/string.cblc`, and its class
is spelled `string`. There is no alias that preserves the removed built-in
meaning. The class already has constructors and copy assignment plus `len`, `empty`,
`clear`, `append`, `equals`, `starts_with`, `ends_with`, `compare`, `contains`,
numeric conversions (`atoi`, `atol`, `atoll`, `strtod`), and C-like helpers
(`strlen`, `strnlen`, `strcmp`, `strcpy`, `strncpy`, `memcmp`, `strcat`,
`toupper`, `tolower`). These existing methods are a starting point, not a
complete or consistently safe 1.0 contract.

The 1.0 `string` class should own its storage, keep representation fields private,
and expose the following cohesive class-method surface. Existing helper
behavior should be folded into these methods; old free-function/ABI entry
points may remain only as thin compatibility adapters calling the class, not
as separate implementations.

| Method group | Proposed methods and contract |
| --- | --- |
| Lifecycle/value | default constructor; construction from a CBL-C string/value or explicit `(bytes, length)` input; copy/move constructors and assignments; destructor; self-assignment-safe `operator=`; `swap`. A NUL-terminated constructor stops at its first NUL; the length-delimited constructor preserves embedded U+0000. A moved-from `string` remains empty and destructible. |
| Size/capacity | `len` and explicit `byte_length` (both documented as UTF-8 bytes); `codepoint_length` (validates/decodes UTF-8); `empty`; `max_size`; `capacity`; `reserve`; `shrink_to_fit`; `clear`; `resize(new_length, fill_byte)` with UTF-8-boundary restrictions. |
| Access | checked `at(byte_index)` and `codepoint_at(character_index)`; `front`, `back`; `set_byte`/`set_codepoint`; `begin`/`end` only after iterator and borrow lifetimes are supported. Avoid exposing a mutable raw `value` field. |
| Construction/mutation | `assign`, `append`, `operator+=`, value-returning `operator+`, `push_back(ASCII byte)`, `push_codepoint(validated scalar)`, `pop_back` (removes one codepoint), `insert`, `erase`, `replace`, `replace_all`, `remove_prefix(byte_count)`, `remove_suffix(byte_count)`. Mutators return `void`; growth failure throws and leaves the documented guarantee. Prefix/suffix counts and positional APIs use UTF-8 byte offsets and must end at codepoint boundaries. |
| Search/slicing | `find`, `rfind`, `find_first_of`, `find_last_of`, `find_first_not_of`, `find_last_not_of`, `substr`; return `optional<size_type>` for normal “not found” results. Byte-offset APIs must reject offsets inside a UTF-8 code point; add explicit codepoint variants when needed. |
| Compare/predicates | `compare`, `equals`, equality/inequality and ordering operators where supported, `starts_with`, `ends_with`, `contains`, `equals_ignore_case_ascii`. Ordering is lexicographic over UTF-8 bytes unless a future Unicode collation API is explicitly added. |
| Whitespace/token helpers | `trim`, `trim_start`, `trim_end`, `split(separator)`, `join(parts)` (delimiter is the receiver); define empty separator and empty input behavior. Unicode whitespace handling must be explicit; ASCII-only methods must say so in their names/docs. |
| Case/encoding | `to_upper_ascii`, `to_lower_ascii` as in-place `void` methods; non-mutating `upper_ascii`, `lower_ascii` may return `string`. `is_valid_utf8` and `codepoint_length`; full Unicode case mapping/normalization should only be promised once versioned Unicode tables and conformance tests exist. |
| Parsing/formatting | `parse_int`, `parse_long`, `parse_long_long`, `parse_double` return the parsed value directly or throw `parse_error`/`range_error`; define strict full-consumption behavior. If prefix parsing is needed, return a two-field `Parsed<T>` value containing both value and consumed byte count. Numeric formatting methods return a new `string` and define precision/locale-independent formatting. |
| Interop | `to_c_string` returns an owned NUL-terminated allocation and rejects embedded U+0000; `copy_to(destination, capacity)` copies exactly `len` bytes without a terminator, returns the copied byte count, and writes nothing if capacity is insufficient; `view`/`data` are deferred until lifetime-checked borrows are supported. |

For 1.0, the recommended representation contract is valid UTF-8 text: constructors
from external bytes validate and raise `invalid_encoding`; mutation preserves
validity; byte offsets/counts are explicit; codepoint-oriented helpers never
split an encoded character. If arbitrary binary data is required, introduce a
separate `ByteBuffer` rather than weakening `string` semantics. This decision
must be settled before `substr`, indexing, and `len` are frozen.

Keep legacy names such as `atoi`, `strcpy`, and `memcmp` only if compatibility
requires them; give them exact CBL-C semantics and implement them by delegating
to safe class operations where semantics match. The 1.0 `string::parse_*`
methods never silently return a plausible zero for malformed input, and
bounded-copy operations never silently truncate while reporting success. If a
historical external symbol must preserve zero-on-parse-error behavior, isolate
that behavior in a versioned/deprecated compatibility adapter; it is not a
safe `string` method and new CBL-C code must not call it.

### 4.2.3 Per-method exception and post-failure contracts

These are the proposed 1.0 contracts to put into the normative standard and
then test. They make “throws an error” concrete: the trigger, error type, and
observable state after the throw are specified. An operation that is not listed
as throwing below is non-throwing by contract, except that an explicitly
user-supplied element/comparator/hash operation may throw and is handled as
stated for that family. Destructors and cleanup paths never throw; attempting
to throw during cleanup while another exception is active aborts as specified.

Error names mean:

| Error | Exact meaning |
| --- | --- |
| `invalid_argument` | An argument is not a member of the method's valid input domain (for example an empty split delimiter or invalid capacity argument). |
| `out_of_range` | An index, count, or iterator position is outside the object's current valid range, including checked access to an empty sequence. |
| `length_error` | A requested size/capacity cannot be represented or exceeds the object's/container's documented `max_size`. |
| `overflow_error` | A numeric operation's mathematically correct result is outside the destination integer/type range. |
| `bad_alloc` | Required storage cannot be obtained. |
| `stack_overflow` | The configured maximum live generated-program activations was reached before entering a call; this is the compiler/runtime activation bound, not an operating-system stack-fault report. Its fixed payload is `{limit, requested_depth}`; frame unavailability is terminal fatal, not another `stack_overflow`. |
| `invalid_encoding` | External or mutated text is not well-formed UTF-8, or a text operation would split/introduce an invalid encoding. |
| `parse_error` | Text does not match the requested complete grammar (including unconsumed non-whitespace suffix characters). |
| `range_error` | Input parses successfully but its numeric value is outside the requested destination type's representable range. |
| `domain_error` | Inputs are representable but the mathematical operation is undefined by the library contract (for example square root of a negative value). |
| `iterator_error` | An iterator is past-end when dereferenced, belongs to another container, or was invalidated by a documented operation. |
| `io_error` | A file/console/path operation did not complete; a stable subcode distinguishes not-found, permission, EOF where exceptional, disk-full, and other portable causes. |
| `format_error` | A formatting pattern or specifier is not in the documented formatting grammar. |
| `bad_optional_access` / `bad_result_access` / `bad_variant_access` | Checked access asks for a value/error/alternative that is not active. |

The error object carries a stable code and origin location where the operation
has source-location metadata. Human-readable message text is diagnostic and
may evolve; programs branch on the type/code, never message text. Normal
absence is not an exception: `find` returns empty optional, `contains` returns
false, and `weak_ptr.lock()` returns empty after expiration.

Freeze these `io_error` subcodes: `not_found` (path absent),
`permission_denied`, `already_open`, `not_open`, `end_of_file` (only for an
exact-read API; iteration EOF is normal), `short_read`, `short_write`,
`disk_full`, `invalid_path`, `not_directory`, `already_exists`,
`unsupported_operation`, and `device_failure`. I/O exception payloads report
the exact byte/record count already transferred. File position and partial
external effects are not rolled back; handle state and committed prefix are
part of each method's post-failure contract.

#### `string`

| Method(s) | Throw condition | State/result after failure |
| --- | --- | --- |
| Default constructor | None. | Empty, valid UTF-8 string. |
| Construct from bytes/value | Invalid UTF-8 -> `invalid_encoding`; storage failure -> `bad_alloc`. | No partially constructed `string` escapes; temporary storage is released. |
| Copy constructor / copy assignment | Storage failure -> `bad_alloc`. | Construction fails cleanly; assignment leaves the destination unchanged (strong guarantee), source unchanged. Self-assignment succeeds unchanged. |
| Move constructor / move assignment | None for the built-in owning representation. | Destination receives storage; source becomes empty and valid; self-move is safe. |
| Destructor, `clear`, `empty`, `len`, `byte_length`, `max_size`, `capacity`, `is_valid_utf8`, `codepoint_length`, `compare`, `equals`, `starts_with`, `ends_with`, `contains`, `equals_ignore_case_ascii`, `to_upper_ascii`, `to_lower_ascii`, `swap` | None. | No failure state; destructor releases storage exactly once. `codepoint_length` is non-throwing because stored strings satisfy the UTF-8 invariant. |
| `reserve`, `shrink_to_fit` | Above `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Original bytes, length, and capacity remain valid and unchanged on failure. |
| `resize` | Size above `max_size` -> `length_error`; shrink target inside a UTF-8 codepoint -> `invalid_argument`; non-ASCII/invalid fill byte -> `invalid_encoding`; allocation failure -> `bad_alloc`. | Original content is unchanged on failure. Successful shrink ends only at a codepoint boundary; growth uses a valid ASCII fill byte. |
| `at(byte_index)`, `front`, `back` | Byte index outside `[0, len)` -> `out_of_range`; empty `front`/`back` -> `out_of_range`. | Read-only; valid byte access may return one byte of a multi-byte code point. |
| `codepoint_at(character_index)`, `set_codepoint(character_index, scalar)` | Character index outside `[0, codepoint_length)` -> `out_of_range`; invalid Unicode scalar (including surrogate values) -> `invalid_encoding`. | Read does not mutate; setter leaves original text unchanged on error. |
| `set_byte(byte_index, byte)` | Index outside `[0, len)` -> `out_of_range`; resulting byte sequence not valid UTF-8 -> `invalid_encoding`. | Strong guarantee; original bytes unchanged on error. |
| `assign` | Invalid UTF-8 source -> `invalid_encoding`; source length above `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Strong guarantee: old value remains byte-for-byte unchanged on failure; self-assignment is a no-op. |
| `append`, `operator+=` | Invalid UTF-8 source -> `invalid_encoding`; combined-length overflow/above `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Strong guarantee: receiver unchanged on failure. Self/overlapping source is staged before mutation. |
| `operator+` | Invalid UTF-8 operand -> `invalid_encoding`; combined-length overflow/above `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Both operands are unchanged; no partial result escapes. |
| `push_back(ASCII byte)` | Non-ASCII byte -> `invalid_encoding`; size at `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Receiver unchanged on failure. |
| `push_codepoint(scalar)` | Surrogate or value above U+10FFFF -> `invalid_encoding`; encoded size above `max_size` -> `length_error`; allocation failure -> `bad_alloc`. | Receiver unchanged on failure; valid scalar is encoded as exactly one UTF-8 codepoint. |
| `pop_back` | Empty string -> `out_of_range`. | Removes exactly the final UTF-8 codepoint on success; empty/error case does not mutate. |
| `insert(byte_position, text)` | Position past `len` -> `out_of_range`; position inside a codepoint -> `invalid_argument`; invalid source -> `invalid_encoding`; resulting length above max -> `length_error`; allocation failure -> `bad_alloc`. | Strong guarantee; self/overlap source is staged. |
| `erase(byte_position, byte_count)` | Position/count outside the string -> `out_of_range`; start/end inside a codepoint -> `invalid_argument`. | No allocation is required; invalid request leaves receiver unchanged. |
| `replace(byte_position, byte_count, text)` | Same position/count and UTF-8 checks as `erase`/`insert`; combined result above max -> `length_error`; allocation failure -> `bad_alloc`. | Strong guarantee; source is staged before mutation if it aliases the receiver. |
| `replace_all(old_text, new_text)` | Empty `old_text` -> `invalid_argument`; invalid source -> `invalid_encoding`; result above max -> `length_error`; allocation failure -> `bad_alloc`. | Strong guarantee; no match is a no-op; all non-overlapping matches are replaced left-to-right. |
| `remove_prefix(byte_count)`, `remove_suffix(byte_count)` | Count greater than `len` -> `out_of_range`; count ending inside a codepoint -> `invalid_argument`. | No allocation; invalid count leaves receiver unchanged. |
| `find`, `rfind`, `find_first_of`, `find_last_of`, `find_first_not_of`, `find_last_not_of` | Start position past `len` -> `out_of_range`; a non-boundary byte position -> `invalid_argument`. | Not found returns empty `optional<size_type>`; input string is unchanged. |
| `substr` | Start past `len` or invalid count -> `out_of_range`; start/end inside a codepoint -> `invalid_argument`; result allocation failure -> `bad_alloc`. | Source unchanged; no partial result escapes. |
| `trim`, `trim_start`, `trim_end`, `upper_ascii`, `lower_ascii` (value-returning forms) | Result allocation failure -> `bad_alloc`. | Source unchanged; result is absent on throw. |
| `split` | Empty separator -> `invalid_argument`; allocation of result/vector/element -> `bad_alloc`. | Source unchanged; no partial result escapes. Empty source returns an empty vector per chosen contract. |
| `join` | Combined length overflow -> `length_error`; allocation failure -> `bad_alloc`. | Delimiter and input parts unchanged; no partial result escapes. |
| `parse_int`, `parse_long`, `parse_long_long`, `parse_double` | Invalid grammar/trailing junk -> `parse_error`; valid number outside target range -> `range_error`; invalid UTF-8 -> `invalid_encoding`. | Receiver unchanged; never returns a plausible default on failure. |
| `to_c_string` | Embedded NUL -> `invalid_argument`; allocation failure -> `bad_alloc`. | Receiver unchanged; no ownership is transferred on failure. `string` itself may contain U+0000; only NUL-terminated interop rejects it. |
| `copy_to` | Null destination when `len > 0` or negative capacity -> `invalid_argument`; nonnegative capacity below `len` -> `length_error`. | Destination is unchanged on every failure; success copies exactly `len` bytes without a terminator and returns `len`; overlap is handled with move-safe copy semantics. |
| `view`, `data`, `begin`, `end` | Deferred until borrowing/iterator rules are implemented. Once enabled, invalidated view/iterator use -> `iterator_error` in all 1.0 profiles. | No pointer/view API may silently outlive its owner. |
| Retained legacy conversion helpers (`atoi`, `atol`, `atoll`, `strtod`) | Same grammar/range exceptions as corresponding `parse_*` method. | No C-style “return zero on failure” behavior in the native API. Compatibility wrappers may translate only at the external boundary. |
| Retained raw-character-buffer interop helpers (`strlen`, `strnlen`, `strcmp`, `strcpy`, `strncpy`, `memcmp`, `strcat`, `toupper`, `tolower`) | Null pointer with nonzero requested extent, negative length/capacity, or extent exceeding the supplied buffer -> `invalid_argument`; object index outside its extent -> `out_of_range`; destination too small -> `length_error`; invalid text encoding -> `invalid_encoding`; allocation failure -> `bad_alloc`. | These operate on explicitly supplied raw character buffers, not on a compiler-created `string` type. Owning class methods use their own typed contract. A legacy ABI wrapper may translate errors only if that mapping is documented and tested. |

#### Containers and adaptors

| Class/method(s) | Throw condition | State/result after failure |
| --- | --- | --- |
| All container constructors, copy constructors, and copy assignments | Allocation -> `bad_alloc`; an element constructor/copy may propagate its original typed exception. | Failed construction exposes no object; completed elements are destroyed in reverse order. Copy assignment leaves destination unchanged if it fails; source is unchanged. |
| All container move constructors/assignments | None when only ownership handles are transferred; otherwise propagate a user element move exception according to that container's documented guarantee. | Source is valid and empty after successful move; self-move is safe. If move throws, both objects remain valid and no resource is duplicated/lost. |
| All container destructors and `clear` | No throw; element destructors are required non-throwing. A violation terminates. | Every live element is destroyed once; container becomes empty for `clear`. |
| `size`, `capacity`, `max_size`, `empty`, `bucket_count`, `load_factor`, `begin`, `end`, `data` where present | None. | No mutation. Iterators follow the validity rules for the current container state. |
| `array::at`, `vector/deque::at`, checked `operator[]`, `front`, `back`, adaptor `top`/`front`/`back` | Invalid index or empty checked access -> `out_of_range`. | No mutation. For 1.0, `operator[]` is checked as well as `at`; CBL-C does not provide an unchecked UB variant. |
| `array::fill` | Element assignment may propagate its typed exception. | Array remains fully constructed and valid; elements successfully assigned before the throw retain their new values, later elements retain old values (basic guarantee). |
| `array::swap` and container `swap` | User element swap/move may throw unless the selected type capability guarantees non-throwing swap. | Both objects remain valid; exact element-level guarantee is specified by type capability. Self-swap is a no-op. |
| `vector::reserve`, `shrink_to_fit` | Request above max -> `length_error`; allocation -> `bad_alloc`; relocation copy/move exception propagates. | Old allocation/content retained on failure. Types used in reallocating operations must be copyable or nothrow-movable; otherwise a compile-time capability diagnostic is issued. |
| `vector::resize` | Above max -> `length_error`; allocation -> `bad_alloc`; default/value construction exception propagates. | Shrink destroys removed elements. Growth failure leaves old size/content unchanged and destroys newly constructed tail elements. |
| `vector::push_back`, `emplace_back`, `insert`, `assign` | Invalid position/range -> `out_of_range`; above max -> `length_error`; allocation -> `bad_alloc`; element construction/assignment exceptions propagate. | Reallocation and range assignment use transactional storage and leave old contents unchanged on failure. In-place shifts provide the documented basic guarantee: valid size/storage and no leaks; values already moved/assigned may be changed. Self/range aliasing is supported. |
| `vector::pop_back`, `erase` | Empty pop, invalid index/iterator/range -> `out_of_range`/`iterator_error`; element move/assignment exception may propagate. | Empty pop does not change state. Erase destroys removed elements once; if shifting throws, vector remains structurally valid with the documented basic guarantee. |
| `deque` insertion, emplacement, resize, reserve-like growth | Above max -> `length_error`; allocation -> `bad_alloc`; element exception propagates. | No element loss/leak; transactional operations preserve old contents; in-place edits retain a valid deque and documented basic guarantee. Invalid position/index -> `out_of_range`/`iterator_error`. |
| `deque` pop/access | Empty pop/front/back or invalid index -> `out_of_range`. | No change when the operation throws. |
| `list`/`forward_list` insertion, emplacement, assignment | Invalid `after` position/range -> `iterator_error`; allocation -> `bad_alloc`; element construction/copy exception propagates. | New node is either fully linked or fully destroyed; old list unchanged on failed insertion. Copy assignment is transactional and leaves destination unchanged on failure. |
| `list`/`forward_list` erase/pop/front/back | Empty access/pop -> `out_of_range`; foreign/stale/end iterator or invalid `after` iterator -> `iterator_error`. | Error leaves list unchanged; success destroys erased value exactly once. |
| `list::sort` (if included) | Comparator exception propagates; temporary ordering storage failure -> `bad_alloc`. | Compare a staged node-order array and relink only after success; failure leaves original order and nodes unchanged. |
| `list::remove_if` / `forward_list::remove_if` | Predicate exception propagates. | Nodes visited before the exception may already be erased; list remains valid, unvisited suffix is unchanged, and erased values are destroyed exactly once. |
| Ordered set/map lookup (`find`, `contains`, `count`, bounds) | Comparator exception propagates unchanged. | Container is unchanged; absent key is normal (empty optional/false/zero count). |
| Ordered set/map insert/emplace/`try_emplace`/`insert_or_assign` | Allocation -> `bad_alloc`; comparator/key/value construction exception propagates; unsupported key ordering/default construction is a compile-time diagnostic. | Single-element insertion is atomic: on failure no element is inserted and all prior elements remain. Existing mapped value remains unchanged if replacement fails. |
| Ordered set/map `at`, `operator[]` | Missing `at` key -> `out_of_range`; `operator[]` on absent map key may throw `bad_alloc` or mapped default-constructor/comparator exception. | Missing `at` does not mutate. Failed `operator[]` insertion leaves map unchanged. |
| Ordered set/map erase | Invalid iterator/range -> `iterator_error`; comparator exception for key erase propagates. | Invalid request leaves container unchanged; successful erase destroys each erased element once. |
| Unordered lookup (`find`, `contains`, `count`, `equal_range`) | Hash/equality callable exception propagates. | Container unchanged; no matching key is normal absence. |
| Unordered insert/emplace/rehash/reserve | Allocation -> `bad_alloc`; size/bucket request above max -> `length_error`; invalid load factor -> `invalid_argument`; hash/equality/element exception propagates. | Insert has no effect on failure. Rehash/reserve stage a replacement bucket table and commit only on success, so allocation/hash/equality failure leaves contents and current bucket arrangement unchanged. |
| `max_load_factor` setter | Non-positive, NaN, or infinite factor -> `invalid_argument`. | Existing factor and bucket structure unchanged. |
| Unordered erase/access | Missing `at` -> `out_of_range`; invalid iterator -> `iterator_error`; hash/equality exception for key erase propagates. | Hash/equality run before unlinking; a thrown exception leaves the container unchanged. Missing `at`/invalid request does not mutate; successful erase destroys value once. |
| `stack`/`queue`/`priority_queue` push/emplace | Underlying allocation -> `bad_alloc`; element/comparator exception propagates. | New item is either fully committed or absent; adaptor invariant/order remains valid. |
| Adaptor pop/top/front/back | Empty access/pop -> `out_of_range`. | Error does not change adaptor; successful pop destroys/removes exactly one item. |
| Priority queue comparator | Comparator exception during push/pop propagates unchanged. | Push/pop rebuild a temporary heap and commit only after all comparisons succeed; failure leaves the original queue and ordering unchanged (strong guarantee). |

#### Smart pointers and utility value types

| Class/method(s) | Throw condition | State/result after failure |
| --- | --- | --- |
| `unique_ptr` construction from `new`/`make_unique` | Allocation -> `bad_alloc`; pointee constructor exception propagates. | If pointee construction fails, raw storage is released; no owner escapes. |
| `unique_ptr::get`, boolean/`has_value`, `release`, `reset`, `swap` | No throw when deleter/destructor is non-throwing. Empty `release` returns null. | `release` clears owner; reset destroys previous pointee once; swap exchanges ownership. A throwing deleter/destructor is rejected or terminates, never escapes a destructor. |
| `unique_ptr` dereference/member access | Empty owner -> `out_of_range` (documented null-owner access error). | Owner remains empty; no dereference occurs. |
| `shared_ptr` construction/`make_shared` | Allocation/control-block allocation -> `bad_alloc`; pointee constructor exception propagates. | On failure no owner/control block leaks; a successfully created object is destroyed if control-block construction then fails. |
| `shared_ptr` copy/move/get/count/reset/swap/boolean | No throw under the chosen thread/runtime profile; strong-count overflow -> `overflow_error` before changing either owner. | Ownership counts remain consistent; last strong release destroys pointee once. |
| `shared_ptr` dereference/member access | Empty owner -> `out_of_range`. | No state change. |
| `weak_ptr` construction/copy/reset/swap/expired/count/lock | Weak-count overflow -> `overflow_error` before mutation; otherwise no throw for the selected runtime profile. `lock` after expiration returns empty, not an error. | Weak count never keeps pointee alive; locking and last release follow the declared synchronization model. |
| `pair` construction/copy/assignment/swap | Member construction/copy/assignment/swap exception propagates. | Failed construction destroys completed member(s); assignment/swap provide documented basic/strong guarantee and self-assignment safety. |
| `optional::value`/dereference | Empty optional -> `bad_optional_access`. | Optional remains empty. `value_or` on empty returns fallback; copying fallback may propagate its exception. |
| `optional::emplace`/assignment | Payload constructor/copy/move exception propagates. | Strong guarantee: build new payload before replacing current one; if construction fails, old value remains (or empty remains empty). |
| `result::value`/`error` | Access to inactive alternative -> `bad_result_access`. | Result unchanged; callers may inspect `has_value` first. |
| `variant::get` | Requested type/index is inactive -> `bad_variant_access`. | Variant unchanged. |
| `variant::emplace`/assignment | Alternative construction/copy/move exception propagates. | 1.0 must specify and test one state: recommended explicit valueless state (`index == npos`) with all previous alternatives destroyed exactly once; `get` then throws `bad_variant_access`. |

#### Algorithms and hosted functions

| Method/function group | Throw condition | State/result after failure |
| --- | --- | --- |
| Iterator algorithms (`copy`, `fill`, `transform`, `reverse`, `sort`, `find`, `lower_bound`, etc.) | Invalid/cross-container/stale range -> `iterator_error`; comparator/predicate/element exception propagates unchanged. | Read-only algorithms do not mutate. Mutating algorithms leave a valid target and document prefix progress. `copy` requires disjoint ranges; overlap detected by checked iterators -> `invalid_argument`, destination unchanged. |
| Numeric/math functions | Division by zero or invalid mathematical domain -> `domain_error`; integer result outside type range -> `overflow_error`; floating result outside supported finite range -> `range_error`. | Inputs unchanged; no status side-channel; no silent saturation unless a separately named saturating API is used. |
| Parsing/formatting | Malformed input -> `parse_error`; invalid format specification -> `format_error`; parsed/printed numeric out of range -> `range_error`; output allocation -> `bad_alloc`. | Inputs unchanged; format output is not partially published. |
| File/console/path calls | Open/read/write/flush/close/path failures -> `io_error` with the defined subcode and transferred byte/record count. | Handle state/file position and committed prefix are preserved/reported; no method reports full success after partial I/O. Ordinary iteration EOF is a normal end result; exact-read EOF is `end_of_file`. |
| Date/time/random/process/environment | Invalid argument -> `invalid_argument`; value outside supported time/numeric range -> `range_error`; unavailable required OS capability -> `io_error(unsupported_operation)`; absent environment variable -> empty optional. | No silent platform-dependent fallback; deterministic random state advances only on successful generation. |

The test catalog in Section 8 must assert these exact trigger/type/state
triples. If implementation work discovers a necessary different guarantee,
change this table and its tests together before marking that method stable.

### 4.3 Useful non-container standard library for 1.0

At minimum, a general-purpose release needs the following cohesive pieces:

- **Core utilities:** `pair`, a bounded `tuple`, `optional<T>`, `result<T,E>`
  or an equivalent typed error value, a tagged `variant`, swap/equality/order
  helpers, and fixed/dynamic views (`span<T>` if its borrow rules are ready).
- **Memory:** `new`/`delete`, `make_unique`, `make_shared`, ownership helpers,
  allocation failure behavior, and an internal allocator interface. User
  allocator injection can follow after lifecycle and allocator propagation
  are stable.
- **Text:** the managed `string` class; `string_view` only when non-owning
  lifetime rules are statically and dynamically checked; UTF-8 validation and
  conversion APIs; safe formatting; numeric parse/format routines with
  typed exceptions by default (or explicitly non-throwing result variants).
- **Algorithms and iteration:** `begin`/`end`, forward and bidirectional
  iteration, sorting/searching/copy/fill/transform/reverse/min/max, predicates,
  and complexity documentation. Algorithms should operate on common iterator
  contracts instead of being duplicated as container methods.
- **Errors:** typed exception categories for invalid argument, range/bounds,
  allocation, I/O, parse/encoding, iterator, and runtime failures. The default
  for a failed operation is to throw a typed exception; a result-valued API is
  allowed only when explicitly designed as non-throwing and documented. Keep
  the rule that an exception raised while another is active terminates the
  program.
- **Hosted facilities:** stable console output/input, command-line arguments,
  environment queries with portability caveats, file/path operations,
  directory inspection where target capability exists, process exit status,
  date/time and duration basics, random number generation, and the core
  mathematical functions.

UTF-8 should be the default interchange encoding for text APIs. COBOL's
fixed-width character storage is an implementation detail and must not imply
that a byte count is a character count. Define explicitly whether `string.len`
returns encoded bytes, Unicode scalar values, or another unit; provide
unambiguous `byte_length` and character iteration where needed.

### 4.4 Explicit post-1.0 scope

Do not block 1.0 on reproducing advanced C++ facilities that are not needed for
the initial complete-program story: user-defined allocators and polymorphic
memory resources; node handles; parallel algorithms; coroutines; reflection;
template specialization/parameter packs; full locale machinery; arbitrary
execution policies; ABI compatibility with C++; and unrestricted
metaprogramming.

Threads and synchronization need a portability decision before 1.0. If no
common target/runtime contract can be implemented and tested, 1.0 must say
programs are single-threaded and defer thread APIs. It must not expose a
nominally C++-like `shared_ptr` contract that appears thread-safe while using
unsynchronized reference counts in a supported multithreaded program.

## 5. Language and standard review: decisions still required

### 5.1 Primitive representation and arithmetic

The standard currently describes `int`, `long`, and `long long` in terms of
backend-selected pictures and warns about conversions. For 1.0, define each
source type's exact range/precision in the language contract, independent of
target profile; a profile chooses only its physical representation. Prefer
fixed-width aliases (`int32`, `int64`, etc.) for serialized/inter-module data
and define ordinary integer types in terms of those aliases. Specify signed
overflow (recommended: checked and throws `overflow_error`), division and
remainder for negatives, shift limits, floating precision, NaN/infinity policy,
comparison behavior, and float-to-integer conversion. Test boundaries on each
supported COBOL compiler. The backend's typed-operation, profile projection,
numeric ABI, generated-output validation, and concrete test obligations are
specified in Sections 6.6 and 8.7.2/8.7.4; these gates must agree with the
adopted source contract and cannot be replaced by a successful parse or a
picture-marker assertion.

### 5.2 Value categories, references, and moves

Containers and smart pointers need predictable move semantics, but CBL-C need
not reproduce C++ value categories wholesale. Define an explicit move operation
or move context; after moving, the source must remain destructible and be in a
specified valid state. Copying should recursively call copy operations, not
blindly copy bytes when a class owns memory. Self-copy/self-move assignment
must be safe and tested. The language contract must also decide which return
expressions copy, move, or construct directly into the result destination;
backend or target-profile heuristics cannot independently add copy elision or
implicit moves. Any permitted elision is a source-semantic rule with explicit
observable-lifetime conditions and profile-independent behavior. Borrows may
not outlive owners; the compiler should
reject returned references to locals and stored views that outlive their
referents when provable.

### 5.3 Exception safety and RAII

Require a cleanup-lowering/CFG stage that enumerates every leaving edge and
constructs reverse-order cleanup paths. Define guarantees per library
operation: no-effect on failure where feasible (strong guarantee), otherwise
valid unchanged/partially changed state (basic guarantee) documented exactly.
For vector growth, allocate new storage, construct/move elements one at a time,
and on failure destroy only the successfully constructed new elements and
retain the old vector; only after success destroy old elements and publish new
storage. No bytewise relocation for non-trivial elements.

The exception plan already establishes a COBOL-native context/propagation
approach and an explicit fatal path for a second exception during propagation
or cleanup. Promote that rule to the normative language standard and test it
for direct throw, called function, constructor, destructor, and library
operation failure. A C++ working-draft reference describes reverse-construction
order during stack unwinding and termination when destruction during
unwinding throws; CBL-C's generated cleanup is a different mechanism but must
make its own rule at least as explicit. See [C++ stack unwinding](https://eel.is/c++draft/except.ctor)
and [C++ termination conditions](https://eel.is/c++draft/except.terminate).

### 5.4 Generic programming and templates

Complete the current bounded monomorphization model before writing a large
template library. Add structural canonical type references throughout the
semantic model; preserve immutable templates and instantiation source spans;
cache concrete types deterministically; check every instantiation; bound
recursion/depth; support nested and multi-parameter types; and diagnose
unsupported traits explicitly. Add a small constraint/capability system (for
example, `Copyable`, `Movable`, `Equatable`, `Hashable`, `Ordered`) before
containers are advertised. Do not infer requirements from template body errors
or accept types that fail only during COBOL generation.

### 5.5 Modules, ABI, and public compatibility

Define stable module identity, visibility, dependency ordering, cycle handling,
and exported generic definitions. Separate the source API compatibility policy
from COBOL ABI compatibility. Version exported signatures whenever layout,
exception context, ownership, or calling convention changes. Keep internal
COBOL names deterministic and collision-safe, but do not make generated names
the public source contract.

The compiler's by-reference call model and trailing result slot must be
specified once. Design the 1.0 ABI around a single canonical ordering for
logical arguments, return storage, and optional exception context; isolate
legacy status-based entry points in adapters. Public `.cblc` methods should
return their useful value directly and return `void` when there is no useful
result. By default, a failed operation throws a named exception; a
result-valued alternative must be explicitly specified as non-throwing and
must never silently discard its error. Do not return a one-field wrapper
struct merely to carry a single scalar.

### 5.6 Text, strings, and buffers

Specify whether `char` is a byte, character, or target character unit; literal
encoding; embedded NUL handling; string capacity and length units; slicing;
comparison; normalization policy; and conversion errors. Keep `string` an
owning value class with methods in one canonical class source, no public
status-output parameters, and deterministic cleanup on all edges. Raw buffers
should require explicit length/capacity and be visibly lower-level. Never
silently clamp requests and report success.

### 5.7 Files, platform profile, and hosted behavior

Adopt a capability-based runtime interface: `filesystem`, process/environment,
wall/monotonic clock, random, and terminal functionality are exposed only
where the selected profile implements the documented behavior through native
COBOL lowering. The compiler/tool environment is separate from the generated
program's declared environment. Paths are values, not strings with assumed `/`
separators. File errors are named and portable; native error numbers may be
included as opaque diagnostic detail, not as portable control flow. Specify
text newline conversion and binary file byte semantics. Test Windows and POSIX
behavior with the same public CBL-C tests.

### 5.8 Diagnostics and developer experience

1.0 should include stable diagnostic codes, source spans for every user error,
an option to promote warnings to errors, readable template instantiation
traces, generated COBOL/source maps, and a formatter or enforced canonical
format. A package/dependency manifest and a reproducible build command should
be designed early even if a remote package registry is deferred. Provide
examples for a command-line tool, file processor, and multi-module program.

## 6. Standard-library implementation architecture

### 6.1 Generic element lifecycle contract

Before any container is called general-purpose, every container operation must
use generated/type-directed operations equivalent to:

```text
construct(destination, source?)
copy_construct(destination, source)
move_construct(destination, source)
copy_assign(destination, source)
move_assign(destination, source)
destroy(value)
```

These are semantic operations, not a requirement to expose those exact
functions publicly. The compiler must know whether a type is trivially
copyable, copyable, movable, and destructible, and generate the right element
loop. Managed `string` and a user class owning `string` must work in `vector`,
`array`, maps, and smart pointers. Destructor dispatch must be correct for
nested fields and arrays. Pointer-bearing layouts that cannot safely be
lowered must be rejected early.

Use a single allocation family (`new`/`delete`) with matching element
construction/destruction. Never pair those with `malloc`/`free`. Introduce an
internal allocator facade for portability and testing, even if user-configured
allocators are deferred. Allocation failure policy must be consistent across
the language and library.

Storage allocation is not object construction. In capacity-based containers,
`capacity` counts available storage slots while `size` counts fully constructed
objects; unused capacity must not acquire user-visible lifetimes just because
the backing block was allocated. Implement raw slot allocation where the
portable COBOL representation permits it, or use opaque slot records whose
bookkeeping initialization does not call `T`'s constructor. A per-slot
constructed flag is metadata for actual `T` lifetimes, not permission to
default-construct every capacity slot. Construction flags are committed only
after the corresponding constructor succeeds. This distinction is required
to unwind partial construction and to avoid calling a user destructor for an
object that never became live.

For `vector<T>`, make these operation rules explicit in the implementation
kernel and test ledger:

1. `reserve` allocates replacement storage, then copy-constructs each live
   element or move-constructs it only when the type's exception guarantee
   permits. It never default-constructs elements merely to fill capacity.
2. Reallocation is transactional. On failure, destroy the successfully staged
   destination prefix in reverse order and leave the old vector unchanged;
   after commit, destroy each old live element exactly once, then release its
   storage. Require copy construction or non-throwing move construction for a
   strong-guarantee reallocation; otherwise diagnose the unsupported operation
   or specify a separately tested basic guarantee.
3. Growth constructs only the new tail and increments `size` after each
   successful construction. If any element construction fails, destroy the
   constructed tail and preserve the old size/content. Shrinking `resize`,
   `pop_back`, `erase`, and `clear` destroy removed live elements exactly once
   before reducing the live range; destruction order is reverse construction
   order for bulk teardown.
4. In-place insertion/erase may use type-directed move construction and
   assignment, but must keep a valid live range at every throwing edge. The
   erased/final moved-from slot is destroyed once, and shifts must not be
   implemented as raw byte copies for non-trivial `T`.
5. Copy construction builds a separate live range; copy assignment stages a
   complete replacement before commit and is self-assignment-safe. Move
   operations transfer ownership only when that preserves the type's stated
   moved-from and exception guarantees. All public representation fields,
   including relocation scratch state, remain private.

These are observable lifetime rules, not merely a request to call destructors
somewhere. Counted test types must prove constructor, copy/move, assignment, and
destructor traces against the live range and allocation ledger.

### 6.2 Iterator and invalidation model

Specify a small progression of capabilities rather than claiming full STL
iterator semantics immediately:

1. input/read traversal;
2. forward traversal;
3. bidirectional traversal;
4. random-access traversal for contiguous/vector-like storage.

Algorithms declare the weakest capability they require. Iterator validity and
invalidation must be specified per operation. In debug builds, iterators should
carry enough owner/generation information to detect cross-container use,
past-end dereference, and stale use after reallocation/erase; report a defined
`iterator_error` rather than relying on unchecked behavior. Release behavior
must still be documented. Container requirements in C++ explicitly make
iterators part of shared container contracts; ranges generalize operations
over those contracts. See [C++ container requirements](https://eel.is/c++draft/container.requirements),
[iterator library](https://eel.is/c++draft/iterators.general), and
[range requirements](https://eel.is/c++draft/range.req).

### 6.3 Container implementation choices

These are implementation recommendations, not observable source guarantees:

- `array<T,N>`: inline/fixed-size representation; `size() == N`; construction
  and destruction for all N elements; zero-length arrays defined.
- `vector<T>`: contiguous dynamic storage, size/capacity, geometric growth,
  transactional reallocation, no raw byte relocation for managed values,
  checked `at`, and the selected 1.0 behavior for `operator[]` written into
  the spec.
- `deque<T>`: segmented storage, avoiding whole-container relocation on
  end-insertion; choose and test a representation that can be expressed using
  portable COBOL groups and runtime allocation.
- `list<T>` / `forward_list<T>`: individually allocated nodes and stable
  references except erased elements; deterministic node cleanup. Do not
  implement these as linked COBOL paragraphs or rely on host layout.
- `map` / `set`: balanced search tree (AVL is a reasonable first implementation
  if simpler to verify than red-black balancing); comparator defines a strict
  weak ordering; never expose balance/tree shape.
- `unordered_*`: bucket array plus separately allocated nodes is recommended
  initially because it makes node lifetime and references easier to specify;
  hash/equality consistency is a type contract. Define load factor, rehash,
  iteration ordering (unspecified but deterministic for fixed implementation
  and insertion sequence, if practical), and invalidation.
- `stack`, `queue`, `priority_queue`: thin adaptors over tested sequence
  containers, not independent storage implementations.
- `unique_ptr<T>`: nullable unique owner; non-copyable; move transfers owner
  and leaves source empty; destructor calls the correct generated destructor
  and `delete` exactly once. The C++ standard's key ownership property is that
  destruction of the owner disposes of its pointee and that it is movable but
  not copyable; see [unique ownership](https://eel.is/c++draft/unique.ptr).
- `shared_ptr<T>` / `weak_ptr<T>`: one allocated control block with strong and
  weak counts, dynamic type/deleter information, and exactly-once destruction
  after the last strong owner. `weak_ptr.lock()` either obtains a live strong
  owner or returns empty. Document that shared ownership cycles require weak
  edges. C++ likewise pairs last-owner destruction with a weak observer model;
  see [shared ownership](https://eel.is/c++draft/util.smartptr.shared) and
  [weak ownership](https://eel.is/c++draft/util.smartptr.weak).

For every operation, document average/worst-case complexity where meaningful,
failure guarantee, element requirements, and iterator/reference invalidation.
The C++ requirements explicitly model element construction/destruction and
allocator participation; CBL-C should retain this rigor even if it initially
has one internal allocator policy.

### 6.4 Standard-library source and generated embedding

Keep one source unit per logical class/family where practical (for example,
`string.cblc`, `vector.cblc`, `map.cblc`) and avoid scattering a class across
helper translation units merely to satisfy the embedder. Related internal
algorithms may be separate private `.cblc` modules. Build generation should:

1. enumerate canonical `.cblc` sources;
2. embed them as immutable build output;
3. attach source identity and version to each embedded unit;
4. compile them through the same parser/semantic/lowering path as user source;
5. reject duplicate or stale embedded definitions; and
6. test the generated COBOL and executable behavior.

Small C++ registries or ABI/process-boundary shims remain permissible only
when they connect compiler internals to generated programs; they may not
implement runtime behavior on the generated program's behalf. Document each
such boundary and test that it contains no independent public semantics.

### 6.5 Nested templates, lifecycle composition, and call dispatch

Nested generic values are a 1.0 correctness requirement, not a stress-test
nice-to-have. A successful `vector<T>` implementation is not proven by
`vector<int>` or even `vector<string>`; every generic layer must correctly
instantiate and invoke the layer below it. CBL-C 1.0 should guarantee at least
16 recursively nested generic type constructors (for example,
`vector<vector<...vector<string>...>>` with 16 `vector` layers). This is a
minimum supported depth, not permission to stop testing at four or five.
Depth beyond the published limit must fail during semantic analysis with a
stable diagnostic and a full instantiation trace, never during COBOL emission
or execution. The standard profile must support at least 16 layers; an
explicit profile may raise the limit. The limit must not depend on host
call-stack size.

Use a structural specialization graph, not string substitution or runtime
type guessing. For `V0 = vector<string>`, `V1 = vector<V0>`, through
`V16 = vector<V15>`, each node is keyed by the canonical template definition,
canonical argument type tree, ABI version, and relevant capabilities. A
template resolver must:

1. collect the generic definition and source spans before instantiation;
2. substitute all class and method type parameters using structural type IDs;
3. resolve member/operator overloads against the fully specialized receiver
   and argument types;
4. recursively check required capabilities (`Copyable`, `Movable`,
   `Destructible`, `Equatable`, `Ordered`, `Hashable`) and report the exact
   failing type layer, definition, and use site;
5. memoize each concrete specialization once, with deterministic keys and
   collision-safe COBOL names; and
6. emit a dependency graph in child-before-parent order for declarations and
   generated lifecycle routines, while retaining source-level call order.

Cycle detection must distinguish an invalid non-terminating expansion (each
instantiation creates a new, deeper type) from a valid already-cached
specialization. A repeated canonical key reuses the existing specialization;
it is not regenerated. Unsupported recursive value layouts or pointer shapes
get a semantic diagnostic. A depth limit, recursive-template cycle, missing
specialization, and COBOL-name collision are distinct errors with distinct
diagnostic IDs.

Every concrete type has type-directed lifecycle operations. For
`vector<vector<string>>`, the exact dispatch is:

| Operation | Outer specialization | Element specialization | Leaf operation / ordering |
| --- | --- | --- | --- |
| Default construct | Initialize empty `vector<vector<string>>` metadata; no element is constructed. | Not called for an empty vector. | No `string` call. |
| Append/insert an inner vector | `vector<vector<string>>::push_back/insert` validates/grows outer storage. | Construct the selected `vector<string>` element by its copy/move constructor (or direct constructor for `emplace`). | Only the inner vector's own construction/copy/move decides whether its `string` elements are visited. |
| Copy construct outer | Allocate staged outer storage; visit source elements in increasing index order. | Call `vector<string>::copy_construct` for each completed outer element. | Each inner copy visits its `string` elements in increasing order. On failure, destroy only the completed staged prefix, recursively in reverse order; source is unchanged and no partial destination escapes. |
| Copy assign outer | Build a complete staged copy using the copy-construction path, then commit/swap. | Each staged inner vector is copied by its generated copy routine. | Each leaf `string` uses `string::copy_construct`; after commit the old destination is recursively destroyed. Any copy failure preserves the old destination and source. Self-assignment is a no-op. |
| Move construct/assign outer | Transfer outer storage/size/capacity ownership and leave source empty/valid. | For storage-stealing vector moves, do not copy or individually move children. If a specified move form must relocate, call the inner move routine per element. | Inner move transfers its storage; no bytewise copy of a `string`. Self-move is safe. |
| Grow/reallocate outer | Allocate replacement outer storage and stage every element before publishing it. | Relocate each inner vector through its declared copy-or-nothrow-move capability. | If an inner vector itself relocates, it calls `string` lifecycle operations. On failure, recursively destroy staged elements and keep the old outer vector valid and unchanged. |
| Erase/clear/destroy outer | Visit live outer elements in reverse construction order, then release outer storage. | Call each live `vector<string>::destroy` exactly once. | Each inner vector destroys live `string` elements in reverse construction order and releases its own storage before returning. |

The rule generalizes recursively: `construct<T>`, `copy_construct<T>`,
`move_construct<T>`, assignment, and `destroy<T>` must resolve by the concrete
type ID. A container must never use `memcpy`, a scalar fallback, or the
outermost template's lifecycle operation for a nested non-trivial element.
Generated names and call edges must show the full concrete specialization;
debug traces should include specialization ID, nesting depth, element index,
and lifecycle operation.

Operator and member-call chains must be resolved one receiver at a time at
compile time and lowered without accidental copies. For example,
`vv.at(i).at(j).append(s)` where `vv` is `vector<vector<string>>` must resolve
to this chain:

1. `vector<vector<string>>::at(i)` checks the outer index and yields a borrow
   of the existing inner vector;
2. `vector<string>::at(j)` checks the inner index and yields a borrow of the
   existing `string`;
3. `string::append(s)` mutates that string under its documented guarantee.

Neither intermediate `at` may copy or move the nested object. A failure at
step 1 or 2 is `out_of_range` and prevents later calls; a failure in step 3
propagates its exact `string` error and leaves the target string unchanged.
Similarly, `vv[i][j]` resolves two checked `operator[]` calls by the static
type at each step, then the final operation's actual overload. Operator
precedence and evaluation order are source-language rules, not inferred from
the generated COBOL paragraph order. If reference/borrow chaining is not
supported for a form, reject it at compile time rather than silently
materializing a copy.

The COBOL call graph for every specialization must be explicit in the
expectation manifest: caller specialization, callee specialization, operation,
argument/result layout, exception frame and parent-frame identity, state
transition, and cleanup continuation.
The validator must verify that an outer vector calls the exact nested-vector
lifecycle routine, that the nested routine calls the exact `string` routine,
and that no unresolved generic name, wrong-depth routine, duplicate symbol,
or scalar byte-copy path appears. This makes nested dispatch reviewable without
guessing from generated names.

### 6.6 Compiler backend and lowering architecture

The compiler backend is compiler infrastructure, not a second runtime. Its
responsibility is to translate already-resolved CBL-C operations into
deterministic, profile-conforming COBOL while preserving source evaluation,
ownership, exception, and ABI semantics. It must not implement a standard
library operation in C++ or call a hidden C++ runtime to make generated code
appear correct. The emitted COBOL and canonical `.cblc` library bodies remain
the only sources of observable program behavior.

#### Current boundary and architectural problem

The current CBL-C compiler assembles its implementation through
`src/transpiler/transpiler_cblc.cpp` and the `src/cblc/transpiler_cblc_*.inc`
files. Parsing records declarations, types, and statements in
`t_cblc_translation_unit`; `t_cblc_statement` is a tagged statement with
fixed-size text fields. The COBOL generator and statement emitters then perform
name substitution, call/lifecycle lowering, control-flow handling, declaration
emission, and text rendering in the same backend path. Exception cleanup
targets are attached to statements during parsing, and the emitter emits COBOL
through text-builder calls. This is a useful working compiler structure, but
it couples semantic decisions to output spelling and makes independent
validation of ownership and control-flow transformations difficult.

Do not replace this backend in one rewrite. Introduce explicit, testable pass
boundaries incrementally; adapters may initially build the new representations
from the existing translation unit. Keep each migration step behaviorally
equivalent for already-supported programs, and retain the old path only until
the new path passes the same contract and generated-program tests. The
following are compiler-internal representations and pass contracts, not new
source-language features.

#### Repository-specific responsibility map

The current files are implementation seams, not a requirement to preserve the
current file boundaries. `src/transpiler/transpiler_cblc.cpp` includes the
`src/cblc/transpiler_cblc_*.inc` implementation pieces into the compiler
translation unit. The important migration is to give each semantic decision
one owner; splitting files without separating decisions is not sufficient.

| Concern | Current implementation evidence | Intended owner after backend migration |
| --- | --- | --- |
| Parse declarations, class/template bodies, expressions, statements, scopes | `transpiler_cblc_unit_parse.inc`, `transpiler_cblc_declarations.inc`, `transpiler_cblc_struct_parse.inc`, `transpiler_cblc_expression_parse.inc`, `transpiler_cblc_statement_parse.inc`, `transpiler_cblc_lifecycle_parse.inc`, and `transpiler_cblc_pointer_parse.inc` populate `t_cblc_translation_unit`. | Parser/source IR. Preserve source spans and syntax structure; do not make target COBOL names or select COBOL ABI while parsing. |
| Resolve references, methods, overloads, and templates | `transpiler_cblc_references.inc` and `transpiler_cblc_struct_parse.inc` perform much of reference/type/template work; `transpiler_cblc_call_match_helpers.inc` also participates in argument matching. | Semantic graph and monomorphizer. Resolve every source reference once to canonical symbol/type/specialization IDs; emit typed diagnostics before target lowering. |
| Import/export and cross-module call contracts | `transpiler_cblc_context_integration.inc` registers exports, imports stubs, and resolves cross-module calls; COBOL call emitters also marshal arguments. | Module/signature table plus ABI-lowering pass. Context integration owns canonical imported signatures; ABI lowering owns only their concrete target calling convention. |
| Scope exits and lifecycle cleanup | `transpiler_cblc_data_call_helpers.inc` records cleanup targets on statements; `transpiler_cblc_lifecycle_parse.inc` adds constructor-related cleanup; lifecycle and function emitters turn these facts into output. | CFG/lifetime pass. Derive object-completion and cleanup operations from control-flow edges; remove parser-authored per-statement cleanup lists after parity tests pass. |
| Exception and result propagation | `transpiler_cblc_statement_parse.inc` records throw/catch facts; `transpiler_cblc_context_integration.inc` handles module signatures; `transpiler_cblc_cobol_emit.inc` and `transpiler_cblc_generate.inc` emit context fields, handlers, and termination paths. | CFG owns semantic success/failure continuations, catch/rethrow edges, and cleanup routing. Logical ABI lowering assigns versioned slot/context/payload contracts; target-profile lowering supplies their physical layout and COBOL representation. Neither stage may introduce unmodeled lifetime effects. |
| Call, pointer, and lifecycle COBOL lowering | `transpiler_cblc_substituted_emit.inc`, `transpiler_cblc_call_match_helpers.inc`, `transpiler_cblc_lifecycle_emit.inc`, and `transpiler_cblc_cobol_emit.inc` contain overlapping lowering/marshalling work. | Typed target-neutral operation lowering followed by a COBOL-profile lowering pass. These passes receive resolved IDs and explicit effects; they do not rediscover source names from statement text. |
| COBOL data layout, paragraph order, and final text | `transpiler_cblc_storage.inc`, `transpiler_cblc_text_helpers.inc`, `transpiler_cblc_generate.inc`, and the COBOL emitter build names, declarations, paragraphs, and text. | Target layout/name allocator, structured COBOL module IR, then pure renderer. The name allocator returns validated identifiers; the renderer serializes existing nodes only. |
| Output and executable evidence | `transpiler_validate_generated_cobol()` and tests under `tests/compiler/cobol/` provide structural and compile/run checks; generated-COBOL manifest/verifier work is planned in Section 8.7. | Independent validator and end-to-end harness. Compare IR-derived expectations to the artifact, then compile and execute it with required profiles. |

Several current decisions cross these rows. For example, a call may be
recognized or partially matched in parsing, checked again by a helper,
resolved across modules, then assigned a COBOL result/context layout by an
emitter. The migration inventory must identify such duplicate decisions
individually. For each one, select the canonical owner, add a pass-level test,
make downstream stages consume its resolved result, and delete the duplicate
only after the same tests cover local, imported, generic, and throwing calls.
Do not retain a string-based “last chance” resolver in COBOL emission: it
reintroduces ambiguity after semantic analysis.

The initial parsed translation unit may remain the parser's public/internal
boundary while typed IR and CFG are introduced. It must be treated as
immutable after semantic finalization. Each derived representation carries a
stable source-unit ID and references source spans rather than duplicating
unbounded source text. Pass results are either a complete immutable output
representation or a diagnostic; a failed pass must not leave partially
resolved specializations, half-emitted modules, or reusable stale output in the
translation unit.

##### Minimum typed-IR record contract

Phase 1 must define a versioned compiler-internal schema before adding more
operations. The schema is not a serialized ABI and must not reuse source text,
COBOL names, or host pointers as identity. At minimum, a function body contains
structured regions (so branches and loops remain explicit), an ordered
operation list per region, and references to canonical symbol, type, template,
and source-unit records. The following is the required information, regardless
of the eventual C++ struct/class layout:

| Record | Required fields and meaning |
| --- | --- |
| Typed value | Function-local value ID; canonical structural type ID; category (`scalar`, `owned_object`, `borrow`, `pointer_or_handle`, or an explicitly modeled compiler-only value); const/mutable access; ownership/lifetime token where applicable; and ordered provenance origins. A value definition is immutable; mutation occurs through a place. |
| Place | Base storage/value ID; typed field/index/dereference projection chain; resulting canonical type ID; access mode; and borrow/owner relationship. A place identifies where a read/write occurs and is never interchangeable with an owned value. Each projection names its resolved field/operator or a dependent lookup record until specialization. |
| Operation | Typed opcode; ordered operand IDs and place IDs; zero or more result IDs; resolved callee/symbol ID or a dependent lookup record (never both); explicit evaluation-order edges within the region; effect summary; outcome/failure semantics; and provenance. Source spelling may be retained for diagnostics, but emission cannot parse it to recover meaning. |
| Effect summary | Independently state reads/writes (including receiver mutation), allocation/deallocation, construction/destruction, possible failure, and exception effect (`non_throwing`, `may_throw` with a known set, or `unknown`). Absence of a fact is not equivalent to an empty effect. Effects must be conservative for unresolved imported bodies. |
| Operation outcome | Distinguish normal completion from each failure category the language observes, including thrown exception, allocation failure, checked-access failure, and non-throwing fatal termination. Record whether each result/object becomes initialized, live, moved-from, or remains uninitialized on each outcome. CFG construction later assigns concrete successor block IDs. |
| Provenance | Primary origin plus ordered related origins, each with a role and canonical module/source-snapshot identity. Generated operations without a direct token use a compiler-generated origin tied to the triggering source operation/declaration; they must not borrow a nearby arbitrary span. |

IDs are unique within their owning function/representation and deterministic for
the same finalized source and specialization graph. Their derivation must not
depend on allocation addresses, hash-table order, worker completion order, or
COBOL spelling. IDs are not compatibility keys: persistent cache and ABI
identity use the canonical source/type/specialization keys and explicit schema
version. Any transformation that creates or removes an operation must update
its provenance and references atomically.

The typed-IR verifier runs after construction and after every transformation.
It rejects duplicate or dangling IDs, use-before-definition outside the
structured region rules, type/projection mismatches, writes through immutable
places, owned values used as borrows or vice versa without an explicit
conversion, lifecycle operations without a concrete or explicitly dependent
target, calls with an incomplete lookup context, missing effect/outcome data,
and operations whose result/liveness state is undefined on a modeled outcome.
It also proves that evaluation-order edges are acyclic and include every
required source sequencing relation; mutually exclusive branch regions do not
gain an artificial cross-branch order. Diagnostics identify the verifier rule,
operation ID, source origins, and relevant canonical types. A verifier failure
is an internal compiler diagnostic and blocks specialization/CFG construction;
it cannot be downgraded to a warning or repaired by the COBOL emitter.

The transition from `t_cblc_translation_unit` is an adapter, not the permanent
semantic contract. During migration, the adapter must reject a statement whose
fixed-size text was truncated or whose meaning cannot be represented
structurally. Add the operation family only after its parser/source forms,
semantic checks, typed-IR verifier rules, and negative cases are specified.
The initial vertical slice should include literal/load/store, scalar and class
construction, method/operator call, explicit copy/move, checked member/index
access, return, throw, and scope exit; each must carry enough data to generate
both normal and failing paths without re-reading source text.

#### Required pass pipeline

| Pass | Input and output | Required invariant before advancing |
| --- | --- | --- |
| 1. Source/semantic unit finalization | Parsed translation unit; finalized symbol/type/module graph. | Every identifier, member, overload set, template argument, and call resolves to canonical symbols and structural type references. Non-dependent calls select one declaration here; dependent calls retain a complete canonical lookup context for specialization. No spelling-based fallback may choose a different class, overload, or ABI. Diagnostics preserve source span and instantiation/import trace. |
| 2. Typed source-operation IR | Finalized unit; typed values, addressable places, and operations with explicit result categories, evaluation order, source origins, and structured effects. Generic definitions may retain type-parameter-dependent operations. | No expression is represented only by text. Values and places are distinct; each call/operator/lifecycle intent identifies a canonical declaration or a dependent lookup record plus its substitution environment; owned values, borrows, and handles are distinguished; numeric operations retain exact canonical operand/result type IDs, conversion and rounding rules, overflow/domain policy, and typed failure outcomes; the staged IR verifier accepts all def-use/type/effect/sequence invariants. A dependent target is not mistaken for a concrete dispatch decision. |
| 3. Generic specialization and concretization | Typed source-operation IR plus canonical type graph; structurally specialized functions/types and concrete typed operations. | Instantiate structurally and memoize by canonical type/substitution keys before constructing CFG. Resolve dependent overloads, conversions, operators, and lifecycle capabilities against each concrete receiver/element type, assigning exact canonical callee IDs. By-value layout/lifecycle dependencies are validated and ordered child-before-parent; indirect lifecycle and ordinary call edges remain explicit and may form call-graph SCCs. No unresolved type parameter, dependent lookup, generic routine name, or scalar fallback enters later passes. Nesting overflow and non-terminating expansion fail semantically. |
| 4. Control-flow graph (CFG) construction | Concrete specialized functions/methods; normalized basic blocks, explicit terminators, and typed edges. | `if`, loops, return, break, continue, throw, catch dispatch, adopted short-circuit logical operators, and every may-fail operation's declared outcomes are explicit for the concrete call/effect graph. May-fail operations end their block with an invoke-shaped terminator; normal results occur only on success edges. Each block has exactly one terminator and a defined successor set; reachable paths terminate or loop intentionally. Scope entry/exit and active exception regions are attached to edges, not inferred later from paragraph text. |
| 5. Ownership and lifecycle analysis | Concrete typed CFG; lifetime facts and selected concrete lifecycle operations. | Each object/element transitions through uninitialized, constructing, live, moved-from, and destroyed states as applicable. Construction completion is recorded only after successful construction; every exit edge has reverse-order cleanup for exactly the live objects. Borrowed values cannot outlive owners. Every lifecycle call targets the exact specialization selected in Pass 3. Invalid ownership/layout is rejected before target lowering. |
| 6. Exception and logical call-ABI lowering | Ownership-aware concrete CFG; explicit logical calls, result values, exception effects, and payload type contracts. | Establish the versioned logical call contract, slot order, ownership transfer, result completion, exception-context semantics, catch/rethrow behavior, and fatal double-exception behavior. Record `interface_version` and `logical_call_contract_version`, but do not select the physical `abi_version`. It does not choose `PIC`/`USAGE`, pointer representation, physical field layout, or COBOL spelling. All effects and cleanup edges already exist in the semantic CFG. |
| 7. Target-profile and physical ABI lowering | Logical-ABI-lowered CFG plus immutable target-profile manifest and export-declared physical `abi_version`; profile-specific operations, physical ABI projection, and complete name-request set. | Read the physical `abi_version` pinned by export metadata and require an exact projection for that version in the selected target-profile manifest before projecting slots. Imported consumers preserve the provider interface's declared version; they do not select or upgrade it. Missing, contradictory, or unsupported version/profile declarations fail closed. Map each logical slot and operation, including each numeric type/operator/conversion tuple and its guard/intermediate/rounding strategy, to the declared physical COBOL representation. Validate pointer/dynamic-storage capability, pictures/usages, call/linkage convention, encoding, request-level name/line feasibility, and supported verbs. Produce the profile-specific layout projection and ABI-name requests; do not allocate collision-resolved names or fingerprint the final descriptor yet. Unsupported lowering returns a source-mapped diagnostic; it never silently changes semantics. |
| 8. COBOL module IR and final ABI/name validation | Profile-lowered module, logical descriptor plus profile projection and selected `abi_version`, and complete canonical name-request set; produces the frozen name table, composed ABI descriptor/fingerprint, and validated structured COBOL module IR. | Allocate and freeze names from the complete request set, detect collisions in the appropriate domains, seal the composed descriptor and fingerprint, and serialize module IR. `PROGRAM-ID`, `LINKAGE SECTION`, `PROCEDURE DIVISION USING`, numeric representation and operation metadata, result/context layout, paragraph calls, and external runtime declarations match that descriptor. Every declaration/reference resolves through one name-table entry; branch targets and cleanup edges resolve. |
| 9. COBOL rendering and artifact validation | The pure renderer consumes only verified COBOL module IR and the frozen name table, producing exact source bytes and a source map. The independent semantic expectation manifest is supplied separately to validation; it is never a renderer input. | Rendering is deterministic and formatting-only: it does not resolve symbols, infer lifetimes, add fallback operations, or mutate ABI choices. An independent parser reconstructs the rendered artifact; the validator compares both module IR and parsed output with the semantic manifest, then emits the versioned validation-correlation report binding expectation IDs to observed IR/output nodes and generated ranges. The renderer cannot inspect or modify expectations or correlation data to make its output pass, and neither validation artifact feeds back into generation. |

The selected target-profile ID and profile version are immutable invocation
inputs, not inferred from the host OS. Their capability facts are available to early
diagnostics, but profile-specific physical layout is selected in Pass 7 and
the final ABI fingerprint is sealed in Pass 8 after ABI-frozen names are
known. The physical `abi_version` is pinned by explicit module/export
metadata. Pass 7 checks it against an exact target-profile projection and
rejects a missing or contradictory policy; it never chooses a default,
highest, or host-derived version. An imported consumer uses the provider's
pinned version unchanged, with compatibility handled only by a separately
declared adapter. The physical version is not derived from the fingerprint
or assigned by Pass 6. Pass 8 freezes ABI-visible names before sealing the
final fingerprint. The `logical_call_contract_version` identifies
the target-neutral source-call rules independently. The order is significant.
Source-level generic lookup is resolved to
canonical declarations before lowering, while type-dependent overload and lifecycle
selection is intentionally deferred until concrete specialization. That
specialization must precede CFG construction: otherwise the CFG cannot name
the concrete callees, effects, or element layouts needed by ownership analysis
for cases such as `vector<vector<string>>`. Ownership/lifecycle analysis then
uses those selected concrete operations to derive cleanup on all normal and
exceptional edges; logical exception/ABI lowering consumes that plan, then
Pass 7 projects it into the selected profile's physical ABI. Name allocation
and profile constraints are checked before rendering, and rendering cannot
repair a semantic, lifetime, or ABI error. A diagnostic at any pass
must identify its pass, source span, relevant symbol/type or CFG edge, and a
stable diagnostic ID.

#### ABI lowering preserves the semantic CFG and lifetime proof

The semantic CFG must already include every operation that can affect source
behavior or object lifetime. In particular, evaluate call arguments once in
source order and represent value-parameter copy/move construction, temporary
stabilization, ownership transfer, and any allocation/failure behavior as
typed operations before CFG construction. Generic calls receive their
concrete argument lifecycle targets during specialization. The CFG then has
normal and failure successors for every potentially failing staging operation
as well as for the call itself; the lifetime pass tracks which staged arguments
completed and who owns each one on every successor.

Logical ABI lowering records the ordered argument slots, optional result slot,
optional exception-context slot, ownership transfer, and payload contract.
Target-profile lowering then maps those logical slots to physical linkage
fields, result storage, and exception-context representation. The profile
manifest is an explicit input, but only the target-profile pass chooses the
physical representation. Independently, it selects and fingerprints the
per-invocation storage strategy from the profile's proven activation
capabilities; this is not silently added as a source argument or reordered ABI
slot. If the chosen strategy requires an explicit activation handle/record in
the linkage ABI, that is a declared versioned ABI field and is verified with
the same exact projection rules as result and exception-context storage.
Moving a staged value into a physical argument slot
is not permission to create a second semantic copy or to transfer ownership
twice. A target profile that genuinely requires an additional
allocating/copying thunk must expose it as a typed operation with an effect,
completion state, and failure edge, then rerun CFG/lifetime verification
before accepting the module. It cannot hide that behavior in a renderer or
classify it as formatting.

Logical and physical ABI lowering may split a semantic block or edge to emit
context checks, payload marshalling, result-completion flags, and
normal/exceptional return protocols. Every new lower-level block/edge must
retain the source edge ID and provenance; projecting away ABI-only protocol
nodes must recover the same semantic successor relation, active exception
identity, and live-object state proved by the ownership pass. The post-profile-
ABI verifier checks that each semantic call-failure edge still reaches its
designated cleanup/handler/propagation path, that cleanup executes exactly
once, and that a failure never publishes or reads an incomplete result. ABI
lowering must preserve the fatal double-exception path without replacing the
first payload.

##### Specialization graph and concretization boundary

The specialization pass must produce one frozen, target-neutral concrete graph
before any per-function CFG or ownership analysis is built. A specialization
key is structural: canonical generic declaration ID, canonical type-argument
IDs, and every supported non-type argument that changes source semantics.
Source spelling, discovery order, host address, generated COBOL name, and
profile-specific physical layout are not key material. The specialization
record retains the definition/use provenance and its parent instantiation.
Interning compares the complete canonical key; a digest may accelerate lookup
but cannot stand in for equality. Parallel workers may use provisional
in-process handles while discovering recursive closures, but persistent
specialization IDs are assigned from the sorted canonical-key set at a phase
barrier (or by an equivalently specified collision-checked canonical scheme).
Remap provisional references before freezing the graph. No ID or selected
specialization may depend on which worker discovers a request first.

Register a specialization key in an `in progress` state before traversing its
body. A recursive request for that exact key adds a call edge to the existing
node; it does not recursively clone the body again. This makes ordinary
same-specialization recursion finite. Keep edge kinds distinct: `type-layout`
and `by-value lifecycle` edges describe dependencies that must be validated
before their parent can be lowered; a cycle in these edges is an illegal
by-value layout. `indirect lifecycle` and ordinary `call` edges describe
runtime dispatch and may form SCCs when the ownership and termination rules
permit it. Preserve those SCCs for later call/effect analysis rather than
requiring an impossible global topological order. If an active instantiation
chain keeps creating new structural keys until a configured depth/work bound
is crossed (for example, `F<T>` requesting `F<vector<T>>` without a
terminating specialization), report the bounded expansion diagnostic with the
complete chain. A changed key alone is not an error: structurally decreasing
recursion that reaches a finite base case is valid. Do not collapse expansion
into the legal same-key recursion case.

The pass discovers the concrete closure from non-template roots, exported or
explicitly requested specializations, and operations that require concrete
types for layout or lifecycle. It resolves dependent candidate sets, checks
constraints/capabilities, and records concrete call targets and direct effects
before sealing the graph. Compute transitive effect summaries over recursive
call SCCs to a deterministic fixed point; imported unknown effects remain
conservative. The CFG builder consumes this sealed graph and may not add a new
specialization or silently revise a selected target/effect. A newly discovered
dependency returns to the specialization worklist before CFG construction; it
never patches an already-built CFG. Physical COBOL storage, ABI slots, and
target names are still chosen later by their profile/ABI passes.

##### Effect-summary lattice and recursive call-SCC propagation

Effect summaries are sound upper bounds used to construct failure edges,
authorize transformations, and compute initialization/cleanup dependencies;
they are not optimization hints. Each summary is versioned and records an
explicit completeness state for every dimension. At minimum, dimensions are:
the finite known thrown-type set or `unknown`; allocation/deallocation;
read/write abstract memory-region sets; receiver mutation; ownership transfer;
borrow creation/invalidation; external I/O; and termination. A complete empty
set means no effect in that dimension. Missing metadata is never decoded as
empty. `unknown` is the top element for its dimension and absorbs joins there;
it need not erase precise facts already known for other dimensions.

The join is component-wise: Boolean may-effects use logical OR, known sets use
union, and `unknown` joined with any value remains `unknown`. A caller's summary
is the join of direct effects from reachable operations and the summaries of
its reachable concrete callees. For exceptions, `non_throwing` is permitted
only when the complete thrown-type set is empty and all other failure channels
are separately accounted for; a known nonempty set remains typed, and an
unknown set stays conservative. Generic definitions retain symbolic/dependent
effects until specialization; each concrete specialization gets its own
summary and cannot inherit a different specialization's result.

Build the call graph only after concrete target selection, retaining ordinary
calls and lifecycle-dispatch edges with stable IDs. Compute SCCs and their DAG
deterministically from canonical specialization IDs. Process callee SCCs
before their callers; inside each recursive SCC, initialize summaries from
direct effects and imported-interface facts, then visit nodes and outgoing
edges in canonical order until component-wise joins stop changing. The domain
is finite for a compilation because concrete operations, memory regions,
exception types, and effect dimensions are finite; joins are monotone and only
increase precision-independent may-facts, so this iteration converges to the
least conservative fixed point. Charge deterministic work for each processed
node/edge and each newly added finite fact, with checked counter arithmetic and
the invocation budget. Exhaustion is `resource_limit_exceeded`, never a
partially usable summary. Do not rely on wall-clock limits or worker completion
order to choose which SCC fails.

Imported summaries come only from the versioned interface's effect record
bound to the exported symbol/ABI contract. Missing record, unknown schema,
unknown required enum, integrity failure, or incompatible summary version
produces a conservative unknown summary (or an explicit interface diagnostic
if the contract requires that field); it never falls back to scanning a
provider source file or guessing from the function name. Unknown effects
prevent claims such as `noexcept`, pure/read-only access, no-allocation, or
safe initializer ordering. Effect summaries are target-neutral; profile
lowering may add physical failure outcomes but cannot narrow the source-level
summary. Cache keys include the canonical specialized body/call-edge closure,
imported effect-record digests, and effect-schema version. A body, target,
exception set, imported summary, or effect-rule change invalidates its SCC and
all transitive callers before CFG/lifetime consumers are reused.

Emission ordering follows dependency kind rather than imposing one impossible
global topological order: concrete value layouts and by-value lifecycle helper
dependencies are emitted dependency-first after cycle validation. Recursive
functions and indirect lifecycle helpers are emitted as strongly connected
call components with declarations or linkage as required by the target
profile. Call/lifecycle SCC membership does not make a by-value type-layout
cycle legal. Any specialization or edge that cannot be classified and
validated fails before CFG construction with a stable diagnostic.

#### Typed IR requirements

The typed IR must use stable internal IDs or canonical references for symbols,
types, blocks, operations, and ABI entries. Human-readable source and COBOL
names are attributes, never semantic identity. Each operation records at least
its source span, containing function/specialization, input and result types,
effect flags (`may_throw`, `may_allocate`, `reads`, `writes`), and required
capabilities. Operation kinds must distinguish at minimum:

- scalar computation and checked conversion;
- direct call, method call, constructor, destructor, and operator dispatch;
- borrow creation/use and pointer/null checks;
- allocation/deallocation and array-slot reservation;
- default/copy/move construction, copy/move assignment, and destruction;
- exception creation, propagation, catch binding, rethrow, and termination;
- file/runtime ABI calls and generated-library calls.

##### Values, places, effects, and typed-IR verification

Represent computations as typed definitions and uses, not as source-text
expressions copied into a statement record. Every IR value has a stable ID,
structural type ID, value category (`scalar`, `owned_value`, `borrow`, or
`handle`), and a definition point. Every storage location is a separate
`Place` descriptor with a root object ID and typed projections such as field,
array/vector element, or checked dereference. A source variable read is an
explicit load from a place; assignment is an explicit store or selected
assignment operation. This prevents a class value, a borrow to that class, and
the addressable storage of that class from collapsing into the same COBOL
identifier or textual operand.

Assign value and operation IDs deterministically in function-local definition
order. Each operation's results are defined once, and consumers refer to
result IDs.
Each operand use records its semantic mode (`read`, `copy`, `move-consume`,
`const-borrow`, `mutable-borrow`, or handle transfer) and mutability
requirements. The typed verifier rejects locally evident use-after-consume, a
mutable borrow from an immutable place, a copy without a selected copy
operation, and an ownership transfer whose source/destination state
annotations are missing. The CFG/lifetime analysis proves path-sensitive
consume, borrow, and completion validity across branches and loops.
Operations remain in source evaluation order until a separately specified
optimization proves reordering safe. Do not introduce SSA phi semantics by
guessing at COBOL paragraph joins; branch results are represented by explicit
block parameters/merge operations with typed incoming edges, and the CFG
lowerer must preserve the chosen merge contract. Side-effecting calls,
allocations, checked accesses, volatile/external I/O, and lifecycle operations
are sequencing barriers unless their effect summaries prove otherwise.

An access path records its base place/value, each projection's resolved field
or element type, and the selected bounds/null/borrow check. A dependent
projection inside a generic body records its canonical member/element lookup
and type-parameter context until concretization. The resulting category is
explicit: for example, checked `vector<T>.at(i)` produces a borrow to the
existing `T`, not an owned `T` value. Chained calls consume that borrow without
copying; a later value-context conversion must insert a named copy/move
operation with its selected lifecycle callee. Overloaded operators and
implicit conversions in concrete functions resolve to canonical callable/type
IDs before CFG construction. In generic bodies they retain a canonical
dependent-lookup record and substitution environment, then resolve during
specialization; no emitter may re-resolve them from operator text.

Effects are structured data, not a flat Boolean alone. Record at least
`may_throw` plus a known throw-set/unknown state, `may_allocate`, reads and
writes of abstract memory regions, receiver mutation, ownership transfer,
borrow creation/invalidation, external I/O, and termination. Imported effects
missing from interface metadata are conservative (unknown throw/mutation and
unproven ownership); a pass may not silently refine them to pure or
non-throwing. An operation that can fail defines its normal result only on its
success edge; the failure edge carries the typed error/active exception and
does not expose an uninitialized result. Its failure guarantee and any
partially completed construction state are explicit inputs to the CFG/lifetime
pass.

###### Abstract memory regions and alias conservatism

Effect regions are canonical semantic identities, not COBOL field names,
runtime addresses, or arbitrary source-variable spellings. Define the finite
region vocabulary before using effects for reordering or initializer planning:

| Region form | Meaning |
| --- | --- |
| `Global(module_id, symbol_id)` | One global object's storage; separate globals are disjoint unless an explicit alias/interop declaration says otherwise. |
| `ModuleState(module_id, service_id)` | Mutable state owned by one module/runtime service, including initialization state. |
| `Formal(function_id, receiver_or_parameter_id, projection)` | A summary footprint relative to a receiver or reference/handle parameter; call-site substitution maps it to the actual argument's known owner/place region. |
| `Frame(function_id, local_id, projection)` | Function-local storage that cannot escape the activation under the borrow/ownership proof. If it escapes, summarize the escaped target as heap/external rather than retaining a false frame-only effect. |
| `Heap(allocation_site_id, concrete_type_id, projection)` | A type-directed abstract heap region. Distinct runtime allocations at one site may alias this summary region; distinct site/type IDs are not assumed disjoint where pointers, shared ownership, or escaped aliases can connect them. |
| `UnknownEscapedMemory` | Any externally reachable or insufficiently modeled memory. It aliases every potentially escaped heap/formal region and blocks disjointness-based transformations. |
| `UnknownGlobalState` | Global or module state that cannot be named precisely, including incomplete imported summaries. It aliases every global/module-state region and forbids claiming no global access. |

Projection paths use canonical field IDs and an explicit element/range
abstraction. Different fields are disjoint only when the concrete layout
proves non-overlap and no union/redefinition/overlay contract applies. Container
elements default to one owner/type element-region; two indices are disjoint
only when bounds and the borrow/ownership model prove it. Type inequality or
different variable names alone never proves non-aliasing.

A call summary's `Formal` regions are substituted with the caller's actual
receiver/argument regions using the verified borrow and ownership relations.
If an argument may refer to several regions, join them all. If the mapping is
missing, external, escaped, or not proven, widen that dimension to
`UnknownEscapedMemory` (and to `UnknownGlobalState` when global reachability
cannot be excluded). Reads/writes are may-effect sets, so conflicting regions
are considered aliasing unless the verifier proves disjointness. A function
summary that stores an argument, returns a borrow, or passes it to an unknown
callee records the escape/borrow effect; it may not keep the footprint falsely
limited to its frame. This same mapping drives caller-side reordering checks,
module initializer read/write edges, and `.cbi` link-effect metadata. A dynamic
initializer with an unknown global effect is rejected before link ordering is
frozen rather than scheduled by source-file order.

Canonical region encodings and alias semantics have explicit
`effect_schema_version`, `memory_region_schema_version`, and
`alias_policy_version` fields. Encode region tags, canonical module/symbol/
function/operation IDs, and field/projection paths; never serialize a COBOL
name or runtime address as a region identity. Include these versions and the
canonical region/effect records in imported effect descriptors, private
`.cbi` `link_effects` records/hashes, recursive-SCC pass-cache keys, and
initializer/link-plan inputs. An unknown required version is rejected or
treated as dimensionally unknown according to the versioned interface rule;
it is never decoded as an empty region set. Changing a region schema or alias
policy recomputes affected call summaries, initializer read/write edges,
`.cbi` link-effect hashes, link manifests, and downstream evidence. It leaves
source-interface hashes, specialization identities, and physical ABI
fingerprints unchanged unless the public source contract or ABI itself
changed.

Distinguish a value's temporary storage from the object whose lifetime it
represents. Owned temporaries get explicit creation/completion and scope-end
facts; borrows retain their owner and invalidation generation; places do not
own storage. Full object-live-state and cleanup remain the CFG/lifetime pass's
authority, but the typed IR must provide enough information to derive those
facts without reparsing text. ABI result slots and COBOL data names are
introduced later and never serve as source-level value identity.

Run a stage-aware IR verifier after construction and after every transforming
pass. Before specialization, it checks unique IDs; definition-before-use;
exact structural types and value categories; legal place projections;
canonical direct targets; and complete dependent-lookup records for operations
whose choice depends on a template parameter. After specialization, the
verifier additionally requires every call/operator/lifecycle action to have
one concrete canonical target and rejects all remaining dependent lookups,
unresolved overloads, or open type parameters in emitted functions. Every
stage checks explicit effect/failure summaries, source-order preservation for
observable operations, well-formed merge inputs when present, and absence of
opaque expression text or COBOL spelling in semantic operands. A verifier
failure is an internal compiler diagnostic with the pass and origin chain, not
permission to fall back to the old text emitter.

The current adapter seam is concrete: `t_cblc_statement` stores fixed-size
`target`, `source`, and comma-delimited `call_arguments` text fields, while
`cblc_extract_call_argument()` is called by argument-matching and emitter
paths to parse those strings again. First build a typed expression/value
sidecar from each legacy statement exactly once; reject malformed or
unrepresentable operands rather than truncating them. Add parity checks from
the original source span through the sidecar and generated behavior. Once all
consumers use the resolved value/place IDs, delete the redundant text-based
argument extraction and substitution paths; do not make the fixed-size legacy
record the permanent typed-IR schema.

An operation that performs a class copy must name the selected copy operation;
it cannot be represented as an undifferentiated assignment. Likewise, vector
capacity allocation and construction of a live `T` are separate operations.
For a `vector<T>`, the IR must represent the live-range transition and each
element lifecycle call, not merely an assignment into an array slot. For
nested templates, lifecycle operations carry the full canonical specialization
ID and parent/child edge described in Section 6.5.

Managed allocation values are typed IR resources with distinct categories:
linear `Reservation<T, shape>`, private `StorageOwner<T>`, published
`Handle<T, shape, ownership_mode>`, and scoped
`ConstructionPlace<T>`/`DestructionPlace<T>`. There is no cast or name-based
substitution between these categories. A unique owner is linear; an explicitly
shared owner uses its declared reference-count operation; a borrowed handle is
non-owning and may be copied only while its owner and invalidation generation
remain valid.
`heap_allocate` defines a reservation only on its normal edge;
`heap_publish_object` or `heap_adopt_storage` consumes it exactly once on the
success path, while rollback consumes it through destruction of its recorded
completed set followed by release. Each CFG path must consume, transfer, or
explicitly retain a reservation; no path may leak it, consume it twice, or use
it after transfer. A published owning handle or owner token likewise has
exactly one owning identity unless an explicit shared-ownership operation
changes that count. Borrowed element handles carry the owning token and
invalidation generation but do not consume it. At control-flow merges,
incoming resource states must agree or be represented by typed block parameters
with a verified ownership transfer; the verifier may not reconcile mismatches
from local variable names or renderer storage.

##### IR transformation and optimization contract

No optimizing transform is required to ship the first correct backend. Until a
specific pass has a stated proof obligation and tests, its behavior is identity
and source operation order is retained. Every enabled transform declares its
input/output IR schema version, the invariants it preserves, the facts it
consumes (types, effects, aliasing, borrow validity, exception state, or
lifecycle state), and the provenance mapping for every retained, combined, or
generated node. It produces a complete immutable result or a diagnostic; it
cannot mutate the prior pass result in place or leave a partially rewritten
graph.

A transform must preserve observable reads/writes, external I/O, allocation
and failure order, thrown payload/order, construction completion, ownership
transfer, and cleanup continuation. It may reorder operations only when typed
effects prove their memory regions and all failure/exception/lifecycle effects
independent. Calls, checked accesses, allocations, potentially throwing
operations, volatile/external operations, borrows whose owners may be
invalidated, and lifecycle operations are barriers unless the pass supplies a
stronger explicit proof. Removing a redundant operation requires proof that
it has no observable effect and cannot fail; common-subexpression reuse cannot
reuse an owned object or borrow beyond its owner/invalidation region.

After any transform that changes operation order, control flow, a call target,
ownership, or a possible failure, rerun the typed verifier and every affected
downstream analysis. A CFG transform reruns CFG verification and lifetime
analysis; an effect or callee change also recomputes recursive call-SCC effect
summaries and exceptional successors. A profile-lowering transform that adds
an allocating, copying, or failing thunk must first materialize it as a typed
operation and repeat CFG/lifetime verification; the renderer is never an
optimization or semantic repair stage. Every pass is deterministic, charged
against the compilation work budget, and leaves a testable before/after
artifact with source provenance.

The backend must preserve source evaluation order explicitly. In
`outer.at(i).at(j).append(value)`, evaluate `outer`, then `i`, perform the
checked outer borrow, then `j`, perform the checked inner borrow, then evaluate
and pass `value` according to the selected method signature. Each intermediate
result is a borrow of the existing object unless the language explicitly
requires a value. The COBOL call graph must not add copies, reorder side effects,
or defer an index check until after the nested call.

##### Pass scheduling, analysis dependencies, and invalidation

The pass sequence is a dependency graph with explicit prerequisites, not a
collection of optional callbacks whose order is inferred from source-file
order. Every pass registration declares its stable pass ID, implementation and
input/output schema versions, consumed artifact digests, required analyses,
produced analyses/artifacts, invalidated analyses, target scope (program,
module, function, specialization, or profile), deterministic work budget,
cancellation safe points, a concurrency class, and any exclusive mutable
service/lane IDs it requires, plus failure-propagation scope for each declared
failure cause (`task`, `dependency_closure`, or `invocation`). A pass may run
only when all prerequisites and verifiers for its exact input versions have
succeeded. A skipped prerequisite is not treated as an empty successful
result; it is either an explicit
not-applicable state with a recorded reason or a pipeline error.

Represent registrations as a versioned compiler-internal pass manifest and
validate the complete manifest before scheduling any work. Each descriptor
must contain:

| Descriptor field | Required contract |
| --- | --- |
| `pass_id`, implementation version, descriptor schema version | Stable canonical ID plus explicit versions; duplicate IDs, implicit version zero, or runtime-dependent IDs are rejected. Implementation version changes whenever behavior changes without a more specific rule/schema version covering it. |
| Scope key and scope kind | Canonical program/module/function/specialization/profile identity defining one pass instance. A profile-neutral pass cannot include host or target-profile identity in its scope key. |
| Input/output artifact declarations | Artifact ID, schema version, multiplicity, and producer/consumer role. Every required input has exactly one resolved producer or an explicit immutable root-input declaration; every output has one owner and declared visibility boundary. No undeclared file or global side effect may be an output. |
| Analysis prerequisites, outputs, and invalidations | Exact analysis IDs and versions consumed, established, and invalidated. Registration validation checks producer edges and topological consistency; a pass cannot consume an analysis it invalidates in the same invocation without an explicit ordered subpass. Fixed-point analyses such as recursive effect SCC summaries are one named pass with a declared convergence rule and budget, not an accidental cycle in the pass DAG. |
| Input digest policy and cache policy | Exact canonical digest set and key domain, deterministic/host-independent classification, cacheability, verifier IDs, and required resource-summary schema. A cacheable pass may not consume undeclared environment, time, process-global state, or live worktree bytes. Nondeterministic or host-observational passes are explicitly non-cacheable and cannot produce semantic artifacts. |
| Concurrency and service lanes | One of `parallel_safe`, `invocation_serial`, or `process_serial`, plus exclusive service/lane IDs and synchronization contracts. The registry validator rejects incompatible overlapping lane declarations and never upgrades an unclassified pass beyond process serialization. |
| Budget and cancellation contract | Deterministic work-unit schema, per-scope quota rule, cancellation safe-point IDs and maximum logical work between checks. Wall-clock timeout belongs only to an external-tool adapter and is not substituted for a semantic work budget. |
| Failure mapping | Exhaustive typed mapping from each pass failure category to task, dependency-closure, or invocation scope, plus whether a diagnostic is source-mappable. Unknown categories fail closed; scheduler policy never parses message text to infer scope. |

Before execution, reject duplicate pass/artifact/analysis IDs, missing producers,
multiple owners, schema-incompatible edges, undeclared inputs/outputs,
unmapped failure categories, invalid lane requirements, and dependency cycles
outside a declared fixed-point pass. Emit one deterministic registry diagnostic
that names the descriptor and broken edge; do not start a subset of the graph.
Partition registered outputs into artifact-producing outputs and attempt-scoped
validation evidence. A pass may not mix both output classes; split it into
separate pass nodes so validation-only changes cannot perturb generated-source
identity. Compute a canonical `artifact_pass_registry_digest` over exactly the
artifact-producing pass descriptors and their dependency closure; include that
digest in the top-level artifact `generation_key`. Compute and record the full
registry digest, including validation-only parser/correlation/tool passes, in
build/reproducibility evidence and the validation-plan/attempt identity, not in
the generated-artifact key. Worker count and runtime scheduling telemetry are
excluded from both semantic artifact identity and validation-plan identity.
Pass-cache keys use the descriptor digest for that pass and its transitive
artifact inputs, not the entire registry digest, so an unrelated registration
edit does not flush reusable pass results. The artifact generation records the
exact artifact-producing registry/toolchain identity; attempt evidence records
the full registry and exact validation-tool identities. A registry edit
invalidates changed passes and their downstream consumers without forcing
unrelated analyses to execute again.

Execution is transactional per published artifact. The pass reads immutable
inputs and writes to private scratch output; its output is verified and hashed
before the scheduler makes it visible to dependent passes or the cache. A
diagnostic, cancellation, resource-limit failure, or verifier failure discards
that output and blocks dependents. The last known-good artifact remains intact.
Parallel passes may share only immutable inputs and explicitly synchronized
interning services. Their outputs and diagnostics are merged at deterministic
phase barriers using canonical artifact/diagnostic keys, never completion
order. A pass cannot observe a peer's unpublished partial result.

##### Concrete backend artifact DAG

Register the following minimum producer/consumer chain explicitly; these are
separate immutable artifacts or validation records, not phases whose ordering
is inferred from the renderer's call stack:

| Artifact / pass boundary | Required inputs | Produced output and forbidden dependency |
| --- | --- | --- |
| Concrete semantic/CFG/lifecycle facts | Finalized source semantics, canonical specialization/effect graph, verified typed IR, concrete CFG and lifetime/cleanup proofs. | Verified target-neutral operations, outcomes, and edge states. No COBOL spelling, module IR, renderer output, or target-dependent source behavior. |
| Profile-lowered plan and complete request closure | Verified semantic/CFG/lifecycle facts, selected immutable target profile, pinned physical `abi_version`, link/heap/exception/runtime contracts. | Physical layout/operation projection, control-region and activation manifests with logical IDs, and the complete profile-lowered name-request set. No allocated COBOL spelling is required to derive logical fields or requests. |
| Frozen name table | Complete request closure, canonical name-key/allocator schemas, profile namespace policy, and ABI-visible identity/slot facts. | Independently verified name table. Name allocation cannot consume module-IR node IDs. |
| Composed ABI descriptor/fingerprint | Logical ABI descriptor, physical profile projection, and frozen name table. | Final physical descriptor/fingerprint including any allocated external spelling; consumers cannot fill missing provider fields or reinterpret versions. |
| Semantic expectation manifest | Verified typed IR/CFG/lifecycle facts, physical ABI descriptor, profile contract, and frozen name table. | Canonical expected semantic/output facts with stable expectation IDs and source origins. Must be sealed before module-IR construction and cannot consume module IR, renderer output, parsed COBOL, or source-map ranges. |
| Module-IR build and self-verification | Profile-lowered plan, frozen names, ABI descriptor, and control/activation manifests. | Structured module IR plus a separate verifier result. The builder does not consume the expectation manifest; comparison against expectations is a downstream validation edge and cannot mutate the IR. |
| Render | Self-verified module IR, frozen names, target profile, and renderer version. | Exact COBOL bytes and source-map artifact; renderer does not consume parser grammar, expectations, or correlation results. |
| Independent output parse | Exact rendered bytes, target profile, and parser grammar/tokenizer version. | Observed normalized graph; parser does not consume module-IR objects, emitter state, expectations, or correlation results. |
| Expectation comparison and correlation | Frozen semantic expectation manifest, verified module IR, independently parsed observed graph, exact rendered bytes/source map, and versioned join policy. | Validation-correlation report with observed joins/ranges and pass/fail results. It cannot feed data back into expectations, module IR, names, or rendered output. |
| COBOL compile/link/run evidence | Exact rendered bytes and declared dependencies, a passing structural/correlation result where required, target profile, separate tool-compatibility/invocation policy, fixtures, and validation plan. | Attempt-scoped evidence bound to exact artifact and evidence-input digests. Tool outcomes do not rewrite semantic or generated artifacts. |

The DAG validator must reject undeclared edges and cycles, including indirect
cycles through cache keys or manifests. In particular, do not make the
expectation manifest depend on module IR merely because the final comparison
needs both, and do not make the renderer depend on expectations merely because
validation runs immediately afterward. Changing only a downstream
validation/correlation node may invalidate evidence but must not mutate or
invalidate an upstream artifact whose complete declared inputs are unchanged.

Invalidation follows semantic dependencies, not filename timestamps or a
manually maintained "probably unchanged" flag. At minimum, the scheduler
implements and tests these rules:

| Changed input or result | Must be invalidated and recomputed | May remain reusable when its own key and verifier inputs still match |
| --- | --- | --- |
| Source body, declarations, imported public interface, language semantic rule, or standard-library interface | Finalized semantic unit for the affected module; dependent specializations; direct/transitive effects; CFG; ownership/lifetime; logical and physical ABI projections; module IR; names/descriptors; rendering and evidence for affected dependents. | Unrelated modules whose imported interface/body hashes and dependency edges are unchanged. |
| Generic body, canonical substitution, selected overload/operator/lifecycle target, or specialization closure | Affected specialization and callers that depend on its identity/effects; SCC effect summaries; all affected CFGs and lifetime proofs; ABI/module outputs that contain those calls. | Semantic parsing and specialization records with distinct, unchanged canonical keys and no dependency path to the changed record. |
| Operation order, effect summary, call target, exception set, or may-fail outcome | Typed-IR verification; affected call-SCC summaries; CFG construction/verification; ownership and cleanup analysis; logical exception ABI and all downstream profile/module artifacts. | Canonical type/layout records that do not depend on changed behavior. |
| CFG edge, merge input, scope exit, construction-completion fact, borrow relation, or cleanup operation | CFG verification; reachability/dominance as applicable; lifetime/borrow fixed points; cleanup plan; logical ABI if exceptional continuations/payload flow change; profile lowering and all emitted artifacts. | Source semantic unit, concrete specialization identities, and direct effects when those facts are proven unchanged. |
| Logical call contract, result/context/payload rule, or logical ABI schema | Logical ABI lowering; all physical projections/descriptors; module IR, frozen names when ABI-visible requests change, fingerprints, manifests, rendering, and compile/link/run evidence. Re-run CFG/lifetime only if the changed contract adds/removes/reorders a semantic operation or changes ownership/failure behavior. | Parsed/typed source IR and CFG/lifetime proofs only when the logical contract change is representation-only and the verifier proves semantic equivalence. |
| External dependency adapter mode, status map, call-availability policy, output-validity guarantee, ownership/effect contract, callback signature/lifetime/failure translation, or exception boundary descriptor | Revalidate the imported external declaration and adapter; update the call operation's effect/outcome and exception set; recompute dependent call-SCC summaries, forward and reverse callback call-graph edges, activation/re-entry and borrow/lifetime proofs, CFG exceptional edges, result-publication and cleanup proofs when failure/ownership behavior changes; rebuild relevant `.cbi` source-interface/ABI/link-effect hashes, module IR, adapter/callback COBOL, link manifest, and compile/link/run evidence. Re-run names/layout only when physical entry spelling or linkage slots change. Reject consumers built against incompatible descriptor hashes/schema rather than silently substituting a new contract. | Unrelated module analyses, specializations, and call sites with no dependency on the changed descriptor; target-neutral CFG/lifetime facts only where the complete outcome/effect/ownership contract is unchanged and verifier inputs still match. A private provider implementation-effect change that preserves the declared external contract updates only implementation/link-effect dependents and evidence, not the consumer's public call semantics. |
| Target profile/version, capability manifest, physical `abi_version`, picture/layout policy, or physical call convention | Profile and physical ABI lowering; ABI descriptor/fingerprint; module IR/name feasibility; rendering, validation, and tool evidence for that profile. If lowering introduces a copy/allocation/failure thunk, invalidate and rerun CFG/lifetime from the typed operation insertion point. | Source/semantic IR, specialization identity, and target-neutral CFG/lifetime results when their inputs and semantics are unchanged. |
| Profile mapping or declared bounds for source-runtime services, such as `program_argv_limits`, environment lookup, clock, and random-service capabilities | A changed declared profile bound/capability bumps the profile-contract version/digest and invalidates affected profile-bound host-effect operations, typed outcomes/CFG/lifetime results when failure or ownership behavior changes, profile lowering, expectation manifests, generated artifacts, and conformance evidence. Every mapping must still implement the canonical source-language encoding, error, and observable-behavior contract; a mapping that changes those semantics is rejected as non-conforming and cannot be legitimized by a profile-version bump. Recheck profile compatibility and separate-compilation closure before linking. Preserve the profile-neutral source-interface hash when public declarations are unchanged, and preserve the ABI fingerprint unless the physical call/layout/entry projection changes. Tool-only `tool_argv_limits`, compiler environment, and process supervision policy remain invocation/evidence inputs and do not invalidate generated-program semantics or artifact keys. | Unrelated source interfaces, canonical types/specializations, and target-neutral analyses whose complete inputs and effects are unchanged; evidence for other host-service APIs not affected by the changed profile contract. |
| Source numeric type/operation policy, exact range/precision, conversion rule, rounding rule, or arithmetic failure contract | Numeric type checking and typed-IR/constant-evaluator verification; affected operation effects/CFG edges when may-fail behavior changes; dependent specialization/lifecycle results when signatures or behavior depend on the changed type/rule; affected profile mappings, source-interface/semantic hashes, manifests, rendered artifacts, and evidence. Recompute physical ABI descriptors only if numeric slot identity, layout, or boundary marshalling changes. | Unrelated source identities and target-independent analyses when canonical numeric IDs and their complete policy digests are unchanged. |
| Physical numeric ABI projection: `PIC`/`USAGE`/sign/scale/representation, boundary conversion/marshalling, or physical numeric ABI version | Physical ABI projection and fingerprint; module/layout manifests; link compatibility; generated declarations/call slots; renderer validation and compile/link/run evidence for that profile. | Source numeric contract, operation-lowering strategy, and semantic specialization identity when behavior and source type IDs remain unchanged. |
| Profile numeric operation mapping: guard/intermediate strategy, helper selection, or emitted `ROUNDED`/`ON SIZE ERROR` policy | Numeric profile-lowering catalog and its versioned artifact; expectation manifest; affected COBOL module IR/rendering; structural validation and compile/link/run evidence. Re-run CFG/lifetime if failure effects change; update the ABI fingerprint only if an exported helper/linkage/layout or ABI-boundary rule changes. | Source semantic/type contract, canonical type IDs, and physical ABI descriptor when the new implementation preserves their semantics and layout. |
| Heap-handle ABI, merged heap-domain descriptor, concrete pool request/layout set, or profile resource ceiling | Heap-domain merge/service generation; physical handle projections and descriptors; affected module/link artifacts, manifests, generated COBOL, and compile/link/run evidence. Re-run CFG/lifetime only if the changed allocation/failure/ownership semantics or operation graph changes. A changed profile limit must update profile-bound `max_size`/resource contracts and their runtime tests. | Canonical source types, semantic specialization identities, and target-neutral lifecycle/CFG facts when type/layout semantics and failure outcomes remain the same. |
| Heap-pool request provenance, deduplication/aggregation rule, or effective executable-wide per-type capacity | Request normalization/merge verification; merged heap-domain descriptor and digest; shared heap service; affected physical handle/module/link artifacts, manifests, generated COBOL, and compile/link/run evidence. Re-run CFG/lifetime only when allocation limits alter source-visible failure behavior or operation edges. | Canonical type/layout identity, specialization IDs, and target-neutral lifecycle/CFG facts when their semantic contracts and failure outcomes are unchanged. |
| Runtime handle encoding, domain-instance validation, generation width, or slot-retirement policy | Physical handle projection and heap-service validation/release operations; ABI descriptor/fingerprint when the representation or exported boundary changes; profile-specific heap artifacts and stale/foreign-handle evidence. Re-run CFG/lifetime only if access/release error effects or ownership behavior change. | Logical handle identity and canonical type/specialization IDs when the representation change preserves null, ownership, access, and failure semantics. |
| Heap block/slot state schema or reservation/publication/adoption transition | Heap-service state machine, lifecycle operation lowering and verifier, logical/physical operation descriptors, affected CFG/lifetime evidence when failure or cleanup edges change, generated module/link artifacts, and state-transition tests. | Source signatures and type/specialization identities when only the internal state representation changes and transition/failure behavior is proven equivalent. |
| Name-key schema/byte encoding, allocator algorithm/hash domains, name policy, reserved-word table, ABI-visible name request, or profile identifier/length limits | Name-key validation/serialization; name allocation and collision validation; ABI fingerprint when ABI-visible names participate; module IR references; rendered text, source-map ranges, expectation manifests, and target compiler evidence. Existing name tables with a different key/algorithm version are incompatible and must be regenerated; dependent provider interfaces/link manifests are rebuilt when exported spellings change. | Semantic resolution, specialization, CFG, and lifetime analyses; target-neutral IDs remain unchanged when only name-key serialization or private spelling policy changes. |
| CFG-to-COBOL control-region plan, structuring strategy, or dispatcher-state schema | Control-region/terminator lowering and its versioned plan; private dispatch-state/name requests; module IR and its verifier; expectation manifest; output/source maps; structural and compiler-profile evidence. Re-run CFG/lifetime only if the selected transformation changes semantic edges, operation order/effects, or cleanup behavior. Recompute the ABI fingerprint only if an ABI-visible slot/name/layout actually changes. | Verified typed CFG, lifetime facts, source contract, and physical ABI descriptor when the new plan has a verified edge-preserving projection and only private generated representation changes. |
| CFG/control-region budget policy, work-unit schema, or target nesting/dispatcher bound | Revalidate cached deterministic charge summaries against changed limits; rerun only passes whose work-unit schema/inputs changed or whose summary is missing, invalid, or over the new limit. Recompute region planning and downstream module IR/expectation/rendering when target nesting or dispatcher feasibility changes. A resource-policy-only change does not alter semantic CFG edges or ABI unless a lowering choice changes their observable behavior. | Typed source operations and verified CFG/lifetime artifacts when their complete inputs and validated work summaries still satisfy the new policy; renderer-independent ABI descriptors when no physical slot/layout changes. |
| Module/global initializer, transitive initialization effect summary, init dependency edge/order, or bootstrap ABI | Recompute affected initializer effect closure and link initialization graph/order; regenerate the provider `.cbi` link-effect record/hash, bootstrap/module-init entries, link manifest/digest, affected module IR/output, and compile/link/run evidence. Re-run CFG/lifetime for an initializer body or cleanup change; a source-order-only dependency change that leaves operations unchanged need not alter unrelated CFGs. | Unrelated module semantic/CFG/lifetime artifacts and source interfaces whose imports, global effects, and public ABI are unchanged; preserve `source_interface_hash` and physical `abi_fingerprint` when only private initializer implementation/effect metadata changes and no exported contract or ABI-visible bootstrap layout changes. |
| COBOL module-IR schema, allowed-node set, or canonical serializer | Rebuild and reverify affected module-IR artifacts under the new schema; update module-IR/cache artifact digests and rerun downstream rendering/validation when the serialized module-IR subject changes. Preserve generated COBOL bytes and generation identity only when normalized IR and renderer inputs are proven identical; never accept stale serialized IR by heuristic decoding. The independent expectation manifest remains derived from typed-IR/CFG/lifecycle/ABI/profile/name inputs, not from module IR. Recompute the ABI fingerprint only if the composed descriptor or ABI-visible names actually change. | Typed source IR, specialization, CFG/lifetime proofs, and the final ABI descriptor when lowering semantics and descriptor fields are unchanged and the new representation passes equivalence validation. |
| Output-parser grammar/tokenizer, validation-correlation report schema, or expectation-to-output join/cardinality policy | Reparse the exact existing rendered bytes and regenerate the correlation report and validation evidence using the new parser/join versions; update evidence-plan/input digests and rerun every affected structural, compiler, or release gate. This is validation-only: preserve typed IR, CFG/lifetime, ABI/name artifacts, verified module IR, rendered COBOL/source map, `generation_key`, and artifact-manifest digest when their own inputs are unchanged. A newly detected mismatch fails validation; it does not rewrite output or expected semantics. | Generated artifacts and their keys remain reusable only when the exact source/profile/expectation inputs are unchanged; old parser/correlation evidence is never upgraded or relabeled as evidence for the new versions. |
| Semantic expectation schema or expectation-derivation rule | Re-derive and validate the semantic expectation manifest from verified typed IR, CFG/lifecycle, ABI/profile descriptors, and frozen names; update its artifact/evidence identity and rerun the independently parsed COBOL comparison and affected executable gates. A serialization-only schema change may preserve expected semantic values; a changed expected rule is a conformance-contract change and must be adjudicated against the language/ABI specification, never learned from current module IR or output. Rebuild generated COBOL only if the adopted semantic/lowering contract or its actual lowering inputs change. | Target-neutral source/type/specialization and ABI facts, plus generated artifacts when the change is proven to be representational serialization only and their complete generation inputs remain identical. |
| `.cbi` interface schema, canonical codec, hash-domain definition, or reader compatibility rule | Provider interface serialization/integrity digests; consumer interface ingestion and compatibility checks; affected interface/build cache envelopes; dependent artifact manifests and validation evidence. Re-run semantic resolution only if the validated normalized export graph or compatibility semantics changed; never accept an old schema through heuristic decoding. | Canonical module/symbol IDs, source semantic hashes, specializations, CFG/lifetime, and ABI fingerprint when a codec-only migration preserves the complete normalized interface and all descriptor fields byte/field-equivalently. |
| COBOL renderer version or formatting rule | Rendered text/hash, generated line/range source-map data, output manifests that bind those hashes, and compile/link/run evidence. Reparse and structural equivalence validation must run on the new text. | All semantic, specialization, CFG, lifetime, logical/physical ABI, and frozen-name artifacts whose schemas and input digests are unchanged. |

Activation policy has an explicit invalidation edge even when no source file
changes. A changed activation strategy/profile capability, per-function frame
slot/record layout, `max_activation_record_bytes`, or
`max_live_activation_storage_bytes` invalidates physical activation layout,
arena provisioning, root bootstrap, module IR, output, and profile evidence. A
changed configured activation limit, exhaustion type/catchability,
`max_exception_frames_per_activation`, or exception-CFG frame-bound algorithm
invalidates the call-site activation-check operation, frame-depth/byte
summaries, checked resource product, transitive effects, recursive call-SCC
summaries, affected invoke/cleanup CFGs, logical call/exception-effect
descriptors, linked root-control layout, expectation manifests, runtime
traces, and compile/link/run evidence. Recompute the affected chain in pass
order and reject stale imported effect/ABI records; preserve canonical
source/type/specialization identities only when their complete inputs and
behavior remain unchanged.

These are minimum invalidation edges; implementations may invalidate more,
but never less. The verifier records the exact input-digest set used for each
analysis result. An analysis may be retained across a downstream-only change
only if its pass/schema versions and complete verifier-input digest still
match. A cache hit is the same decision as an in-memory reuse: verify its
envelope, dependency digests, schema, profile scope, and result invariants
before reuse. Unknown rule-version changes, absent dependency records, and
partially validated outputs are cache misses, not best-effort hits. Clean and
incremental executions must produce the same normalized pass graph, semantic
diagnostics, final artifacts, and observed behavior for the same declared
inputs.

##### Logical-condition evaluation and CFG lowering

The current language standard records `&&` and `||` precedence but explicitly
does not yet promise short-circuit behavior. Phase 0 must close this source
semantic gap before the backend treats right-hand-side suppression as
observable behavior. The recommended 1.0 contract is left-to-right
short-circuit evaluation: evaluate the left operand once and convert it to the
condition type; for `&&`, evaluate the right operand only when the left is
true; for `||`, evaluate the right operand only when the left is false. The
result is a boolean value. This recommendation must be adopted in the language
standard and tests, or replaced by another equally explicit, target-neutral
contract; it cannot be left to a COBOL profile or inferred from a compiler's
`AND`/`OR` implementation.

Under the recommended contract, represent each logical operator as control
flow, not as a single eager binary COBOL condition: the left result branches
either to the right-expression region or directly to the corresponding
constant-result block, and a typed merge supplies the final boolean. Nested
operators preserve source precedence and associativity. Side effects,
allocations, checked accesses, and throws in a skipped operand do not occur;
when evaluated, they retain their ordinary effects and exceptional edges. The
right operand cannot be hoisted, duplicated, or evaluated while forming a
COBOL condition string. If the language-standard decision is still open, a
backend must not claim either behavior; expressions whose accepted meaning
depends on the unresolved rule remain gated until the decision is recorded.

##### Activation-local storage, recursion, and re-entry

The logical call model gives every dynamic invocation a distinct activation,
even when recursion reaches the same function/specialization repeatedly. All
mutable function locals, receiver/argument temporaries, construction flags,
catch parameters, cleanup state, return-completion state, and the activation's
exception-frame link belong to that invocation. They must not alias another
recursive or re-entrant invocation. Immutable constants and explicitly
declared module/process state are separate storage classes and never serve as
an accidental substitute for locals.

Every target profile must declare and prove one physical activation strategy:

1. compiler-native per-invocation storage and recursive-call support, with the
   exact generated COBOL declarations/call attributes and observed compiler
   behavior; or
2. compiler-managed activation records represented in generated COBOL, with a
   canonical record layout, explicit initialization, push/pop transitions,
   representable capacity, and a deterministic exhaustion path.

The semantic contract is identical across strategies. `WORKING-STORAGE` may
hold only immutable constants or explicitly shared process/module records; it
must not contain mutable per-call locals, cleanup flags, handler variables, or
exception-frame slots. Never infer persistence, recursive activation
isolation, `LOCAL-STORAGE` availability, or re-entry behavior from the COBOL
compiler brand or host OS. If neither strategy is independently proven for a
profile, recursive/re-entrant functions are rejected for that profile before
COBOL rendering.

The activation model has a deterministic maximum live activation count and a
frozen source-visible outcome when it is exceeded. Phase 0 adopts the typed,
catchable `stack_overflow` exception for acquisition failure at an ordinary
call site. Its fixed payload is exactly two profile-independent `size_type`
values, `limit` and `requested_depth`; profile validation requires the
configured limit to be at least one (the executable entry activation counts),
no greater than the profile capacity, and strictly below the maximum
representable `size_type`, so `limit + 1` is representable. Construct and
publish this payload inline,
without allocation, user code, or a potentially throwing constructor. The
check occurs before publishing a new activation or entering the callee.
Native and compiler-managed strategies obey the same declared bound and
outcome; this limit is a program/profile input recorded in the manifest, not a
host-stack guess. `stack_overflow` is catchable at the failed call site when
the current exception frame is `EMPTY`, and from an active handler only by
using a pre-reserved child frame; it never overwrites the caught parent.
For each concrete function, CFG/lifetime analysis computes the maximum number
of simultaneously live exception frames in one activation: one primary frame
plus the maximum nested child-frame depth required by active handlers. Store
those slots in that function's activation-local storage/activation record;
they are independent of activation-count capacity and never allocate on a
throw path. The profile declares `max_exception_frames_per_activation` and
`max_activation_record_bytes`; reject a function exceeding either bound
before COBOL rendering. Compute record size from the canonical profile-projected
layout of locals, staged arguments/results, flags, and exception frames,
including declared `PIC`/`USAGE`, `OCCURS`, and alignment rules—never from host
`sizeof` or estimated C++ structures. The compiler checks, using overflow-safe
target-independent arithmetic, that `configured_activation_limit *
max_activation_record_bytes` fits `max_live_activation_storage_bytes` and all
profile/address/count representations. A compiler-managed arena reserves that
declared bound before `main`; an allocation failure uses the root bootstrap
failure path and prevents entry. A native per-invocation strategy must prove
its compiler/runtime can sustain the same worst-case bytes at the configured
depth; a count-only recursion probe is insufficient. If no capacity guarantee
is available, the profile cannot claim that activation limit and must reject
the recursive/re-entrant feature or select another proven strategy. A valid
program therefore cannot exhaust a reserved child frame or activation slot
before the declared limit. A missing/corrupted slot at runtime is an internal
ABI invariant violation and takes the profile's terminal fatal path without
replacing the parent exception.

Compute the frame-slot requirement from the completed typed exception CFG, not
from source-token or brace nesting. Start each function activation with one
primary call frame. A forward dataflow tracks the abstract stack of live frame
owners and their states across normal, catch, rethrow, replacement, cleanup,
and exceptional edges: child-frame entry pushes one slot; successful nested
handling clears/pops that child; an escaping replacement releases the old
handler frame only after no-throw cleanup and reparents the completed child;
rethrow preserves the current slot. Calls in a callee use that callee's own
activation and do not add to the caller's per-activation slot count. At every
CFG merge, incoming owner/state stacks must agree or be reconciled by explicit
verified transitions; a loop back-edge must return to its entry stack and may
not accumulate frames. The required slot count is the maximum live stack
height over reachable CFG states. The independent verifier recomputes this
bound from the exception CFG and compares the per-function layout and profile
limit; it does not trust a copied maximum from the layout builder. Resource
arithmetic for maximum activations times maximum activation-record bytes is
checked before emitting target COBOL.

The physical layout pass emits a canonical activation-layout manifest before
rendering. It has one entry per concrete function activation layout and records
each field's stable owner/value ID, target representation, `PIC`/`USAGE`,
element count/`OCCURS` dimensions, alignment or padding, byte/unit extent,
offset (when the profile has addressable offsets), storage class, and the
function-specific exception-frame range. It also records the activation-slot
stride/capacity, control/header fields, and the mapping from each typed field
reference to its emitted COBOL data item or profile-supported address
projection. Every physical field must map to exactly one logical field; no
mutable local, temporary, completion flag, or exception slot may silently map
to shared `WORKING-STORAGE`. Storage reuse is permitted only when a separate
verified liveness proof establishes non-overlap on all normal and exceptional
CFG paths; the initial backend may keep fields distinct to simplify audit.

The target-profile lowering must choose and version a concrete representation
for that manifest. A native-per-invocation strategy maps fields to compiler
storage whose per-call isolation is proven for the exact emitted declarations
and call form. A compiler-managed strategy maps each successful activation to
one reserved slot and maps field accesses through the profile's declared
record/address mechanism; it may not assume arbitrary byte addressing,
pointer arithmetic, or `LOCAL-STORAGE` support. If a profile cannot represent
the required layout, it rejects that strategy before rendering. The generated
COBOL verifier reparses declarations and access operands and compares names,
storage sections, dimensions, representations, slot stride, frame ranges, and
field-to-slot projections against the independently produced manifest. Runtime
traces then prove distinct live recursive activations use distinct slots and
that pop/reuse does not retain a prior invocation's flags or exception state.
Thus layout arithmetic, emitted storage shape, and observed recursive behavior
are three separate checks; passing one cannot stand in for the others.

Represent activation acquisition as an explicit may-fail CFG operation after
the receiver and arguments have been evaluated/staged and before callee entry.
If the check fails, do not enter the callee or publish its result; destroy only
completed caller-owned receiver/argument temporaries in reverse order, preserve
borrowed arguments, and take the frozen typed failure edge. Include this
failure in the call's transitive effect summary and logical ABI `may_throw`
contract unless analysis proves the configured limit cannot be reached on
that call path. The successful edge alone publishes the callee activation;
normal return and exceptional propagation each restore/decrement it exactly
once after callee cleanup. Never hide the check in generated COBOL after the
call or treat its failure as an ordinary return status.

The activation-limit outcome participates in the exception state machine. From
an `EMPTY` call-site frame, the declared `stack_overflow` outcome is propagated
normally. From `HANDLER-ACTIVE`, it uses a child frame so a nested `try` can
catch it without losing the caught parent payload. If activation acquisition
fails while a frame is `RAISING`, `PROPAGATING`, or `CLEANING`, the failure is
a second exception and must enter terminal `FATAL`, preserve the first payload,
and skip remaining cleanup/dispatch. The exact `stack_overflow` fields and
frame-capacity rule above are part of the exception ABI and profile descriptor.
Payload construction is infallible by contract: it writes two fixed scalar
fields into a reserved frame without allocation or user code. Any implementation
that introduces a fallible step, or any runtime frame-capacity/corruption
failure, is a fatal invariant violation, not a second catchable error. The
child frame for failure inside a handler uses storage already reserved to the
current activation/control context; it cannot consume the activation slot
whose exhaustion triggered the error.

Recursive calls, mutually recursive SCCs, cross-module recursion, and
re-entry through any supported callback boundary use the ordinary typed call
ABI and effect/cleanup analysis. The call graph's SCC is a compile-time fact;
it does not license a shared runtime activation. A callback/re-entry boundary
must declare its call and exception-context behavior; undeclared foreign
re-entry is rejected rather than allowed to overwrite generated state.
The root activation counter, activation arena, and exception-frame chain are
mutable runtime control state. Phase 0's threading decision governs their
ownership: for single-threaded 1.0 they may be run-unit-local; if threads are
included, each concurrently executing task requires an isolated control
context, and shared heap/reference-count state needs the separately specified
synchronization contract. A process-global counter/context is never accepted
as a substitute for task-local state in a threaded profile.

#### CFG and cleanup lowering

The CFG is the authority for cleanup planning. For each lexical scope, track
the ordered set of objects whose construction has completed. On a normal scope
exit, `return`, loop transfer, exception propagation, or constructor failure,
derive the cleanup list from the objects live on that exact edge and destroy in
reverse completion order. Do not maintain independent hand-built cleanup lists
for each statement kind once the CFG is authoritative.

##### Normalized block and edge contract

Every CFG block has a stable ID, one owning function/specialization ID, an
ordered list of non-terminator operations, and exactly one explicit terminator.
There is no implicit fallthrough. Terminators include unconditional branch,
conditional branch, typed catch dispatch, invoke, return, throw/rethrow,
terminate, and unreachable. An invoke names exactly one may-fail operation,
normally a call, and owns that operation's normal and failure successors;
allocations, checked accesses, and construction/copy/move operations that can
fail use the same invoke-shaped split. No later pass may add a successor to a
non-failing operation or leave a may-fail operation without all declared
outcomes. The invoke is the final operation in its block. Source `break` and
`continue` become branches to their resolved loop targets annotated with the
scopes exited; they do not get a special emitter-only cleanup path. A call that
may fail is an `invoke` with distinct normal and exceptional successors; a
non-throwing call is an ordinary call operation and cannot acquire an invented
exception edge later.

Each edge has a stable ID, source and target block IDs, edge kind, source
origin, exited-scope sequence, and any outcome payload (such as the active
exception frame/payload identity or an operation result). The successor list on the source
terminator and predecessor list on the target must be reciprocal and complete.
An invoke result is available only on its normal edge; the exceptional edge
carries the failure/exception state and has no usable result. Constructor,
copy, and move operations likewise publish destination liveness only on their
successful outcome. A branch merge uses explicit typed block parameters or
merge operations with exactly one incoming value per predecessor edge; each
incoming value must match the parameter type/category and preserve its
ownership/borrow mode. No later pass may infer an incoming value from paragraph
order, a COBOL working-storage item, or a matching name.

Cleanup is represented by ordinary blocks and operations joined by typed
cleanup edges. The edge records the reason for exit, while each cleanup
operation records the specific object/subobject and active exception state. A
cleanup block may be shared only when its ordered live-object set, completion
state, active exception frame/payload identity, parent-frame stack, and
continuation are identical. Post-CFG
verification rejects unterminated blocks, missing or extra successors,
orphaned edges, invalid merge inputs, result uses on failure paths, and
unreachable cleanup that is still referenced by a live exit. It also confirms
that every reachable exit reaches exactly one return, propagation, or
termination continuation.

Represent cleanup as explicit CFG blocks/operations. A cleanup block records
its incoming edge class (normal, return, break, continue, throw, or failed
construction), active exception frame/payload and parent-frame stack, and
objects destroyed. For every
potentially throwing destructor or cleanup operation, enforce the language
rule that destruction is non-throwing; if a destructor violates that rule,
reject its definition or terminate according to the specified fatal policy.
If cleanup occurs during propagation and another exception is raised, route
directly to the designated double-exception termination block; never replace
the active payload. Constructor failure cleans only completed bases, fields,
and nested elements, not the object whose construction did not complete.

The lowering should deduplicate equivalent cleanup suffixes only after proving
that the active-exception state and object-completion set are identical. Any
cleanup sharing must remain visible in the manifest and source map. Initial
implementation may emit more paragraphs than optimal; correctness and
inspectability precede paragraph-count optimization.

#### Control-flow and lifetime dataflow algorithm

Use explicit basic blocks for entry, expression evaluation, branch tests,
loop headers/back-edges, scope exits, call success/failure, constructor
success/failure, catch dispatch, return, and termination. Preserve source
evaluation order by sequencing side-effecting subexpressions in the CFG before
their consuming operation. A may-throw operation has separate normal and
exceptional successors; do not encode its failure path as an emitter-side
conditional inferred from a helper name.

Run a forward worklist analysis over the CFG. The input state for each block
contains, at minimum:

- the active lexical-scope stack and declaration order;
- per-object construction/lifetime state;
- the ordered completed-subobject/element prefix for any partially constructed
  aggregate or container operation;
- owner/borrow relationships and finite invalidation facts keyed by borrow
  definition/owner where checked references or iterators are enabled (not an
  unbounded integer generation); and
- active exception frame/parent-frame stack, payload identity, and the cleanup
  boundary to which propagation is headed.

The conceptual object-state lattice is `not-entered`, `uninitialized`,
`constructing`, `live`, `moved-from`, `destroyed`, and `path-dependent`. A
constructor operation changes `uninitialized` to `constructing`; only its
success edge changes the complete object to `live`. Its failure edge cleans
only completed bases, members, or elements. A successful move changes the
source to the type's specified moved-from state and makes the destination live;
if move can fail, both continuations use the type's documented failure state.
Assignment transfer functions come from the selected operation's declared
strong/basic guarantee rather than assuming every assignment is atomic.

Implement this as a finite powerset domain over the concrete states plus
`unreachable`: an unreachable block has the empty set; each reachable block
joins predecessor facts by set union; a singleton set is the exact state and a
set with multiple members is displayed as path-dependent while retaining its
members for later transfer and diagnostics. Thus `definitely-live` means the
state set is exactly `{live}`, while `maybe-live` means it contains `live`.
Transfer functions map each incoming concrete state to its documented
outcomes; they may not collapse distinct states to a more convenient state.
Every block and edge transfer must be monotone under subset ordering on state
sets (`A subset B` implies `F(A) subset F(B)`). Transfers are defined as the
image of each incoming concrete state under its declared outcomes, and edge
predicates may only filter/refine facts from their proven condition; neither
may invent facts based on iteration order or remove a fact that remains
possible. The worklist processes reachable blocks and their outgoing edges in
canonical ID order, recomputes a block when any predecessor fact changes, and
starts from empty/unreachable facts, so iteration is ascending. Since the
domains and static object, subobject, borrow, and exception identities are
finite and the transfers are monotone, this reaches the least fixed point. Do
not use widening, state dropping, or order-sensitive overwrite at joins.
Exceeding the deterministic transfer/work budget is a resource-limit failure,
not permission to weaken ownership facts or accept partial analysis. Predicate
refinement is allowed only for facts proven by the branch condition and is
recorded on that edge.
If any concrete incoming state violates an operation precondition, preserve
that state as a verifier diagnostic keyed by operation, predecessor edge, and
state; do not filter it out and continue as though the invalid path were
unreachable. Diagnostics are deduplicated by that stable key across worklist
revisits, and any such error prevents publication or caching of a successful
analysis result.
For dynamically sized containers, the finite static domain tracks the
container object plus its typed live-count storage/value and construction
success-prefix state; generated runtime lifecycle loops use the successfully
constructed live count, never capacity, to destroy elements. It does not
invent a static object ID for every possible runtime index.

At a join, merge facts conservatively: definitely-live is the intersection of
predecessor live sets, while maybe-live is their union. A use requiring a live
object must be proven definitely-live or guarded by an explicit source-visible
condition; the backend must not turn uncertainty into an uninitialized read.
When an object is live on only some incoming paths but its language lifetime
extends across the join, either split the cleanup paths or emit a completion
flag initialized false and set only after successful construction. Cleanup
tests that flag and clears it exactly once. `path-dependent` is not permission
to byte-copy, skip cleanup, or call a destructor unconditionally.

Iterate loop dataflow to a fixed point over back-edges. A local whose lifetime
is inside a loop body is destroyed on every path leaving that iteration,
including `continue` and exceptional exits, before the next iteration can
construct the same logical local again. `break` exits through the same lexical
cleanup as other scope exits. For a return of an owning value, first construct
or move the result into the typed abstract return destination; then destroy
locals in reverse completion order; then transfer control. Result construction
and any failure edge are part of the semantic CFG and lifetime proof. Physical
ABI lowering later maps that destination to the caller-provided result slot
without changing the order or adding a second construction. If result
construction fails, take the exceptional edge and clean only locals whose
construction completed.

For try/catch, model the exception edge leaving the throwing operation, the
cleanup of scopes exited before the handler, typed catch selection, payload
binding/copy, and handler entry as distinct CFG operations. Rethrow preserves
the active payload and skips already-completed cleanup. A cleanup action may
not clear or replace the active exception. Since destructors are required to
be non-throwing, a destructor exception is a compiler/runtime invariant
violation and follows the specified termination path; it is not another
ordinary catchable edge.

The worklist must converge deterministically. Give blocks and operations stable
IDs derived from source-unit identity and structural order, not allocation
addresses. Emit analysis diagnostics for inconsistent object state, use after
move/destruction, borrow escape/invalidation, unsupported throwing cleanup,
and a control-flow path whose completion state cannot be represented by the
selected ABI/profile. The analysis result must include predecessor/successor
edges, incoming/outgoing lifetime states, and derived cleanup operations so
the test oracle and COBOL expectation manifest can inspect the exact proof.

Borrow invalidation must use a finite abstract state, not a monotonically
incremented integer epoch that can grow without bound in loops. Associate each
borrow value with its owner and creation operation; owner-invalidating
operations kill the affected live borrow facts, and joins produce
valid/invalid/path-dependent facts conservatively. Re-executing a borrow
creation site may establish validity for the newly defined result only; it
must never revive an older borrow that crossed an invalidating edge. Loop
back-edges merge these facts to a deterministic fixed point, and the chosen
abstraction must distinguish a fresh post-mutation borrow from a stale
pre-mutation borrow without relying on runtime counter wraparound. If a target
profile cannot represent the required dynamic check, reject the operation or
feature at profile lowering rather than omitting the check.

##### CFG-to-COBOL structuring and terminator lowering

Do not emit CFG blocks by concatenating source statements or relying on COBOL
paragraph fall-through. After CFG/lifetime verification and target-profile
selection, construct a versioned control-region plan. It records each source
block/edge, the chosen lowering strategy, generated region/paragraph IDs,
synthetic state fields if any, and the proof that all terminators and cleanup
edges are preserved. The COBOL module-IR builder consumes this plan; it does
not rediscover branches from source text or guess structure from paragraph
names.

The versioned plan is a proof-carrying lowering artifact, not advisory
metadata. Its canonical record contains at least:

| Record | Required contents |
| --- | --- |
| Plan header | Plan schema/version; source CFG schema and digest; lifetime/cleanup proof digest; selected target-profile ID/version and relevant capability digest; region-discovery algorithm version; canonical resource-charge summary; and deterministic plan digest. |
| Region node | Stable region ID; kind (`sequence`, `conditional`, `natural_loop`, `invoke`, `cleanup`, `exit`, `dispatcher`, or explicit non-structurable region); canonical parent/child IDs; ordered member block IDs; entry and continuation/exit IDs; selected lowering strategy and a reason/capability code. |
| Edge disposition | Every CFG edge ID exactly once, with source/target IDs, original edge/outcome kind, owning region ID, and disposition (`sequence`, branch arm, loop test/back-edge, invoke outcome, cleanup transition, terminal exit, or possible divergence). It also records any explicitly modeled lowering-protocol edge so the semantic projection can remove only those nodes. |
| Region proof | Dominator/postdominator witnesses or canonical fact references; loop header/latches/member set/exits; branch merge and arm ownership; proof that cleanup and failure edges are not bypassed; and the reason a candidate structure was rejected. The independent verifier recomputes the facts from the CFG rather than trusting copied sets. |
| Physical dispatch mapping | For dispatcher regions, canonical block-ID to dense ordinal mapping, ordinal width/range, done/exit states, and selected dispatch construct. For structured regions, the chosen profile construct and exact condition-evaluation placement/count. |
| Synthetic node | Stable synthetic ID, source/ABI origin, semantic operation represented, and the inverse projection needed to compare the lowered graph with the source CFG. No synthetic cleanup/result/exception operation may be unlabeled. |

Before module-IR construction, an independent plan verifier checks schema and
input digests, recomputes dominator/postdominator and loop facts, validates
parent/child and member-set consistency, and proves a bijection between
reachable CFG blocks and their declared executable lowerings (with each
proven-unreachable block explicitly omitted and retained in the CFG artifact).
It verifies that every source edge has exactly one disposition, every
dispatcher ordinal is unique/in-range, all synthetic protocol nodes have an
inverse semantic projection, and the selected strategy is supported by the
exact profile and within resource limits. Projection of the plan back to
semantic edges must equal the verified CFG, including exception identity,
result availability, cleanup order, and possible divergence. A missing,
duplicate, stale, or contradictory certificate fails before module IR; the
builder may not repair it or fall back to emitter heuristics. The canonical
plan and its verified work summary are cache inputs, and any CFG, lifetime,
profile-capability, algorithm-schema, or resource-policy change invalidates
the affected plan and module-IR descendants.

The preferred strategy structures reducible CFG regions using their explicit
dominators, postdominators, loop headers/back-edges, merge blocks, and
exceptional successors. Lower nested conditional regions to `IF`/`ELSE` or
profile-approved `EVALUATE`, and natural loops to a profile-approved
`PERFORM` form with a proven pre-test/post-test condition. Multi-exit loops
must route `break`, `continue`, return, and exceptional exits through their
already materialized cleanup blocks before leaving/restarting the loop. A
loop's source condition is evaluated exactly as often as the CFG requires; do
not duplicate a side-effecting predicate in both a COBOL loop clause and its
body. If a profile construct changes evaluation count or operand order, it is
not a valid structured lowering for that region.

Region discovery is a deterministic pass with an inspectable certificate:

1. Start from the verified reachable CFG and use stable block/edge IDs for all
   traversal and tie-breaking. Preserve unreachable blocks and their source
   diagnostics in the CFG artifact; omit them from executable module IR only
   when the prior reachability pass proves they have no observable effect.
2. Compute dominator and postdominator facts to a fixed point using canonical
   predecessor/successor order and the invocation work budget. For the
   reachable graph `V`, set `Dom(entry) = {entry}` and initialize every other
   `Dom(n) = V`; repeatedly apply `Dom(n) = {n} union intersection(Dom(p))`
   over reachable predecessors until stable. Preserve each return,
   exception-propagation, and termination edge as a distinct outcome, but add
   an analysis-only virtual exit `Omega` reached by each such terminal edge.
   Let `F` contain nodes with a finite path to `Omega`; set `PostDom(Omega) =
   {Omega}` and initialize each other `PostDom(n) = F union {Omega}`. Iterate
   `PostDom(n) = {n} union intersection(PostDom(s))` over only successors that
   can reach `Omega`. An edge to a successor outside `F` is retained and
   classified as a possible divergence edge; it is not mistaken for a
   terminating path or discarded from the region certificate. Nodes outside
   `F` have no finite-exit postdominator facts and cannot be selected as a
   merge by treating `Omega` as a real block. `Omega` is never rendered as
   COBOL. `F` means may-reach-a-finite-exit, not must-terminate: postdominance
   is over finite-exit paths, while back-edges and the possibility of infinite
   execution remain explicit and must be preserved by the chosen loop/dispatch
   strategy.
3. Identify each natural loop from a back-edge whose target dominates its
   source. Compute its member set by reverse predecessor closure, group nested
   loops by strict set containment, and record its header, latches, exits,
   condition/test placement, and associated `break`/`continue` targets.
   Overlapping non-nested loop sets or a cyclic strongly connected component
   with more than one external entry are irreducible for this structurer.
4. For each conditional/multiway region, choose its nearest valid common
   postdominator as the merge/continuation over finite-exit paths. Every arm's
   terminating path must pass through that merge without entering another arm
   or bypassing required cleanup. Record edges into non-finite-exit successors
   as possible divergence and preserve them inside a nested loop/closed SCC;
   they do not count as a terminating bypass, but cannot be erased or routed to
   the merge. Exception and cleanup edges are included in this proof, not
   ignored as "unlikely" control flow. If no valid single merge exists or a
   divergence edge cannot be represented without changing behavior, the
   region is not structured by this strategy.
5. Build a region tree only when loop sets are nested, branch regions are
   properly nested/disjoint, every reachable block belongs to exactly one
   leaf region, and every CFG edge is accounted for as a sequence edge,
   branch arm, loop edge, invoke outcome, cleanup transition, or exit. Record
   the dominator/postdominator evidence, edge classification, and reason for
   any region marked non-structurable.
6. Select the structured emitter only if the tree can represent all operations
   and effects in the chosen profile without changing evaluation count,
   evaluation order, failure routing, or cleanup. Otherwise select the bounded
   dispatcher only if its own profile capabilities and resource limits pass;
   otherwise fail before rendering with the smallest source-mapped
   non-structurable region and its entry edges.

The analysis does not "repair" an irreducible graph by changing semantic CFG
edges. If an upstream normalization splits a critical edge or adds a merge,
that is a typed CFG transform with stable synthetic IDs and provenance; it
must rerun CFG verification and any affected lifetime/cleanup proof before
region discovery. The region certificate is hashed/versioned with the control-
region plan, so cache reuse cannot skip a changed reducibility decision.

For a CFG that cannot be represented by the structured subset, the backend may
use a bounded block-dispatch state machine, but only as an explicit
profile-lowering strategy. It uses a private program-counter field with a
verified representable range, an explicit done/exit state, and a
profile-supported dispatch construct (for example `PERFORM UNTIL` around
`EVALUATE`). Each dispatch arm corresponds to exactly one CFG block; every
branch arm writes the target block state before the next dispatch iteration;
there is no arm fall-through into another block. Stable semantic block IDs
map to deterministic dense physical ordinals sorted by canonical ID, and the
profile manifest records the ordinal width/range and dispatch syntax. The
state-machine path cannot lower `invoke` as an opaque statement: after the
call, it tests the declared result/exception protocol and selects only the
normal or failure successor. No `GO TO`, `PERFORM THRU`, vendor extension, or
implicit COBOL fall-through is introduced unless that exact mechanism is an
explicitly named and tested profile capability. If neither structured nor
state-machine lowering is proven for a region, issue a stable
source-mapped `unsupported_control_flow` diagnostic before rendering.

###### Deterministic CFG/control-region resource accounting

The pass descriptor publishes a versioned CFG-lowering work-unit schema and
charges at least these independent dimensions:

| Dimension | Stable charge unit and boundary |
| --- | --- |
| `cfg_blocks` / `cfg_edges` | One unit per canonical block/edge created, including explicit invoke outcomes and cleanup edges; count is checked before appending beyond the configured limit. Unreachable source blocks are charged even when later proven removable from executable module IR. |
| `cfg_dataflow_transfers` | One unit per canonical `(block, incoming-edge, abstract-fact-kind)` transfer; reprocessing after a predecessor fact changes is charged again. Fixed-point order is canonical block ID then edge ID. |
| `cfg_dominator_transfers` / `cfg_postdominator_transfers` | One unit per predecessor/successor edge examined during each fixed-point transfer, with analysis-only virtual exits counted as declared synthetic nodes. A changed worklist algorithm requires a work-unit-schema version bump. |
| `cfg_loop_region_work` | One unit per predecessor edge inspected during SCC/natural-loop discovery and loop-member closure, and per candidate loop-set containment comparison. |
| `cfg_region_merge_checks` | One unit per canonical `(branch, outgoing-edge, candidate-postdominator)` test, including cleanup and exceptional-edge legality checks; cached pair results may be reused only with their verified charged-work summary. |
| `cfg_region_nodes` / `cfg_dispatch_states` | One unit per control-region node or selected dispatcher block state. Dispatcher ordinals are checked against the profile-declared representable range before module-IR construction. |
| `cfg_region_depth` | Maximum nested structured-region depth, charged when entering a child region and checked against both the compiler policy and target-profile syntax limit. Exceeding a profile nesting limit may select the dispatcher only if the structured limitation is recorded and the dispatcher is supported; exceeding a compiler budget is a resource failure. |
| `cfg_lowered_edges` / `cfg_module_ir_nodes` | One unit per source-edge mapping and emitted structured/dispatcher node, including synthetic protocol nodes with a source or ABI origin. |

Every charge is associated with the owning pass scope and canonical CFG or
region ID. Increment/check occurs before unbounded append, traversal, or
allocation; limit-plus-one fails with `resource_limit_exceeded`, dimension,
limit, observed count, pass ID, and the smallest stable source/edge origin
available. A limit failure discards the incomplete CFG/control-region artifact
and cannot be reclassified as `unsupported_control_flow`. Conversely, a graph
that fits all compiler budgets but has no semantics-preserving lowering in
the selected profile fails as `unsupported_control_flow`, not as a resource
failure. Prefer structured lowering whenever its proof succeeds and profile
nesting/syntax limits permit it; use the dispatcher only when structured
representation is invalid or exceeds a declared target-profile capability.
Do not choose strategies based on host speed, observed memory, worker count,
or an unversioned cost heuristic. If a structured plan is valid but its
analysis exceeds a compiler work budget, report that deterministic budget
failure rather than silently bypassing the budget by retrying another
strategy.

| CFG terminator/operation | Required COBOL-lowering behavior |
| --- | --- |
| Unconditional branch | In a structured region, continue only into its proven successor; in a dispatcher, assign that successor's physical block ordinal. Branches exiting scopes execute the CFG cleanup blocks first. |
| Conditional branch | Evaluate the typed predicate once, with explicit conversions; choose exactly one edge. Keep both source edge IDs and branch outcome facts in module IR/manifest. |
| Multiway/catch dispatch | Use a profile-approved `EVALUATE` or equivalent decision tree whose cases preserve source ordering and exact type-selection rules. No catch payload is copied until a matching handler is selected; unmatched exceptions follow the propagation edge. |
| Loop/back-edge | Enter the header/test in CFG order; execute the body and latch exactly as represented. A `continue` targets the resolved latch or header specified by the CFG, after its edge cleanup; `break` targets the loop exit after its edge cleanup. Nested transfers use their resolved loop IDs, not the nearest emitter scope by name. |
| Invoke/call | Evaluate and stage receiver/arguments once before the call. Call the exact target, then test the declared exception/result completion state. The normal edge alone may read results or mark constructed values live; the failure edge carries the original active exception and its cleanup chain. |
| Return | Materialize/complete the value result first, perform the CFG cleanup suffix, then transfer to the caller using the selected ABI. A `void` return has no result write. Entry-point process termination and subprogram return are distinct module-IR operations. |
| Throw/rethrow | Initialize/retain the typed payload and active context exactly once, run the planned cleanup, then enter the catch/propagation successor. Rethrow preserves payload identity. A second exception during active propagation takes the designated fatal termination path. |
| Terminate/unreachable | Emit only the profile-declared fatal termination mechanism; it must not return as success. A reachable `unreachable` node is an internal compiler/ABI invariant failure, never an implicit continuation. |

##### Entry-point completion versus subprogram return

Freeze the source contract for normal entry-point completion in Phase 0; do not
derive it from a COBOL verb or the host process API. Recommended 1.0 contract:
`void main()` completes successfully on explicit `return;` or fallthrough;
`int main()` completes successfully for zero and unsuccessfully for nonzero,
with fallthrough equivalent to returning zero. Reject an entry point with any
other return type or a value-return from `void main` before lowering. For
portability, the cross-profile language guarantee is the normalized
success/failure category, not an exact OS exit-code bit pattern; retain the raw
code separately as tool evidence. A profile may promise an exact numeric
mapping only if it documents and passes that mapping on every required host.
Record the source-level rule in `docs/cblc_language_standard.md`; reconcile
`docs/abi_spec.md` to state that process status is not an ABI result slot; and
encode normal entry completion separately from fatal termination in the
backend/profile contract before enabling this behavior in the release profile.

Represent this as an explicit entry-completion module-IR operation, distinct
from an ordinary function `return` and from fatal termination. Returning from a
called local/imported function transfers control to that exact caller once and
publishes its result slot only on success; it must never terminate the run
unit. Completing the entry point terminates the run unit using the profile's
native COBOL mapping and must not return to a COBOL caller. `STOP RUN` (or any
equivalent whole-run-unit verb) is forbidden in an ordinary subprogram return
path. The entry status is not an ABI result slot, and lowering may not convert
one semantic outcome into the other to simplify paragraph control flow.

Fatal termination is an explicit target-profile capability, not an assumed
meaning of a COBOL verb. The profile manifest declares the allowlisted COBOL
termination construct, its scope (must end the entire run unit/process, not
merely `GOBACK` from a called subprogram), and its observable result mapping:
either a canonical nonzero fatal status or a declared abnormal-termination
class that the required process adapter can distinguish from normal return.
The module-IR terminate node carries a stable reason such as
`double_exception` or `compiler_invariant_failure`; the renderer may not
substitute a normal `STOP RUN`/return whose status can be success. Do not call a
C++ `abort`, OS API, shell command, or hidden runtime to supply generated
program semantics. A profile that cannot prove whole-run-unit non-success
termination with native COBOL output cannot claim the corresponding 1.0
exception/control-flow capability and must reject such programs or be excluded
from that conformance profile. Double-exception termination runs only the
cleanup already required before the second exception; it does not resume
remaining cleanup, dispatch another handler, or return to the caller.

The process adapter records raw exit status/signal separately from the
profile-normalized termination category. This keeps tests portable when a host
represents return codes differently, while still requiring an exact declared
profile result and proving that no caller or later statement executes.

##### Program argument acquisition and COBOL lowering

Treat runtime argument acquisition as an explicit hosted-language operation,
not as a side effect of the test launcher. The recommended source API is
`process::arguments() -> vector<string>`: it excludes the executable name,
preserves each argument boundary including empty arguments, and returns owning
UTF-8 `string` values. Process arguments are immutable; each API result owns
its elements independently. Lowering must represent native argument-count
and argument-value acquisition as typed backend operations and map them via a
declared profile capability to native COBOL syntax/runtime facilities. The
profile states how argument count, zero-length arguments, maximum length,
encoding conversion, and acquisition errors are observed. It must not rely on
the host OS or generate a C/C++ shim to provide source semantics; if the
selected native COBOL profile cannot provide the required behavior, diagnose
the capability as unsupported rather than silently omitting arguments.

Convert each native argument to strict UTF-8 before constructing its owning
`string`. A malformed byte sequence or invalid native UTF-16 sequence raises
`invalid_encoding`; replacement characters and locale-dependent conversion
are forbidden. Build the returned vector transactionally: if allocation or
construction fails at argument N, destroy only the completed prefix in reverse
order, release all staged storage, and propagate the original typed failure;
no partial vector escapes. The expectation manifest records the typed
argument-acquisition operations, selected profile mapping, element lifecycle
edges, and failure cleanup. Generated-COBOL structural validation checks the
count/value calls and concrete `string` construction/destruction dispatch;
execution tests compare the exact source-visible vector. This is distinct
from process-adapter `program_argv`, which supplies test inputs to the
generated executable.

##### Environment, clock, and random host services

The process environment visible to a generated program is a declared runtime
input, distinct from the compiler/tool environment allowlist. The recommended
`process::get_environment(name)` contract returns an empty `optional<string>`
for an absent variable and an owning empty `string` for a present empty value.
Freeze the name grammar and case rule; for portable 1.0 behavior, use ASCII
environment names with a canonical case and strict UTF-8 values. Windows
UTF-16 values and POSIX byte values follow the same strict conversion rule as
program arguments; malformed values raise `invalid_encoding`, never locale-
dependent replacement. Tests provide an explicit environment fixture and do
not inherit developer or CI secrets.

Model wall-clock observation separately from monotonic duration measurement.
The profile declares native COBOL acquisition, resolution/range, UTC/timezone
and failure behavior for wall time, plus nondecreasing behavior and failure
mapping for monotonic time. Locale and host timezone do not silently change
the source-visible value. Random generation uses a specified source-level
algorithm and explicit seed/state representation when deterministic behavior
is promised; do not substitute a vendor intrinsic whose sequence varies by
compiler. Random state advances only after a successful result. Any
non-deterministic entropy API is a separate capability and is unsupported
unless its error/security contract is documented and tested.

Represent environment lookup, wall-clock read, monotonic-clock read, and
random-state transition as typed host-effect operations with explicit profile
mapping and failure edges. Test-mode fault/time/environment/random overrides
attach only at those declared operation boundaries using test-only CBL-C or
COBOL instrumentation; C++ infrastructure hooks cannot provide values to a
generated executable. Environment fixtures, clock schedules, random seeds,
and expected observations belong to the immutable validation evidence input,
not the generated-artifact key. A compile-time pass may not query any of these
live process services; any pass consuming host state is non-cacheable and
cannot publish semantic artifacts. The expectation manifest records each
host-effect node and required runtime dependency so structural validation can
check the selected native COBOL mapping and test instrumentation isolation.

For both strategies, the module-IR control-flow verifier reconstructs the
successor relation from emitted regions/dispatch arms and compares it with the
profile-lowered CFG, including synthetic ABI nodes. After erasing only the
declared dispatch/region protocol, every semantic edge must map one-to-one to
the expected edge; extra or missing paths fail. Check that each reachable
module path reaches a defined exit or intentional back-edge, each `PERFORM`
has a balanced scope/exit, and no paragraph's lexical order supplies a
control-flow edge. State-machine storage is initialized before first use and
cannot overflow its ordinal range; an invalid ordinal routes to an explicit
compiler-invariant termination path rather than executing an arbitrary arm.
The chosen strategy and mapping are recorded in the expectation manifest so
tests can compare structured and dispatcher lowerings for the same CFG when
both are supported by profiles.

#### COBOL target profiles and output layout

Treat a COBOL compiler/dialect/source format as a named target profile, not as
an implicit property of the host OS. A profile records its accepted syntax,
source format, line/name limits, pointer and dynamic-allocation facilities,
numeric representations, call/linkage conventions, and supported runtime
capabilities. Build a profile-lowering table mapping each IR operation to one
of: portable COBOL form, explicitly enabled profile extension, or unsupported
diagnostic. Generated COBOL must not select a different feature based on
Windows versus POSIX unless that difference is a declared target capability
with matching tests.

The renderer receives already allocated COBOL identifiers. A central name
allocator must reserve runtime names, user-visible names, generated temporaries,
paragraph labels, and imported module symbols under the target profile's
case-folding and length rules. Collision resolution must be deterministic and
must not depend on filesystem enumeration, hash-table order, process address,
or build directory. If a name cannot be shortened without collision, issue a
diagnostic rather than truncating it. Wrap long source lines using the selected
profile's continuation rules; do not split identifiers, literals, or clauses
at arbitrary character positions.

##### Canonical COBOL rendering and physical-line algorithm

Treat rendered COBOL as a versioned byte artifact. The target profile pins the
source-byte encoding, BOM policy, physical newline bytes, final-newline policy,
source format, sequence/indicator/text columns when fixed format is selected,
maximum physical line length and its measurement unit, literal/continuation
rules, and directive policy. These are profile inputs, never inferred from
Windows/POSIX, locale, editor settings, or the host text API. A renderer
algorithm/version pins deterministic indentation, token spelling/case,
whitespace, clause ordering, wrapping priorities, and how source-map ranges
cover generated separators. Identical verified module IR, name table, profile,
and renderer version must produce byte-identical COBOL and source maps across
processes, machines, worker schedules, and cache states.

The renderer walks canonical module-IR order and emits grammar-aware tokens;
it cannot recover structure from concatenated strings. For each physical line,
measure the profile-defined columns/bytes from the exact encoded output, then
select the last legal break point at or before the limit according to a
versioned priority order (for example, between statements, clauses, and
operands). A continuation is emitted only where the profile grammar permits
it and is represented by a legal token/literal form. Never split an identifier,
numeric literal, quoted literal, or indivisible clause. If no legal break can
fit a token/construct within the profile limit, fail with the stable
source-mapped `COBOL_LINE_TOO_LONG` diagnostic before publishing any COBOL or
source map; do not truncate, silently switch source format, or use a vendor
extension. Apply encoding only after the logical token sequence is
fixed; encoding failure is explicit and cannot replace characters. Physical
line offsets and newline bytes are counted in the exact emitted encoding.

Every emitted token range is half-open in artifact byte offsets. Mapping a
wrapped node to multiple physical lines preserves the same IR-node/provenance
identity on each fragment; punctuation, indentation, and continuation marks
without a direct source operation receive explicit renderer-generated origins
linked to their owning node. Source-map construction must use the same byte
writer but remain independently checked against a fresh scan of the final
bytes, including newline, line/column, encoding, and maximum-line boundaries.
The validator rejects a source map whose artifact digest, byte length, ranges,
or profile coordinate interpretation does not match the exact rendered file.
An over-budget or failed render may retain bounded diagnostic evidence, but
cannot publish a success-shaped source artifact or map.

##### Canonical symbol identity and deterministic name allocation

Name allocation is a backend pass over a complete set of name requests, not a
string conversion performed while parsing or printing. The repository's
`cblc_identifier_to_cobol()` currently uppercases/transforms spellings and
special-cases operators; callers can use that conversion before collision
analysis, and fixed-size COBOL-name fields can truncate unless every caller
checks the result. Treat it as a legacy spelling adapter during migration, not
as the allocator or as proof of uniqueness. In the target design, parsing and
semantic resolution retain canonical symbol IDs and source spellings; profile
lowering asks for names only after the relevant symbol and type identities are
known.

Every request has a canonical key with these components:

- canonical logical module ID, never an absolute path or discovery index;
- resolved semantic symbol ID and symbol kind (program entry, external call
  target, data item, condition name, paragraph, generated temporary, and so
  on); and
- for generated/specialized symbols, the owning function or paragraph ID,
  stable typed-IR node ID and role, plus the structural specialization/type
  arguments and ABI version when those affect the emitted symbol.

Source spelling is a readable stem, not identity. A local variable's key must
also include its lexical owner and stable declaration identity; an overload
includes its canonical signature; a template instance includes canonical type
IDs rather than pretty-printed type names; and a generated temporary uses the
stable operation ID plus a role-specific ordinal within that operation. Do not
use a process-global temporary counter, pointer value, memory address, source
discovery order, absolute checkout path, or hash-table iteration position as
part of a key. IDs must remain unique within one compilation even when two
declarations have the same spelling in distinct scopes.

Serialize that key before hashing with a versioned, platform-independent format;
"full canonical key" never means an in-memory struct dump or a formatted name.
Version 1 starts with the exact bytes `CBLC-COBOL-NAME-KEY\0`, followed by
the schema version as an unsigned 16-bit big-endian integer. It then contains
zero or one record for each field in ascending numeric tag order. A record is
`tag:u16be || payload_length:u64be || payload_bytes`; duplicate tags, unknown
tags in version 1, truncated lengths, trailing bytes, and noncanonical field
order are invalid. Required tags are nonempty logical module ID, symbol kind,
and semantic symbol ID. Optional tags encode the lexical owner/declaration ID,
overload signature ID, generated function/paragraph and typed-IR operation IDs,
generated role and role-local ordinal, the ordered list of structural
specialization type IDs, and the pinned ABI version where ABI identity affects
the spelling. Enum values and each referenced canonical ID/type encoding are
defined by their owning versioned schemas; encode enum/integer payloads as
unsigned big-endian integers of the schema-declared fixed width. Encode a list
as `count:u32be` followed by each element as `length:u64be || bytes`; an absent
optional field is omitted, never encoded as an empty value or zero ID. Every
length is checked against the compiler's deterministic resource bounds before
allocation or hashing. The readable source stem is not part of this key byte
stream; a semantic identity change changes the key, while a presentation-only
spelling change may update the stem without changing identity.

Version 1 assigns these tags and payloads (integer widths are exact):

| Tag | Field | Payload |
| --- | --- | --- |
| `0x0001` | `logical_module_id` | Required, nonempty canonical module-ID bytes. |
| `0x0002` | `symbol_kind` | Required fixed-width `u16be` enum from the name-symbol-kind schema. |
| `0x0003` | `semantic_symbol_id` | Required, nonempty canonical semantic-symbol-ID bytes. |
| `0x0004` | `lexical_owner_id` | Optional canonical enclosing scope/function ID for source locals. |
| `0x0005` | `declaration_id` | Optional stable source declaration ID. |
| `0x0006` | `signature_id` | Optional canonical signature ID for overload identity. |
| `0x0007` | `generated_owner_id` | Optional canonical function/paragraph ID owning a generated name. |
| `0x0008` | `typed_operation_id` | Optional stable typed-IR operation ID for generated temporaries/nodes. |
| `0x0009` | `generated_role` | Optional fixed-width `u16be` enum from the generated-name-role schema. |
| `0x000a` | `role_ordinal` | Optional `u64be` ordinal local to the operation and role. |
| `0x000b` | `specialization_type_ids` | Optional ordered list of canonical structural type-ID byte strings. |
| `0x000c` | `abi_version` | Optional fixed-width `u32be` pinned ABI version when it affects the external spelling. |

For compiler-generated requests, `semantic_symbol_id` is a synthetic canonical
symbol ID assigned from the owning function/paragraph, stable operation ID,
generated role, and role-local ordinal before name allocation; it is never
derived from the eventual COBOL spelling. The additional optional identity
fields remain explicit so validators can verify the derivation and distinguish
request kinds without reparsing the synthetic ID.

The name-table artifact records `name_key_schema_version`,
`name_allocator_algorithm_version`, the primary/secondary hash-domain
identifiers, the effective stem-normalization policy ID/version, and the
configured initial digest length in addition to the profile and allocated
entries. Consumers reject unknown required versions instead of recomputing
with their local defaults.

Collision domains are supplied by the target profile, not guessed from source
scope. The profile must define whether a name participates in the compilation-
wide external `PROGRAM-ID`/`CALL` namespace, a program's data namespace, a
program's paragraph namespace, or more than one namespace (including any
compiler/runtime-specific shared namespace). The allocator compares names
after the profile's specified canonicalization, normally case-folding. It
reserves compiler/runtime entry names and ABI-frozen external names before
allocating private names. User-declared exported/external names either retain
their exact ABI spelling or fail with a collision diagnostic; they are never
silently renamed. Private implementation names may be profile-specific.

Before allocation, complete the profile-lowered request universe. It includes
source/ABI names plus every generated data item and paragraph required by
verified control-region plans, activation layouts, exception/cleanup lowering,
heap/runtime adapters, and dispatch state. Those planning records use stable
pre-module request IDs and logical field/region identities, never allocated
COBOL spellings. A generated request carries its canonical key, stem inputs,
profile collision domains, and ordered provenance back to typed IR/CFG/ABI or
the compiler-generated operation that requires it. No downstream module-IR
builder, renderer, or validator may invent a name request. If a transformation
creates a name-consuming node after this closure is frozen, discard the
dependent profile plan and name table, rerun request collection/allocation as
one new transaction, and revalidate all consumers; never append an ad hoc name
to a published table.

Allocation proceeds deterministically:

Allocator algorithm version 1 defines exact digest and candidate construction.
Primary digest bytes are
`SHA-256(ASCII("CBLC-COBOL-NAME-v1") || 0x00 || name_key_bytes)`; the secondary
digest uses the same concatenation with `CBLC-COBOL-NAME-COLLISION-v1`.
Render digest nibbles as lowercase hexadecimal from most-significant to least-
significant. The profile declares `name_digest_initial_hex_digits` (a positive
multiple of four from 8 through 64); collisions extend by four hex digits at a
time until the primary digest is exhausted. The fixed kind discriminator is
`K` followed by the symbol-kind enum as four uppercase hexadecimal digits.
With normalized readable stem `S`, the primary candidate is
`S-Kkkkk-Hhhhh...`; when no readable-stem characters fit, omit `S-`, leaving
`Kkkkk-Hhhhh...`. The profile's versioned stem-normalization policy yields only
profile-legal characters and a deterministic possibly-empty stem. To fit a
candidate, remove characters only from the right of `S`; never shorten the
kind or digest fields. A profile whose maximum identifier length cannot hold
`Kkkkk-H` plus the configured initial digest digits rejects private generated
names with `COBOL_NAME_SPACE_EXHAUSTED` before emitting any COBOL.

If two or more distinct private keys produce the same profile-canonicalized
candidate, extend the digest length for every private member of that collision
group by one four-digit chunk and recompute each candidate from its original
normalized stem. Repeat group detection in canonical-key order until all
private candidates are unique against one another and all fixed reservations.
A collision against a fixed/reserved name extends only the private candidate;
an ABI-frozen name is never changed. When a private candidate still collides
after the full primary digest (with another private key or a fixed reservation), use
`S-Kkkkk-H<full-primary>-C<secondary-prefix>` with the independently computed
secondary digest, beginning at `name_digest_initial_hex_digits`. Extend
secondary prefixes by four lowercase hex digits on collision, shortening only
the readable stem under the same rule. Exhaustion
of both digests or the profile's name length is a deterministic error, never a
numeric-order suffix. Collision groups are resolved as sets, not by assigning
the first discovered request a preferred spelling, so input enumeration cannot
change which symbol gets extended.

1. Collect every request for the current generated program unit from the
   closure-complete profile-lowered plan. Coalesce requests with the same canonical key and
   identical identity/ABI/stem fields into one table entry while retaining all
   referencing pre-module name-request IDs and provenance in the request-to-entry
   relation. The requests may originate in typed IR/CFG or compiler-generated
   layout/control records; module-IR node IDs are never inputs. Reject malformed
   requests and duplicate canonical keys whose kind, owner, role, ABI metadata,
   or normalized-stem policy inputs disagree; never let discovery order select
   the winning record. Do not wait for unrelated consumers when allocating
   program-local data and paragraph names. Link-global names follow the ABI
   policy in Section 6.7 and are frozen into the provider interface before
   consumers compile.
2. Normalize each request's readable stem using the profile's legal alphabet,
   case policy, leading-character rule, and reserved-word set. Reserved words
   and invalid leading characters are escaped with a stable kind prefix; do
   not depend on compiler-version-specific implicit truncation or escaping.
3. Sort requests by canonical key bytes, with fixed ABI reservations handled
   first. Preserve a fixed external spelling exactly. Generate private candidate
   bytes only with the exact algorithm-version-1 formula above; do not implement
   local formatting variants.
4. Resolve all private/fixed collisions by full-key comparison and the
   order-independent collision-group extension above. Compare names only after
   the profile's specified canonicalization, including reserved-word
   normalization. If both digests or the profile namespace are exhausted,
   emit the stable collision/namespace-exhaustion diagnostic naming all
   conflicting symbol IDs; never use discovery-order suffixes. Provider ABI
   names are already frozen at this point. A collision in a link-global
   namespace is an error for link planning, not an invitation to rewrite a
   provider or consumer name.
5. Freeze the resulting name table before module construction. All declarations,
   references, calls, paragraph targets, manifests, and source maps query this
   same table by canonical ID; no later pass may independently sanitize or
   reconstruct a name from source text.

##### Canonical name-table artifact and publication

Serialize the complete per-module name table with the versioned canonical JSON
codec in Section 6.7; do not create a second escaping, Unicode, or JSON-order
implementation. Its artifact kind is `CBLC-COBOL-NAME-TABLE`, with an explicit
schema version. The fixed-order header records canonical module ID, target
profile ID/version and capability digest, `name_key_schema_version`,
`name_allocator_algorithm_version`, stem-normalization policy ID/version,
`name_digest_initial_hex_digits`, and both hash-domain IDs. The header and
entry arrays use the `.cbi` codec's UTF-8/no-BOM, no-extraneous-whitespace,
fixed-field-order, canonical-integer, duplicate-field, and array-order rules.
The enclosing artifact manifest supplies the exact byte length and content
digest; do not add a competing name-table integrity hash with different
canonicalization.

Each entry records the canonical key as lowercase hexadecimal of the exact
versioned name-key byte stream, the canonical module/symbol IDs, symbol kind,
all applicable collision-domain IDs in sorted canonical order, original source
spelling when one exists, allocated COBOL spelling, ABI-frozen state and pinned
ABI/signature version, plus sorted referencing pre-module request IDs and their
typed-IR/CFG/ABI or compiler-generated provenance roles. The
key-hex field is the identity used to sort entries by unsigned byte order;
canonical IDs and COBOL spellings are not re-derived from that display field.
Private local names stay in this per-module artifact and do not leak into the
public `.cbi`. The provider `.cbi` publishes only external entry spellings
needed by consumers; the `.cbi` spelling, provider name-table entry, ABI
descriptor, and final link-manifest import must be byte-for-byte equal for the
same canonical exported symbol.

The writer emits exactly one entry for each coalesced canonical name request,
preserves all references to that entry, canonicalizes and serializes the
artifact, then reads it back through the independent name-table verifier
before making it visible to module IR, source maps, expectation manifests, or
cache publication. The verifier decodes each key-hex field, checks canonical
key re-encoding and schema support, profile/name grammar and lengths,
reserved words, folded-name uniqueness in every collision domain, fixed-ABI
preservation, request completeness, and exactly-one allocation for every name
request derived from verified typed IR/CFG and closure-complete profile planning.
It independently reruns allocator v1
from the recorded complete request set and compares every spelling, including
the deterministic collision groups; it also compares that request set against
the independently derived complete set from verified profile-lowered IR, never
against a list copied from the name-table artifact. Matching a digest or
trusting the allocator's own output is insufficient. After module IR is built,
its verifier separately proves that every declaration, operand, call, and
branch-target name reference resolves through exactly one frozen table entry;
it cannot add or rename entries. A failed check publishes no partial name table
or dependent module IR. Cache hits must replay
byte-identical table bytes to cold builds; profile or name-policy changes
invalidate affected allocations without changing target-neutral source identity.

Keep the COBOL module IR separate from the text renderer. It should model
program identity, divisions, sections, declarations, linkage, paragraphs,
statements, calls, branch targets, literals, source spans, and profile
capabilities. The module-IR self-verifier checks this structure against its
typed CFG/lifecycle facts, ABI descriptors, and profile; a separate downstream
equivalence validator also consumes the independent expectation manifest.
Neither validator depends on incidental whitespace. A reparse of rendered
COBOL remains an independent check that serialization follows the profile
grammar.

Define this IR as a versioned, immutable node graph. It is a target artifact
model, not a second source-language IR: every node is already profile-lowered
and references the canonical ABI/name tables, while retaining source-operation
and CFG provenance. Minimum node families and fields are:

| Node | Required representation |
| --- | --- |
| Module/program unit | Canonical module ID; selected profile ID/version; generated program-symbol ID; frozen `PROGRAM-ID` name-table reference; ABI descriptor/fingerprint; ordered section IDs; external dependency IDs; and module-level source/generated provenance. Exactly one program identity exists per emitted program unit. |
| Section/division | Section kind from the profile-allowed enum, parent module ID, ordered child declaration/paragraph IDs, and required-presence/order constraints. The renderer does not infer or insert omitted sections. |
| Data declaration | Stable node ID; name-table reference and collision domain; parent group/module ID; level and role; elementary/group shape; canonical source type when applicable; exact `PIC`, `USAGE`, sign/scale, occurrence bounds, storage/linkage class, initialization/completion-state policy, and ABI field ID; typed initializer or no initializer; provenance. `REDEFINES`, `RENAMES`, or implicit fillers appear only when explicitly modeled and profile-enabled. |
| Paragraph/entry | Stable paragraph ID; name-table reference; paragraph kind (local block, lifecycle routine, exported entry, runtime adapter, termination path); owner function/specialization; ordered statement-node IDs; explicit entry/exit and source CFG block/edge mappings; ABI signature ID when callable; provenance. No implicit paragraph fall-through is permitted. |
| Statement | Closed tagged variant, stable node ID, source operation/CFG edge ID, explicit operands/results, effects already accounted for upstream, ordered child nodes, and provenance. Variants cover only emitted profile constructs (for example `MOVE`, guarded arithmetic, `CALL`, `IF`, `EVALUATE`, `PERFORM`, file operations, `DISPLAY`, and explicit return/termination). There is no raw COBOL statement-text escape hatch. |
| Expression/operand | Either a typed data/name-table reference, exact numeric/alphanumeric literal with explicit encoding/continuation policy, or a parenthesized tree of profile-approved operators/intrinsics with explicit result representation. A reference resolves to one declaration; a literal records source type and canonical value separately from its rendered spelling. No operand may be recovered by reparsing a CBL-C expression string. |
| Call node | Resolved provider module/program and external-symbol IDs; frozen entry-name reference; canonical logical signature and physical ABI projection IDs; ordered receiver/argument/result/context operands; pass mode for each slot; and originating typed call ID. A string spelling alone is never a call target. |
| Control-flow node | Explicit branch predicate and target paragraph/block IDs, loop/repetition form, merge/exit semantics, and source CFG edge IDs. Profile-required syntactic nesting may group nodes, but cannot change targets or add an implicit edge. Unconditional transfers are explicit variants and must be in the profile allowlist. |
| Source-map origin | Primary and related origin IDs/roles plus the generated node ID. Byte/line/column output ranges are attached only by rendering and are not part of semantic node identity. Many-to-one and one-to-many mappings are explicit; generated-only protocol nodes use a compiler-generated origin linked to the triggering IR/ABI node. |

The module-IR verifier runs before rendering and enforces the following
invariants:

1. **Identity and references:** node IDs are unique in their declared domain;
   every parent, section child, declaration reference, paragraph target,
   call/provider, ABI slot, and name-table key resolves exactly once. Required
   references cannot be null or represented by a numeric array offset whose
   meaning depends on construction order.
2. **Profile legality:** every statement, expression operator, literal
   encoding, data clause, source-format feature, and external runtime entry is
   present in the selected profile capability manifest. Unsupported nodes fail
   before text rendering with source provenance; the renderer cannot lower or
   approximate them.
3. **Layout agreement:** data-level nesting, elementary/group distinction,
   `PIC`/`USAGE`, sign/scale, `OCCURS`, linkage order, and hidden result,
   exception, ownership, and completion fields equal the frozen physical ABI
   descriptor. No host `sizeof`, implicit padding, unnamed field, or unmodeled
   overlay is allowed to fill a layout gap.
4. **Call agreement:** each `CALL` has one provider or declared external
   dependency, exact arity and slot order, and a matching profile-projected
   signature. The call graph and dependency set must equal the frozen ABI/link
   descriptors and resolved specialization graph; extra, missing, duplicate,
   or wrong-specialization calls fail.
5. **Control-flow agreement:** reconstruct the graph induced by paragraphs,
   branches, loops, `PERFORM`s, and exits and compare it to the expected
   profile-lowered CFG. All targets exist; each reachable path reaches an
   explicit transfer/return/stop or an intentional back-edge; no success,
   exception, or cleanup edge is dropped, duplicated, or redirected. Projecting
   away ABI-only protocol nodes must recover the semantic CFG successor and
   live-object states.
6. **Effects and lifecycle:** module-IR operations cannot introduce an
   allocation, copy, mutation, throw, or external effect not present in the
   typed operation, verified CFG, and lifetime proof. Constructor completion,
   result completion, active exception, cleanup order, and heap release are
   explicit fields/transfers; generated byte-copy statements cannot substitute
   for a type-directed lifecycle call.
7. **Deterministic serialization:** node order, child order, literal encoding,
   and profile clauses are canonical. Canonical module IR serialization is
   stable across host OS, process address, worker scheduling, map iteration,
   source checkout path, and cold/warm cache. Renderer formatting changes may
   alter bytes and source-map ranges but not normalized structure or node IDs.

Run these checks as explicit verifier stages, in this order, before a module
IR is eligible for rendering:

1. **Input closure and version gate:** bind the candidate IR to the finalized
   module/specialization set, target-profile ID/version/capability digest,
   logical and physical ABI descriptor hashes, frozen name-table artifact,
   control-region plan, and activation-layout manifest. Reject absent, stale,
   mismatched, or unsupported required
   schemas before interpreting node payloads. This stage verifies identities
   and digests; it does not trust those digests as proof that later semantic
   checks pass.
2. **Canonical node inventory:** validate closed node tags and field encodings;
   enforce unique stable IDs, owner domains, required fields, canonical child
   order, and provenance shape; build typed indexes by node ID and canonical
   symbol/type/ABI/name ID. Reject duplicate IDs and noncanonical encodings
   before resolving references. Index construction must not make input order
   observable.
3. **Reference and containment resolution:** resolve every parent/child,
   declaration, name-table, paragraph, operand, call provider, ABI slot,
   source CFG block/edge, and runtime dependency exactly once. Check ownership
   and section containment, reject cycles where the schema requires a tree,
   and validate legal graph cycles (such as CFG back-edges) against their
   explicit edge kinds. Missing, ambiguous, cross-module-private, or
   wrong-specialization references fail at this stage.
4. **Profile and physical-layout validation:** validate every node variant,
   literal/encoding, clause, field layout, linkage order, call convention,
   external entry, and runtime operation against the pinned profile and
   composed ABI descriptor. Check activation and payload bounds using declared
   profile limits, not host sizes. Unsupported syntax or representation fails
   here, before any renderer is called.
5. **Control-flow and lifecycle reconstruction:** independently reconstruct
   paragraph reachability, branches, loops, call/return continuations,
   exception and cleanup paths, object completion/moved-from state, and
   activation-state transitions from the node graph. Compare the reconstructed
   graph and edge states with the independently verified profile-lowered CFG,
   lifetime proof, and activation manifest. Reject implicit fall-through,
   missing/doubled transfers, altered cleanup order, and impossible state
   transitions.
6. **ABI and dependency consistency:** compare calls, slots, signatures,
   exported names, dependency edges, result publication, runtime effects, and
   required declarations against the frozen ABI/link descriptors and profile
   contract. This self-verification stage does not consume the semantic
   expectation manifest; it checks module IR against its declared ABI/profile
   inputs and verified typed CFG/lifecycle facts.
7. **Canonical serialization and seal:** serialize only after stages 1–6
   succeed, then independently deserialize and revalidate the serialized
   bytes. Publish the immutable verified-IR artifact atomically with its
   schema/profile/input digests. A failed stage publishes neither a success
   artifact nor a cache entry; diagnostic ordering is deterministic by
   canonical module ID, phase, node ID, rule ID, and provenance key.

Each stage returns either a complete immutable verified result or structured
diagnostics; it cannot leave mutable indexes, accepted node subsets, or
success-shaped cache entries behind. Implement stage-level tests as well as
end-to-end tests so a deliberately malformed input is rejected by the earliest
stage whose invariant it violates. A digest-valid mutation must still reach
and fail the semantic stage, rather than being accepted because its envelope
was resealed.

##### Module-IR, output-parser, and correlation work budgets

Give serialization, verification, tokenization, parsing, expectation
comparison, and correlation their own versioned deterministic work-unit
schemas and compiler-policy quotas. These limits are distinct from target
profile syntax limits and external-tool wall-clock/resource envelopes. Charge
before allocating or appending, use checked arithmetic for byte/count products,
and never choose a smaller semantic result to fit a budget. Limit-plus-one
returns a stable `resource_limit_exceeded` diagnostic naming the pass,
dimension, configured limit, observed count, schema version, and smallest
stable node/source origin; discard the entire affected artifact/report. Do not
relabel resource exhaustion as malformed COBOL, unsupported profile syntax,
or a semantic mismatch. Wall-clock time and available host RAM are not
deterministic compiler work units.

| Dimension | Stable charge and checked boundary |
| --- | --- |
| `module_ir_nodes` / `module_ir_edges` | One unit per canonical node and explicit containment/control/dependency edge, including generated protocol nodes; charge before insertion into builders or indexes. |
| `module_ir_reference_checks` | One unit per resolved reference and one per declared index/ownership consistency check; duplicate/missing-target diagnostics do not trigger unbounded rescans. |
| `module_ir_nesting_depth` | Maximum active nesting across expression trees, data groups, structured control regions, and statement children; check before recursive descent or stack growth. |
| `module_ir_literal_bytes` / `module_ir_serialized_bytes` | Exact encoded bytes, including canonical framing and escaping; use overflow-checked size accumulation before buffer growth. A byte limit is applied to the serialized artifact before publication. |
| `module_ir_provenance_origins` | One unit per primary or related origin reference retained by a node/expectation; deduplicate only where the canonical schema explicitly permits it. |
| `module_ir_verify_work` | One unit per node/edge/reference visited by each verifier stage, plus each specified layout/CFG/ABI comparison; algorithm changes that alter charging require a work-unit-schema bump. |
| `cobol_source_bytes` / `cobol_physical_lines` | Exact captured source bytes and physical lines accepted by the selected parser invocation; enforce before token buffers or line tables grow, while separately applying target-profile line/source-format rules. |
| `cobol_tokens` / `cobol_literal_bytes` / `cobol_parse_depth` | One unit per token and parser transition, exact decoded literal bytes, and maximum nested grammar depth; check before token/AST/stack insertion. Comments and whitespace count toward source bytes even when they do not create tokens. |
| `cobol_parse_work` | One unit per lexed byte/token transition and per grammar production/branch attempt in a specified deterministic order. Error recovery work is charged too; a recovered or truncated parse is never accepted as evidence. |
| `expectation_records` / `correlation_rows` | One unit per independently derived expectation and emitted join row. Correlation candidate comparisons are separately charged; implementation must use bounded indexes rather than an unbounded all-pairs join, while still rejecting ambiguous matches. |
| `diagnostic_records` / `diagnostic_bytes` | Charge each structured diagnostic and canonical serialized byte before retention. On exhaustion, stop collecting at the deterministic boundary and emit one reserved, bounded summary diagnostic identifying the limit; never claim the resulting partial diagnostic set is complete or accept the artifact. |

The limits and overrides are invocation/compiler policy inputs, not COBOL
profile semantics; record their policy/schema digests in the validation plan
and attempt evidence. A quota change reruns only affected verification or
validation stages when the exact generated inputs remain unchanged. A parser,
verifier, or correlation cache hit is reusable only with the exact source/IR,
schema/work-unit, policy-limit, profile, expectation, and algorithm digests;
cached charge summaries are revalidated before reuse. Cancellation is checked
at the stage's declared safe points in addition to deterministic quotas and
must discard the same incomplete outputs.

Implement and test the module-IR self-verifier independently of the renderer
and expectation builder. The output parser consumes only the exact rendered
bytes and selected profile grammar, reconstructing a normalized module graph
(program/declaration/paragraph/call/control-flow/layout subset); it does not
consume pre-render module IR or expectation data. A separate downstream
equivalence/correlation pass compares self-verified module IR and parsed output
field-by-field with the already-frozen semantic expectation manifest and
produces the observation report. This second parse catches serializer defects;
it is not allowed to regenerate expectations from its own parsed output. When
a profile has syntax that the internal parser cannot model, that syntax needs
an explicit node variant and grammar test before the profile can emit it.

##### Target-profile manifest and compatibility policy

The profile is an explicit compiler input recorded with every generated
module. At minimum, its manifest contains:

- stable profile ID/version and profile-contract digest, with exact
  source-format/dialect options and accepted COBOL semantic capabilities;
- generated-source byte contract: source encoding, BOM and final-newline
  policy, physical newline bytes, fixed/free format, fixed-format column
  boundaries, maximum line length and its byte/column measurement rule, and
  profile-approved continuation/directive forms;
- a separate tool-compatibility-policy digest naming allowed COBOL compiler
  families/versions, executable/package identity-verification policy,
  permitted wrapper/downstream-tool closure, invocation adapter contract,
  per-phase compile/link/run process-result mapping, argument/options
  constraints, warning disposition (`allow`, `reject`, or `promote_to_error`),
  policy for unclassified diagnostics, and an allowlist for tool-affecting
  environment/configuration inputs; selecting an allowed compiler version
  changes attempt evidence, not the profile contract or
  generated source identity. Each compile/link/run attempt binds the resolved
  executable/package closure, including every compiler/linker actually
  invoked and the target COBOL runtime/library closure used to execute the
  artifact. The host adapter runs tools from an immutable verified toolchain
  snapshot or an equivalent identity-pinned facility; hashing a mutable path
  and later launching it is insufficient. If the adapter cannot prevent or
  detect substitution across verification, invocation, and completion, that
  toolchain row is unsupported and its outputs are not accepted. A compiler's
  self-reported version string is compatibility evidence only, never
  executable identity. Wrapper scripts are either forbidden or
  explicitly included in the verified tool closure; undeclared executable
  substitution, ambient option/config injection, or an identity change between
  verification and invocation fails the attempt before its output is
  accepted. Resolved paths and host-specific invocation details belong only to
  attempt evidence, not to the source-language profile or generated-artifact
  key;
- accepted syntax and runtime capabilities, including pointer representation,
  dynamic allocation/release, address-based data, arithmetic pictures/usages,
  file organizations, and literal/character encoding;
- control-flow lowering capabilities, including explicit terminator/dispatch
  forms, supported `IF`/`EVALUATE`/`PERFORM` nesting, dispatcher ordinal width,
  and the maximum representable state count;
- activation storage and re-entry capabilities: native recursive-call and
  per-invocation storage support or the compiler-managed activation-record
  model; the activation-record layout/strategy version; the profile maximum
  live activation capacity; the selected-call limit-check and push/pop
  mappings; the canonical `stack_overflow` type and its fixed
  `{limit, requested_depth}` payload layout; reserved, non-allocating
  `max_exception_frames_per_activation` and the fatal policy for a missing or
  corrupted reserved slot; `max_activation_record_bytes` and
  `max_live_activation_storage_bytes`; and run-unit versus task-local
  ownership under the adopted threading contract. A build's configured
  activation limit is a separate immutable input in `[1, profile_maximum]`
  and strictly below the `size_type` maximum,
  and is bound into affected CFG/effect/ABI artifacts and execution evidence;
- managed-handle encoding and validation, including null representation,
  runtime domain-instance identity, allocation-token bounds, generation width
  and exhaustion/slot-retirement behavior, contiguous-array extent checks, and
  the executable-wide per-type/total pool-capacity policy;
- case-folding, reserved-word list, maximum identifier/paragraph lengths,
  source line/continuation rules, generated-name stem-normalization policy ID/
  version, name-key schema and allocator algorithm versions, primary/secondary
  hash-domain IDs, `name_digest_initial_hex_digits`, and collision-domain map;
- physical `CALL`/linkage convention, external program naming and resolution,
  record/payload layout rules, and ABI version;
- the profile-declared generated COBOL form and behavior for
  `CALL ... ON EXCEPTION`/call-availability failure, including the normalized
  process/runtime outcome observable by the adapter. This is distinct from
  any external program's returned application-status mapping;
- separate native COBOL mappings for normal entry-point completion, an
  uncaught source exception during startup, an uncaught source exception after
  entry-point execution begins, and fatal double-exception/internal-fatal
  whole-run-unit termination. Pin the portable result category and any
  profile-specific raw process status for each case; ordinary uncaught
  exceptions must not be collapsed into fatal double-exception status, and no
  callee-level failure may terminate the run unit unless it reaches the
  declared root uncaught/fatal boundary;
- the private uncaught-report paragraph's permitted native COBOL output
  statement(s), destination/stream, encoding and record/newline behavior,
  allowed literal and field forms, and whether output failure is observable.
  Its read-only input fields and no-user-call/no-managed-allocation effect
  contract are fixed by the language-level reporter ABI, while this profile
  selects only the proven COBOL operation and its observable status behavior.
  Pin the reporter ABI/schema version and profile output-adapter policy version
  explicitly; a version change invalidates the reporter expectations and all
  dependent generated artifacts/evidence;
- a statically provisioned, non-allocating root exception/control record
  available before heap/runtime bootstrap, including the inline bootstrap-error
  representation/capacity and mapping when no dynamic exception payload can be
  allocated; and
- native program-argument count/value acquisition, encoding conversion,
  empty-argument preservation, maximum count/length, and failure mapping;
- program-environment, wall/monotonic-clock, and random-service mappings,
  deterministic inputs, encoding, and error behavior; and
- the required compiler acceptance, linking, execution, and negative-feature
  tests that establish each claimed capability, including recursive local
  isolation and activation-limit behavior at limit-1, limit, and limit+1.

The profile ID/version identifies the semantic target contract only. The tool
compatibility policy has its own ID/version/digest and may change independently;
merely adding or removing an accepted compiler build does not change the
profile ID/version, ABI fingerprint, or generated-source key. Any change to
syntax meaning, layout/calling convention, runtime capability, or lowering
semantics must change the profile contract version and invalidate its dependent
artifacts.

Each operation has exactly one profile disposition: supported by the common
generated-COBOL subset; supported through a named profile capability whose
behavior is proven equivalent; or rejected with a stable, source-mapped
diagnostic before COBOL rendering. Do not equate a compiler accepting syntax
with the language promising its behavior. A profile-specific lowering is
permitted only if the profile is explicit and the same CBL-C contract is
preserved. The host OS is not a profile selector; differences in path quoting
or compiler process invocation belong to test/build tooling, not to generated
program semantics.

The current repository has a concrete profile conflict to resolve in Phase 0:
`docs/cobol_dialect_requirements.md` names an ANSI-85-compatible, no-extension
baseline, while the CBL-C emitter currently generates `USAGE POINTER`, `BASED`,
`SET ADDRESS OF`, `ALLOCATE`, and `FREE` in pointer, dynamic-string, and
lifecycle paths. Treat these as observed emitted constructs, not as proof that
the stated baseline accepts them. Phase 0 must either identify and test a
portable implementation for the required semantics or narrow/rename the
profile claims. It must not silently label the current output ANSI-85 or
silently remove heap-backed `string`, containers, or smart pointers from the
1.0 scope to make the document agree.

For each heap/pointer operation, the profile matrix must answer: which
generated COBOL construct or declared COBOL runtime entry reserves/releases
storage; what size/overflow check runs before allocation; how allocation
failure becomes `bad_alloc`; how the returned address is represented and
validated; which exact release family matches allocation; and whether
reallocation can preserve the old block until staged construction succeeds.
The allocation mechanism may vary by COBOL profile, but it must remain behind
one compiler/runtime ABI operation contract. It may not call a C++ allocator,
POSIX/Windows allocator, or undeclared vendor routine to supply application
semantics. Profiles unable to satisfy a required ownership operation must
reject that feature at lowering with a stable capability diagnostic.

##### Heap and pointer ABI decision and implementation plan

The recommended design is one target-neutral managed-heap contract with a
portable, bounded handle-pool implementation as the common-profile baseline;
an explicitly named native-allocation profile may optimize that implementation
without changing source semantics. This keeps `new`, `delete`, strings, and
containers within scope without claiming that ANSI-85 itself supplies a
general-purpose heap. GnuCOBOL documents `ALLOCATE` and `FREE` as dynamic
allocation/release operations and cautions that using a `BASED` item before its
allocation or after its release has unpredictable results ([GnuCOBOL
Programmer's Guide, `ALLOCATE` and `FREE`](https://gnucobol.sourceforge.io/HTML/gnucobpg.html)).
Those statements are therefore a native-profile facility, not the implicit
meaning of the common profile.

This is the roadmap recommendation, not a claim that the portable pool has
already been demonstrated on the repository's stated ANSI-85 baseline. Phase 0
must make the profile choice from executable evidence. The common profile is
the portability baseline only if the pinned compiler set can compile and run
the typed-pool representation and all required lifecycle/exception probes
using the declared common COBOL subset. If it cannot, either select and name a
less restrictive common dialect profile with a documented compiler matrix, or
record the unsupported semantic capability and defer the 1.0 portability claim;
do not silently fall back to native pointers, C++/host allocation, or a
different ownership model. The roadmap does not authorize dropping owning
`string`, containers, or smart pointers to make a profile appear portable.

Freeze these as separate, reviewable decisions rather than one vague
"portable heap" choice:

| Decision | Required Phase 0 evidence and frozen result |
| --- | --- |
| Common profile syntax | Minimal typed-pool fixtures compile under the exact declared source format and compiler versions. Record every required verb/data clause and reject any extension absent from the common-profile allowlist. |
| Handle representation | Compile and execute null, live, stale, wrong-type, cross-module, and generation-exhaustion probes. Freeze field widths, bounds, identity scope, and the rule that handles are opaque logical values rather than host addresses. |
| Typed storage and indexing | Prove concrete monomorphized COBOL groups can represent/index scalar, class, and nested-class slots without `POINTER`, `BASED`, address overlays, or byte reinterpretation. Confirm array contiguity and typed lifecycle dispatch in generated call graphs. |
| Capacity and fragmentation | Freeze per-type and total slot/extent limits, deterministic allocation behavior, fragmentation policy, and `max_size`. Prove exhaustion maps to `bad_alloc`; limits must be visible in the selected profile, not discovered as compiler/runtime accidents. |
| Native profile boundary | For each optional native profile, test its declared pointer/allocation facility and exact release pairing behind the same logical heap ABI. Its source semantics and lifecycle traces must match the common profile; only physical capacity/performance may differ. |
| Claim and naming | Publish profile ID/version, compiler compatibility policy, capability manifest, restrictions, and evidence. Use "ANSI-85-compatible" only for the exact tested common subset; use a named dialect/compiler profile for extensions. |

Do not choose the common subset by intersecting compiler feature checklists
alone. A capability is supported only after generated output is independently
parsed, compiled, linked, and executed for the relevant behavior. A native
profile's success cannot stand in for common-profile evidence, and a successful
common scalar fixture cannot establish class, array, exception-cleanup, or
nested-container support.

Define one logical runtime interface after generic specialization, when the
concrete object layout and lifecycle dispatch are known:

| Logical operation | Contract |
| --- | --- |
| `heap_allocate(type_id, element_count, byte_extent, alignment)` | May fail with `bad_alloc` or a checked size/range error. On failure it returns no token and mutates no live object. On success it returns a private reservation token and records the matching concrete type, extent/count, alignment, generation, and release policy. It reserves storage only; it does not construct any element or produce a source-usable handle. |
| `heap_construct_place(reservation, element_index, lifecycle_id)` | Private lifecycle operation that validates a reserved slot and produces an uninitialized construction place for exactly the declared concrete type/index. It cannot be used for ordinary reads, writes, or source-level pointer formation. Constructor completion is reported separately and marks only that element live. |
| `heap_publish_object(reservation, constructed_count)` | Converts a completed scalar/array reservation into a source-visible opaque handle only after all required construction succeeds (`constructed_count == element_count`; the zero-length array is the defined empty token). On failure/partial construction it publishes nothing. Publication transfers ownership once and records the exact scalar/array release shape. |
| `heap_adopt_storage(reservation, owner_id, live_count)` | Transfers an allocation to a concrete container owner after its staged state is ready to commit. The storage token remains private to that owner; unused capacity slots remain unconstructed, and only `[0, live_count)` may be resolved as live elements. Reallocation adopts the replacement and swaps the owner token only after every required element has been constructed. |
| `heap_resolve(handle, typed_projection, access_mode)` | Ordinary internal/source borrow operation for published live objects or live elements owned by a container. Validates handle generation, bounds, and requested concrete type, then produces a typed borrow/place. It does not transfer ownership or create a source-level raw address. Reserved slots and unconstructed capacity are inaccessible through this operation. Invalid, stale, mismatched, or out-of-range handles follow the specified checked-access/invalid-pointer error path. |
| `heap_destroy_place(owned_token, element_index, lifecycle_id)` | Validates a live element, marks it `DESTROYING` before invoking its type-directed destructor, and creates a scoped destructor-only place for that object. On successful non-throwing completion it marks the slot `UNCONSTRUCTED`; ordinary resolution is denied during destruction. |
| `heap_release(reservation_or_owned_token, expected_type_id)` | Non-throwing. A reservation may be released only after its successfully constructed prefix has been destroyed; an owned object/range only after type-directed destruction, or an owned container block after its live range is empty. Rejects or terminates on a mismatched/double release according to the memory-safety contract; successful release invalidates the token exactly once. It releases storage only. |

The source allocation contract must distinguish failure classes: `new T`
requests one object; `new T[0]` yields a valid empty-range token that cannot be
dereferenced and can be released as a no-op; a negative runtime element count
is `invalid_argument`; arithmetic overflow or a count above the profile's
documented `max_size` is `length_error`; and a representable request that
cannot obtain storage is `bad_alloc`. Deleting a null handle is a no-op. A
stale, wrong-type, or duplicate release is an internal memory-safety violation
and follows the documented non-throwing fatal policy; it must not replace an
active exception. The source type system/semantic pass must reject compile-time
impossible extents before lowering, while runtime checks remain for dynamic
counts.

Allocation and object construction use separate states. A block starts `FREE`,
becomes `RESERVED` after `heap_allocate`, and exposes only a private reservation
token to the lifecycle dispatcher. Per-element states are `UNCONSTRUCTED`,
`CONSTRUCTING`, `LIVE`, and `DESTROYING`; the reservation also records the
completed prefix (or the exact live bitmap when an operation permits
non-prefix construction). A source-visible object/array handle is published
only after its required constructor set has completed. A failed constructor
destroys only completed subobjects/elements in reverse completion order,
releases the still-private reservation, and propagates the original failure.
Cleanup failure while that exception is active takes the fatal second-exception
path.

The required block/slot transitions are:

| Transition | Required operation and visibility rule |
| --- | --- |
| `FREE -> RESERVED` | `heap_allocate` reserves a generation-bearing block; no source handle exists and no slot is live. |
| `UNCONSTRUCTED -> CONSTRUCTING -> LIVE` | The lifecycle dispatcher obtains one construction place and reports completion only on the successful constructor edge; failed construction leaves that slot non-live after subobject cleanup. |
| `RESERVED -> PUBLISHED_OBJECT/RANGE` | `heap_publish_object` succeeds only when the scalar object or every element in the source array is complete; the handle becomes usable after this commit. |
| `RESERVED -> OWNED_STORAGE` | `heap_adopt_storage` commits a container allocation and its exact live range to one owner; unused capacity remains unconstructed and inaccessible. |
| `LIVE -> DESTROYING -> UNCONSTRUCTED` | `heap_destroy_place` closes ordinary access before the destructor call, exposes only the destructor's scoped place, then records completion before any release/reuse. |
| `RESERVED/OWNED_STORAGE/PUBLISHED_OBJECT -> FREE or RETIRED` | Release is legal only after all successfully constructed objects are destroyed; generation invalidation precedes reuse, and exhausted generations retire the block/slot. |

The runtime and generated lifecycle code must reject every other transition
before changing state. In particular, a failed `heap_publish_object`,
double-construction, destruction of an unconstructed slot, storage adoption by
a second owner, release with live elements, and ordinary access during
`CONSTRUCTING`/`DESTROYING` are invariant violations, not recoverable allocator
statuses. They follow the internal-failure/non-throwing policy and cannot
replace an active source exception. Container operations publish size/live
range only after constructor success and update it only after destructor
completion; the verifier must match the owner token, slot states, and range
metadata on every CFG edge.

Container capacity is a storage ownership state, not a set of default-created
objects: `heap_adopt_storage` records the owning specialization and exact live
range, while `[size, capacity)` remains `UNCONSTRUCTED`. `push_back`, `emplace`,
`insert`, `resize`, and reallocation stage construction separately and advance
container size/live range only after success. For reallocation, the old storage
and old values remain authoritative during staging; commit swaps the owning
storage token and live-range metadata as one logical operation, after which the
old objects are destroyed exactly once and the old block is released. A failed
stage destroys only its completed prefix and releases only the replacement.
Ordinary resolution rejects reserved, constructing, destroying, or
unconstructed slots; constructor/destructor code receives a scoped special
place for its own object, which cannot escape or be used to publish it early.

Represent a managed pointer as an opaque handle plus a typed projection and,
where relevant, an element index; do not expose the arena's COBOL variable,
numeric offset, or native `POINTER` as source identity. Pointer arithmetic,
comparison, null state, and interop must each have explicit language rules.
Operations that require an actual foreign address are a separate named
capability and must be rejected by profiles that cannot provide one. In
particular, `heap_resolve` must not let a handle escape its borrow/lifetime
region or silently turn a value parameter into an alias.

For the common profile, lower handles to compiler-generated typed pools in
COBOL storage: monomorphization provides the finite set of concrete element
layouts, and each pool has profile-declared slot/extent limits, occupancy and
generation metadata, and a deterministic free-list/range allocator. Objects
remain in typed COBOL groups; byte storage is used only for byte-oriented
payloads such as `char[]`, not as an untyped overlay for arbitrary classes.
Array allocation returns a contiguous logical range of same-type slots so
indexing and `delete[]` remain well-defined. Capacity is a resource ceiling,
not a language-visible truncation rule: exhaustion raises `bad_alloc`, and
`max_size` is stable for the selected profile. No operating-system allocator,
host pointer, `BASED`, or vendor extension may leak into this common lowering.

An optional native-allocation profile may implement the same contract using
that compiler's documented dynamic allocation and pointer facilities. Its
adapter owns allocation/release pairing, pointer validation, and failure
translation; generated application code calls the logical heap ABI rather
than emitting `ALLOCATE`/`FREE` ad hoc at each `new` or container operation.
The physical profile may have a different resource ceiling, but allocation
failure remains `bad_alloc`, object lifetime and ownership remain identical,
and profile-specific artifacts cannot be mixed at link time. No native
allocator choice may alter copy/move/destructor dispatch or exception order.

Implement the boundary in this order:

1. In Phase 0, build small allocation probes for the stated common COBOL
   baseline and each intended native profile. Confirm grammar, typed-pool
   indexing, pointer/handle representation, checked size arithmetic, and
   runtime failure observability with the actual pinned compiler versions.
   Record which profile supports the required semantics; do not infer support
   from a successful parse of one example.
2. Implement the logical heap interface and common typed-pool allocator as
   CBL-C library/runtime source compiled through the ordinary compiler path.
   Keep C++ limited to compiler analysis, profile selection, name/layout
   allocation, and ABI marshaling; no allocator algorithm or user-observable
   fallback belongs in C++.
3. Add native-profile adapters only behind the same logical operations and
   profile manifest. Verify exact allocation/release pairing, null/failure
   behavior, alignment/layout, overflow guards, stale-handle detection, and
   cleanup on every exception path before enabling the capability.
4. Integrate allocation with Pass 5 lifecycle state: reserve storage first,
   construct objects one at a time, publish liveness only on successful
   construction, and release only after reverse-order destruction. For
   `vector::reserve`, keep the original allocation/content authoritative until
   every new element has been constructed; on failure destroy the constructed
   new prefix and release only the replacement allocation. The link planner
   then merges compatible pool requests into one versioned per-executable heap
   domain and rejects duplicate type IDs with conflicting layout/lifecycle
   descriptors before module rendering.

##### Link-time heap domain and cross-module handles

Managed storage has one heap domain per final linked program, not one
uncoordinated allocator per source module. A handle can be created in one
module, passed through an imported method, and later destroyed from another;
the consumer must resolve it through the same domain and exact concrete type.
The logical handle is opaque and carries enough validated identity to distinguish
its domain, allocation, and reuse generation. It never encodes a host address,
module-local working-storage name, or profile-specific pool index as a
source-visible value. Handles from another executable, target profile, or
domain are rejected; they are not silently rebound to a similarly shaped
allocation.

At the logical ABI boundary, model a handle as the tuple
`(domain_instance, allocation_token, generation)` plus the statically known
typed projection supplied by the operation. `domain_instance` identifies the
initialized heap service instance, not merely the stable executable ID,
profile ID, or descriptor hash; two simultaneously live heap domains must not
accept one another's handles even when their layouts are identical. The
physical profile may encode or validate this tuple differently, but it must
prove domain-instance validation for every boundary that can receive a handle.
If a profile cannot distinguish a foreign handle and the compiler cannot prove
that the boundary is reachable only through verified same-domain calls, it
must reject managed-handle ingress for that entry rather than treating the
foreign token as local. A profile must not claim cross-executable handle
rejection based only on link-time IDs when runtime tokens can cross that
boundary.

For reusable pool slots, increment the generation before publishing each new
allocation in that slot. A release invalidates the current token before the
slot becomes reusable. Generation arithmetic is checked: on exhaustion, retire
the slot permanently instead of wrapping and allowing an old token to become
valid again. Retired capacity is unavailable to later allocations and can
eventually cause the ordinary `bad_alloc` outcome. The null token is a
reserved, non-dereferenceable value; deleting it is a no-op. Array handles
identify the allocation as a whole, with count/extent checked on every indexed
projection; an element projection cannot be released as an independent scalar
allocation or with a mismatched `delete[]` extent.

Every resolve validates in a fixed order: domain instance, null/empty state,
token range, slot occupancy, generation, canonical concrete type/layout,
allocation extent and projected element bounds, then requested access/borrow
mode. Access failure returns the source contract's checked invalid-pointer or
bounds error without changing allocation state. Release validates the same
identity and allocation shape, runs only after type-directed destruction, and
uses the non-throwing fatal policy for stale, mismatched, or duplicate release
so it cannot replace an active exception. Tests must force generation
exhaustion with a reduced-width test profile; they must not wait for a
production-width counter to wrap.

The link planner collects immutable heap-pool requests from every module and
concrete specialization: canonical type/layout ID, element and array shape,
required alignment, construction/destruction entry IDs, requested profile
capabilities, and resource limits. It verifies that all requests for a
canonical type agree on layout and lifecycle ABI, then builds one profile-bound
heap-domain descriptor. For the typed-pool profile, generate one shared COBOL
heap service containing the concrete typed pools and checked access/release
entrypoints, with generated names allocated from the complete link graph. For
a native-allocation profile, the declared heap adapter owns the domain's
allocation table and maps opaque handles to native storage. In both cases,
module interfaces expose only the stable logical handle and typed operation
contracts; physical pool declarations, native pointer fields, and allocator
state remain private to the linked artifact set.

Pool-request identity and merging are deterministic and distinct from request
provenance. A normalized `pool_key` contains the heap-ABI version, selected
profile identity, canonical concrete type/layout identity, storage/element
shape, alignment, and construction/destruction ABI identities. It excludes
module-local COBOL names, source paths, source offsets, and discovery order.
Each request also has a provenance identity (module/symbol ID, specialization
ID, and source allocation-site ID) so the link manifest can prove which
compiled operations are covered without making those local origins part of the
physical pool identity. Requests with an equal `pool_key` coalesce into one
typed pool; they do not accidentally create duplicate pools or multiply a
type-wide capacity merely because the same specialization was referenced by
more than one module. Equal keys still require field-by-field descriptor
equality; a digest match alone is insufficient.

The merge has explicit aggregation rules. Distinct statically reserved object
slots are summed with checked arithmetic; the largest required single
contiguous extent is the maximum across requests; alignment is the maximum
required alignment; and required capabilities are unioned. Repeated references
to the same canonical allocation site are deduplicated, while distinct sites
remain distinct provenance entries. Dynamic allocation headroom is selected
once by an executable-wide heap-capacity policy in the profile/build manifest,
not independently by each module. The merged descriptor records both that
configured capacity and the statically reserved portion, and rejects a policy
that cannot cover the static reservation, maximum contiguous request, or
profile limits. Runtime requests beyond the configured pool capacity fail
through the normal `bad_alloc` contract. The descriptor never infers a finite
program-wide upper bound from local call counts: recursion, re-entry, and
multiple live module activations are covered by the shared runtime capacity
and exhaustion behavior. If a profile requires a statically proven upper bound
instead, that profile must state the bound rule and reject an unprovable
program before rendering.

Requests are sorted by canonical `pool_key` and provenance ID before merge;
checked aggregation is independent of module discovery, import traversal,
parallel completion, and link input order. The final domain descriptor and its
digest include normalized requests, aggregation-rule version, effective
per-type capacities, total resource ceilings, and profile identity. A
request-set or effective-capacity change invalidates the shared heap service
and every physical module/link artifact that refers to it. Source type IDs and
specialization IDs remain stable when only physical pool sizing changes.

Generated methods and compiler-lowered field/element access use the canonical
owner/type operations for a handle. They do not dereference a handle as if it
were a local COBOL address. Construction and destruction remain type-directed
operations selected in specialization; the heap service reserves/releases
storage and validates the domain/slot state, but does not substitute a generic
byte copy or invent destructor dispatch. Constructor re-entry is safe because
the reserved slot is marked unavailable before user constructor code can
allocate again. A failed constructor returns the slot only after cleanup of its
completed subobjects, without publishing the object as live.

The heap service's storage lifetime is an explicit target-profile capability.
The profile must prove that its state persists across calls for the full
program lifetime, or the logical ABI must pass an explicit heap-domain context
through every allocation/access/release call. Do not assume COBOL subprogram
`WORKING-STORAGE` persistence, initialization, reentrancy, or cancellation
behavior from one compiler. The generated heap service is initialized exactly
once before the first allocation; repeated initialization is either a
specified idempotent operation or a stable runtime error. Program shutdown
must report leaked live handles in test/debug builds and release the domain
only after static and automatic object cleanup has completed.

Linking fails before COBOL rendering if a handle-domain version, target
profile, pool descriptor, type layout, lifecycle entry, or heap service symbol
is missing or incompatible. The link manifest records each pool request,
canonical owning type, service entry, allocation/release edge, domain/version,
and profile resource ceiling. A private class-body change that does not affect
the type/layout/lifecycle interface need not rebuild unrelated semantic
clients, but it must rebuild any heap service or provider artifact whose
generated pool accessors depend on that body. Heap-domain descriptor changes
invalidate physical ABI/module artifacts while preserving target-neutral
source type and specialization identities.

If the common profile cannot meet these probes within documented resource
limits, rename or narrow the profile claim and make that limitation explicit;
do not call a native extension "portable" or remove required owning classes
to hide the gap. Whether the common typed-pool design is feasible for every
required 1.0 allocation shape is a Phase 0 spike with an evidence-based gate,
not an assumed implementation fact.

##### Physical call ABI and argument/result lowering

Keep source-language parameter semantics distinct from COBOL's physical
argument mechanism. The generated COBOL ABI may pass storage `BY REFERENCE`,
but each argument descriptor must still record whether its source semantics
are value, const-borrow, mutable-borrow, pointer/handle, or ownership transfer.
Physical by-reference passing must never silently turn a value parameter into
an alias to caller-owned mutable storage.

An instance method has an implicit receiver operand; it is part of the logical
call contract even though it is not a source-declared parameter. Record the
receiver's canonical structural type, const/mutable access mode, ownership
(`borrowed` for ordinary methods), live-state requirement, and any
base-subobject projection required for dispatch. A constructor receiver is an
uninitialized construction target; a destructor receiver is a live destruction
target. Constructors and destructors do not encode the receiver as a
value-result slot or transfer
storage ownership through it. Static methods have no receiver. Assignment and
other mutating operators use the same receiver model as instance methods.
Physical lowering may represent a local receiver as a module-local data
reference or an external receiver as a linkage slot, but both must map back to
the same logical receiver and preserve aliasing, access, and lifecycle state.

For each emitted entry point, construct one canonical ABI descriptor in two
linked layers. Pass 6 fills its logical contract: program/symbol ID, source
signature ID, `interface_version`, `logical_call_contract_version`, optional
receiver type/projection/access/lifecycle state, ordered explicit parameter
type IDs and source pass modes, result presence/type, the `may_throw`/
exception-set state, logical exception-context/payload types, and ownership
transfers. Pass 6 does not assign the physical `abi_version`.
Pass 7 reads the version pinned in export metadata and requires an exact
matching projection in the selected target-profile ABI policy; it does not
negotiate or substitute a version. It then adds the profile projection:
target-profile ID/version, physical linkage layouts, result/context/payload
representation, pointer/handle encoding, and external-entry name requests.
Pass 8 freezes the ABI names, composes the final descriptor, and computes its
deterministic physical layout fingerprint. The
final descriptor is immutable thereafter;
the manifest and `LINKAGE SECTION`/`PROCEDURE DIVISION USING` must be generated
from it. Never reconstruct a signature from emitted names or trust matching
source spellings as proof of ABI agreement.

The 1.0 native logical slot ordering, identified by
`logical_call_contract_version`, which every supported physical profile must
preserve, is fixed:

1. one receiver slot first if and only if the entry is an instance or
   object-lifecycle entry; it is a borrow for ordinary methods, an uninitialized
   construction target for constructors, and a live destruction target for
   destructors;
2. one slot per explicit logical argument, in source declaration order;
3. one result slot if and only if the function returns a value; and
4. one exception-context slot if and only if the entry may throw (an unknown
   imported throw effect is treated as may-throw, not as non-throwing).

Legacy status/result orderings remain available only in individually named,
versioned compatibility adapters. The adapter calls the canonical entry and
maps its typed result/error at the boundary; native and compatibility slots
must not be combined in one call descriptor. The logical order is a 1.0
language ABI contract; the selected profile maps each slot to its physical
representation without reordering it. Phase 0 must reconcile the currently
stale `docs/abi_spec.md` against this contract and generated COBOL before
claiming ABI v1.

Argument lowering rules:

- Evaluate an instance receiver exactly once before explicit arguments, then
  evaluate explicit arguments once from left to right. Stabilize
  non-addressable expressions in typed temporaries; the descriptor records
  temporary construction and cleanup effects. If evaluating the receiver or
  an earlier argument fails, later arguments are not evaluated and the call
  is not entered. Never create a by-value receiver copy as an ABI convenience.
- A constructor receives destination storage in the uninitialized state and
  marks it live only after successful completion; on failure, caller-owned
  storage remains allocated but is not destroyed as a complete object. A
  destructor receives a live object by borrow, destroys its subobjects, and
  never takes ownership of the enclosing storage. A static method has no
  receiver slot. These distinctions are present in logical IR and manifests
  even when the COBOL profile lowers local calls without linkage fields.
- Pass scalar values through their profile-declared storage layout. A
  by-value class/container argument is copy-constructed or move-constructed
  into one owned argument object, then ownership transfers to the callee's
  parameter lifetime at entry; the callee destroys it on every return/throw
  edge. A borrowed/reference argument aliases the original object and never
  receives a hidden copy. The selected constructor and ownership mode are
  explicit in the typed IR and manifest.
- If constructing/stabilizing any argument fails, do not call the callee;
  destroy only already-completed argument temporaries in reverse order and
  propagate the exact failure. If the callee throws, its transferred
  parameters are cleaned by the callee before control returns with an active
  exception context; the caller cleans only values it still owns.
- For value returns, the result slot is caller-provided storage with an
  explicit completion/ownership state. The callee publishes a complete result
  only on the success path. On exception, the slot is not read or destroyed as
  a complete value. A returned class/container transfers ownership exactly
  once; scalar results use their declared picture/usage and conversion rules.
  Typed IR records the return expression's value category, the exact selected
  copy/move/construct operation, source moved-from transition, result-destination
  identity, and any source-language-authorized elision. The result construction
  is staged before local cleanup; profile lowering may not select a different
  lifecycle operation, introduce an extra semantic copy, or elide a required
  operation. If a physical ABI requires a separate marshalling object, model its
  construction/transfer/destruction as typed operations and reverify the
  failure and lifetime edges.
- For a potentially throwing call, pass the caller's exception-context/frame
  reference last. The callee may publish only into the designated `EMPTY`
  frame; a nested throw from an executing handler uses a distinct child frame.
  A second exception in a frame already raising, propagating, or cleaning an
  active exception follows the termination rule and cannot overwrite its
  payload.

An exception-context/frame descriptor must identify ABI version, frame ID,
parent-frame identity, state (`EMPTY`, `RAISING`, `PROPAGATING`,
`HANDLER-BINDING`, `HANDLER-ACTIVE`, `CLEANING`, or `FATAL`), static and
dynamic type IDs, payload layout/size/ownership, source location, and any
secondary-failure diagnostic fields promised by the selected exception ABI.
Payload representation is type-directed and included
in the module manifest; the current fixed message/integer fields are not a
universal class-payload ABI. Catch binding, rethrow, payload copy/move, and
clear/release operations must consume this descriptor and use the type's
lifecycle dispatch. If a payload shape is unsupported in a profile or ABI
version, reject it during semantic/ABI validation rather than truncating it,
copying arbitrary bytes, or treating it as a message buffer.

##### Exception-context state machine

The context is caller-owned state passed by reference, not a process-global
exception variable. It identifies the current exception frame and any enclosing
handler frames; each frame owns at most one payload. Its state and parent-frame
relationship are explicit in the logical call contract and physical ABI
descriptor; generated COBOL paragraphs implement these transitions without a
native unwinder. At minimum, use these states and legal transitions:

| State | Meaning and allowed next transition |
| --- | --- |
| `EMPTY` | The current frame owns no exception payload (an enclosing frame may still be in `HANDLER-ACTIVE`). Beginning a throw in this frame enters `RAISING`; a fresh nested throw from a handler uses a distinct child frame. |
| `RAISING` | A payload is being constructed/copied/moved into context-owned storage. The payload is not catchable or observable as complete. Successful completion publishes its type, source, layout, ownership, and payload-complete marker atomically, then enters `PROPAGATING`. Any failure while raising enters `FATAL`; it must not expose partial payload or resume ordinary flow. |
| `PROPAGATING` | Exactly one complete payload is owned by the context and is moving outward or being matched against ordered handlers. A match enters `HANDLER-BINDING`; no match preserves the same payload and propagates to the caller. Entering cleanup for this exception enters `CLEANING` and retains the payload identity. |
| `HANDLER-BINDING` | The handler is selected, but handler execution has not begun. A reference catch binds as a borrow; a value catch constructs its own local from the active payload. Successful binding enters `HANDLER-ACTIVE`. If value-parameter construction throws, enter `FATAL` immediately: no enclosing or sibling catch handles that second exception. |
| `HANDLER-ACTIVE` | The handler is executing while its frame retains the caught payload. A new exception in a nested `try` uses a child frame, so it can be caught independently; successful child handling clears that child and resumes the parent handler. If the child escapes the handler, the replacement protocol cleans handler locals and any value catch parameter, releases the old caught payload exactly once, then propagates the child frame without overwriting either payload. Normal handler completion enters `CLEANING`, then releases the caught payload and leaves the frame `EMPTY`. Rethrow preserves the same payload identity and returns to `PROPAGATING` after handler-owned cleanup. |
| `CLEANING` | A particular exit edge is destroying its recorded live objects in reverse completion order. The original exception identity remains unchanged when cleanup is for propagation or a handler. Successful cleanup resumes the recorded continuation (`PROPAGATING` for unmatched/rethrown exceptions, or the appropriate normal continuation); any second exception during cleanup enters `FATAL` and skips remaining cleanup. |
| `FATAL` | Terminal state. Preserve the first exception's identity/diagnostics, record secondary-failure details when available, invoke the profile's proven whole-run-unit termination mechanism, and never return to a caller, dispatch another catch, or continue ordinary cleanup. |

Every transition is represented in logical CFG/ABI operations and checked by
the verifier; the renderer may not infer state changes from paragraph names or
COBOL text. Initialize frame fields before publication, set the active state
only after the frame's payload is complete, and clear ownership exactly once.
Every throwing call identifies the frame it may populate. A callee may publish
only into an `EMPTY` frame; nested throws in a handler use a distinct child
frame, while propagation/rethrow continue the existing frame. A second failure
in a frame in `RAISING`, `PROPAGATING`, or `CLEANING` takes the unconditional
fatal edge and cannot overwrite that frame's first payload.

The exception implementation plan's shared-context rule is reconciled with
nested throws by using one owned payload per frame and a staged child frame for
replacement. This is the required Phase 0 decision and Phase 4 ABI contract:

1. Keep the caught parent frame and payload in `HANDLER-ACTIVE`. For a new
   `throw expression` in that handler, reserve the statically assigned child
   frame, leave it `EMPTY`, and evaluate/stage the expression exactly once
   under that child's exception context. The parent remains observable and
   untouched throughout evaluation.
2. If expression evaluation throws, route that exception through the ordinary
   nested-handler CFG using the child frame. If it is caught by a nested
   handler, clear the child and resume the original parent handler. If it
   escapes the handler, it becomes the replacement and follows step 4. If
   evaluation succeeds, construct/copy/move the complete owned replacement
   payload into the child frame; only successful completion publishes the
   payload and changes the child from `RAISING` to `PROPAGATING`.
3. A failure while constructing/copying/moving the payload in `RAISING` is a
   second exception and enters terminal `FATAL`; neither the partial child
   payload nor the parent payload is exposed as a replacement. The source
   expression's own typed exceptions are distinct: they occur while the child
   is still `EMPTY` and use normal nested-handler dispatch.
4. When a complete child exception escapes the handler, first run the handler
   scope's no-throw cleanup in reverse completion order, including any
   catch-by-value parameter. Then destroy the original caught payload exactly
   once, detach the child from the retiring handler frame, reparent it to the
   parent's enclosing frame, and propagate the same child payload without
   copying or changing its dynamic type. Any cleanup/destructor failure is
   fatal and cannot replace either exception.
5. `throw;` is rethrow, not replacement: it reuses the caught frame and payload
   identity, performs handler-local cleanup, then continues propagation. It
   does not construct a child payload. A nested exception caught and completed
   inside a handler clears only its child and resumes the parent handler.

Frame IDs, parent rebinding, child-slot selection, payload ownership transfer,
and each cleanup edge are explicit logical ABI/CFG operations and are included
in the expectation manifest. No implementation may clear the parent early,
overwrite a frame with two payloads, or infer replacement from COBOL paragraph
order. Until the child-frame protocol is implemented and executed for each
supported payload shape, reject replacement/nested-throw forms whose
evaluation, cleanup, and ownership cannot be proven safe.

Physical layouts must be described using target-profile COBOL fields, not
`sizeof` a host C++ structure. The descriptor records field order, `PIC`,
`USAGE`, sign/scale, group/elementary level, occurrence count, representation,
and the profile's size/alignment interpretation. Do not assume that `COMP-5`,
`USAGE POINTER`, a C++ struct, and a native caller share byte layout across
compiler families. Any native interoperability layout is a separate
versioned boundary with explicit marshalling and conformance tests. A layout
fingerprint is computed from the canonical descriptor encoding and ABI/profile
versions, not host addresses, padding, or compiler struct layout.

##### Numeric lowering contract and physical proof

Numeric source semantics and numeric COBOL storage are separate contracts.
The language/type-policy gate must first freeze a target-neutral numeric
descriptor for every public numeric type: category (integer, binary float,
decimal if adopted), signedness, exact value range or precision/exponent
range, representation/radix where relevant, and the behavior of every
conversion and arithmetic operation. Prefer exact-width integer primitives
such as `int32`/`int64`, with `int`, `long`, and `long long` explicitly defined
as aliases or distinct fixed-width types; never let the selected COBOL
compiler's native integer width define source behavior. The same applies to
`float` and `double`: decide the promised precision, rounding, subnormal,
signed-zero, infinity, and NaN behavior before choosing a physical `PIC` or
`USAGE`. The existing backend-selected language-standard wording and older
host-oriented statements in `docs/abi_spec.md` are not a substitute for this
decision.

The typed operation IR carries the exact canonical operand and result type
IDs, operation, conversion kind, rounding rule, overflow/domain policy,
evaluation order, and failure effect. Constants retain exact source values
until type-directed lowering; they must not be prematurely parsed into a host
`int`, `double`, or target picture. Lowering does not receive opaque arithmetic
expression strings and cannot rely on COBOL's implicit promotion, truncation,
or receiver-field size to choose source semantics.

For integer operations, the selected 1.0 policy should be checked signed
arithmetic: a mathematically unrepresentable result raises
`overflow_error`, division by zero raises `domain_error`, and failed
operations leave the destination unchanged. Recommended signed division is
truncation toward zero and the remainder has the dividend's sign; the signed
minimum divided by `-1` raises `overflow_error`. Recommended shift behavior
rejects a negative count or a count greater than/equal to the operand width
with `invalid_argument`, defines signed right shift as sign-extending, and
treats left shift as checked multiplication with `overflow_error` if the
mathematical result is outside range. Adopt or explicitly replace each
recommendation in the language standard; do not inherit host-C/C++ undefined
or implementation-defined cases by accident. Integer widening conversions
preserve the value. Conversions to floating types carry an explicit
precision/rounding and inexactness policy; they are not presumed exact merely
because the destination has a wider range. Narrowing conversions must either
be explicit and checked (`range_error` on failure) or be rejected/warned
according to one adopted conversion policy.

Every failure is a typed IR outcome/CFG edge so exception propagation and
cleanup are planned before COBOL emission.

##### Checked integer-operation lowering catalogue

Use one shared, versioned integer-lowering catalogue for direct operators,
compound assignments, conversions, constant evaluation, and generated-runtime
extent arithmetic. The guard primitive reports checked overflow; its typed
caller maps that to the source contract (`overflow_error` for arithmetic,
`length_error` for allocation-size overflow). Compiler-process resource
counters use separate bounded counters and `resource_limit_exceeded`
diagnostics. For binary operations, first apply the language's mandated
operand conversions exactly once; guard against the resulting operation
type's declared `min`/`max`, and preserve any conversion failure as its own
typed edge. For a signed type with declared `min`/`max`, the guard itself
must be representable and evaluated before the operation; computing into a
same-width destination and testing afterward is too late. The profile may
prove a wider scratch representation, or use the guards below and a separate
staged result. If neither is representable and verified in that profile, use
a CBL-C helper or reject the operation.

| Operation | Required pre-operation rule | Result/failure rule |
| --- | --- | --- |
| Unary negation | Reject `x == min`; otherwise negate in a verified scratch/result representation. | `min` raises `overflow_error`; publish only after successful check. |
| Addition | If `b > 0`, overflow iff `a > max - b`; if `b < 0`, underflow iff `a < min - b`. Each bound expression is evaluated only in its safe sign case. | On either guard, `overflow_error`; otherwise compute into staged storage and commit. |
| Subtraction | If `b > 0`, underflow iff `a < min + b`; if `b < 0`, overflow iff `a > max + b`. | On either guard, `overflow_error`; otherwise compute and commit the staged value. |
| Multiplication | Exclude zero operands, then use sign-quadrant division bounds: `a>0,b>0`: `a > max / b`; `a>0,b<0`: `b < min / a`; `a<0,b>0`: `a < min / b`; `a<0,b<0`: `a < max / b`. Divisions use the adopted truncation-toward-zero rule and these sign cases avoid the `min / -1` intermediate. | `overflow_error` on a failed bound; multiplication itself still uses a proven wide scratch or helper, never a narrow result followed by inspection. |
| Division and remainder | Reject divisor zero. Also reject `a == min && b == -1` for both operators so quotient/remainder share one explicit signed-division domain. | Zero raises `domain_error`; the minimum/-1 case raises `overflow_error`; otherwise quotient truncates toward zero and remainder has the dividend's sign. |
| Left shift | Require `0 <= count < width`; compute by repeated checked doubling (or a profile-proven equivalent) so no intermediate overflows before detection. | Invalid count raises `invalid_argument`; an unrepresentable mathematical product raises `overflow_error`; count zero preserves the operand. |
| Right shift | Require `0 <= count < width`; use a verified arithmetic shift or compute floor division by `2^count` (adjust truncating division downward for a negative nonzero remainder). Special-case `count == width - 1` or prove a wider divisor; positive `2^(width-1)` is not representable in the signed source type. | Invalid count raises `invalid_argument`; signed values sign-extend exactly as the adopted language rule specifies. |
| Float-to-integer conversion | Apply the adopted finite-value and rounding rule; reject NaN/infinity and compare against destination bounds before conversion. | Out-of-range/non-finite input raises `range_error`; in-range fractional input truncates or rounds exactly as the frozen source rule states. |
| Integer-to-float conversion | Apply the destination precision's declared rounding/inexactness policy and prove the integer lies within the float's finite range before conversion. | Overflow raises `range_error`; in-range but inexact values follow the declared rounding rule identically across profiles. |
| Narrowing conversion | Compare the source mathematical value against destination `min`/`max` before any cast or `MOVE`. | Explicit checked narrowing raises `range_error` and leaves the destination unchanged; implicit narrowing is handled by the adopted semantic warning/error rule. |

This catalogue describes the current signed integer recommendation. If an
unsigned integer family is selected for 1.0, it needs its own explicit range,
wrap/overflow, division, remainder, and shift rules and guard rows; it must not
be routed through these signed predicates.

These predicates are the semantic reference, not mandatory COBOL text. A
profile lowering may simplify them only when its verifier proves equivalent
behavior for every boundary value and its generated-output checks can detect
removal, reordering, or narrowing of the guard. All operands are evaluated
once in source order before guards that depend on them. A failed guard follows
the typed exceptional edge and cleanup plan; it does not partially update the
destination. `ON SIZE ERROR` may supplement the prechecks, but cannot replace
them unless the operation/type/profile matrix explicitly proves its covered
conditions and failure-state behavior.

Constant folding uses the same catalogue and source contract as runtime
lowering. The compiler evaluates integer constants in a checked mathematical
domain (or an equivalent multi-precision representation), then checks the
declared destination range; it must not rely on host signed-overflow behavior.
Floating constant folding is permitted only when it reproduces the selected
source precision, rounding, and exceptional-value rules; otherwise retain the
operation for runtime or issue the specified diagnostic. Compile-time and
runtime cases at identical boundaries must produce the same value or typed
failure category.

Each target profile supplies a versioned lowering entry for every supported
`(source operation, source type, destination type)` tuple. A tuple is either
(a) emitted as COBOL with explicit range/zero guards and a staged result,
(b) implemented by a separately named CBL-C numeric helper whose body is
compiled through the normal native-library path, or (c) rejected with a
stable, source-mapped capability diagnostic before rendering. The compiler
must prove that the chosen intermediate has sufficient range and precision
for the operation, or use an overflow-safe algorithm; computing an unchecked
expression into a wider-looking `PIC` is not proof. Stage the result and only
publish it after all checks succeed. `ON SIZE ERROR` may be used only where
the exact COBOL profile/compiler behavior has been probed and shown to match
the CBL-C operation contract; it is not a portable overflow policy by itself.
For example, GnuCOBOL documents `ON SIZE ERROR` for result-size failures and
division by zero, and documents explicit `ROUNDED` modes; profile lowering
must account for the selected mode rather than relying on the defaults
([GnuCOBOL Programmer's Guide, arithmetic statements](https://gnucobol.sourceforge.io/HTML/gnucobpg.html)).

Floating operations require equivalent evidence for result precision,
rounding, exceptional values, and comparisons. A profile may directly use a
native floating representation only when its tested behavior conforms to the
source descriptor. Otherwise it uses a conforming CBL-C implementation or
rejects that type/operation on the profile. It must not silently replace
binary32/64 semantics with decimal arithmetic, flush subnormals, canonicalize
NaNs, or vary exceptional results by COBOL vendor unless that exact behavior
is explicitly adopted in the source numeric contract. Parsing/formatting is a
separate conversion operation with strict `parse_error`/`range_error`
outcomes, not an implicit numeric cast. GnuCOBOL's FAQ documents IEEE-754
representation for its `COMP-1`/`COMP-2` fields, but that is evidence for the
pinned GnuCOBOL profile only, not a portable COBOL guarantee
([GnuCOBOL FAQ, `COMP-1` and `COMP-2`](https://gnucobol.sourceforge.io/faq/gcfaq.html)).

##### Floating-point operation lowering catalogue

Phase 0 must decide whether CBL-C `float`/`double` promise binary32/binary64
behavior, another explicitly named model, or a bounded subset. The backend
must not infer this choice from a COBOL `PIC`, a host C++ `float`/`double`, or
one compiler's `COMP-1`/`COMP-2`. Once adopted, record a profile lowering
entry for each concrete operation tuple: opcode, canonical operand/result
type IDs, operand conversions, evaluation precision, rounding point/mode,
exceptional-value policy, possible typed failure outcomes, and selected COBOL
operation or CBL-C helper. This entry is an expectation-manifest item and a
versioned lowering/cache input.

| Operation family | Backend obligation | Required decision/proof |
| --- | --- | --- |
| Literal conversion | Parse the source token as an exact decimal/radix value, then round directly to the declared source type; do not route through a host `double` or target COBOL decimal temporary. Preserve signed zero where the grammar and literal semantics permit it. | Freeze accepted literal grammar, overflow/underflow behavior, rounding mode (recommended default: nearest, ties-to-even for IEEE binary types), and whether a subnormal result is preserved or diagnosed. Compile-time conversion must match runtime conversion. |
| Unary `+`/`-` | Apply the operation at the declared source precision. Unary minus must implement the selected signed-zero and NaN-sign policy; unary plus must not unexpectedly widen or canonicalize. | Profiles that cannot preserve the specified representation/behavior must use a conforming CBL-C implementation or reject the operation. |
| `+`, `-`, `*`, `/` | Convert operands once under the language's usual-arithmetic-conversion rule, evaluate in the specified operation precision, and round at the specified boundary before exposing the result. Preserve source grouping and left-to-right observable evaluation; do not contract `a*b+c` into FMA unless the source operation explicitly requests fused semantics. | Decide whether division by zero, invalid operations, overflow, and underflow produce IEEE values, typed exceptions, or another specified outcome. If exceptions are selected, the operation has explicit typed CFG edges; if IEEE results are selected, do not turn a vendor trap into unmodeled termination. |
| Comparisons | Implement ordered/unordered behavior exactly, including `NaN == x`, `NaN != x`, and relational comparisons; preserve signed-zero comparison behavior and the source truth type. | Record NaN comparison rules in the source descriptor. A profile intrinsic is usable only after NaN and signed-zero probes demonstrate the same result for each relation. |
| Integer-to-float | Convert from the exact integer value to the selected destination format and declared rounding mode. | Probe values around the precision boundary (including adjacent integers near `2^p`), the largest finite value, and overflow. Do not claim exactness based only on destination exponent range. |
| Float-to-integer | Apply the source's specified truncation/rounding rule, then check finiteness and destination range before conversion. Keep the destination unchanged on failure. | NaN, infinities, both signed zeros, fractional values on either side of zero, exact bounds, and adjacent representable values just outside each bound must follow the same typed outcome on every profile. |
| Float-width conversion | Convert directly between the declared source and destination formats; preserve any promised subnormal, signed-zero, infinity, and NaN behavior. | Decide whether NaN payload/signaling state is observable. If not, state permitted canonicalization explicitly; if it is observable, the profile must preserve it or reject that conversion. |
| Remainder and math intrinsics | Give each adopted operation its own source definition (for example, truncating remainder versus mathematical modulo) and operation-specific accuracy/error contract. | Do not map `%`, `fmod`, remainder, square root, trigonometric, or logarithmic operations to a similarly named COBOL intrinsic by spelling alone. For transcendental functions, define a required accuracy bound and exceptional domain behavior, or defer/reject until a CBL-C implementation and independent oracle exist. |

Every native floating lowering must state whether COBOL evaluates the
expression in the declared binary precision or a decimal/intermediate
precision, where rounding occurs, and whether assignments introduce an extra
rounding step. `ROUNDED` is not by itself proof of IEEE binary rounding. If a
profile cannot force the required precision boundary without changing the
source result, it must call a separately named CBL-C helper through the
ordinary native-library path or issue a source-mapped unsupported-capability
diagnostic. Never compensate with a C++ runtime call, host math-library call,
or renderer-side rewrite. Any helper that may fail or allocate is first
inserted as a typed operation, then CFG/lifetime analysis is rerun before
physical lowering.

Constant evaluation uses the same source-type semantics as runtime lowering.
Retain numeric tokens in an exact representation until their target type is
known, then use a correctly rounded conversion algorithm for the adopted
format. If a fold cannot be proven equivalent to runtime semantics, leave the
expression for runtime; do not substitute a host-dependent approximation.
Compile-time diagnostics and runtime outcomes must agree on overflow,
underflow, invalid conversion, and domain cases. Optimizers may not reassociate
floating expressions or remove signed-zero/NaN-sensitive operations unless
the language defines a separately specified and tested relaxed-math mode.

Floating conformance tests compare the exact promised representation (or the
explicitly adopted accuracy bound), not formatted decimal output alone. Use
independent high-precision or exact-rational conversion oracles, bit-pattern
fixtures for representation-sensitive behavior, and deterministic randomized
coverage across sign, exponent, and significand classes. Include zero, negative
zero, smallest/largest subnormal, smallest normal, adjacent values around
powers of two and integer precision boundaries, largest finite, infinities,
quiet/signaling NaNs where representable, conversion ties, and domain
boundaries. Add output mutations that alter an operand conversion, rounding
point/mode, comparison predicate, failure edge, or FMA contraction; the
structural verifier or executable oracle must catch each mutation. Keep
profile probes separate from the language oracle: a probe records observed
compiler behavior, while only comparison against the frozen source contract
can grant the profile capability.

The expectation manifest records the canonical source numeric type/operation
contract and the selected lowering for each concrete operation. The ABI
descriptor records canonical numeric type IDs for public slots and their
physical projection: exact `PIC`, `USAGE`, sign, scale, representation,
boundary marshalling/conversion rules, profile ID/version, and ABI version.
The ABI fingerprint changes when a physical slot/layout or ABI-boundary
mapping changes. A profile's operator guard, intermediate, helper, or rounding
strategy is separately versioned lowering input: a change that preserves
source semantics and the physical ABI invalidates lowering artifacts and
evidence, but does not by itself change the ABI fingerprint. Source-interface
and specialization identity change when the adopted source numeric type or
operation contract changes. A producer and consumer agree by canonical
numeric type ID and field-by-field profile descriptor, not by coincidentally
matching picture text. Renderer/verifier checks ensure emitted arithmetic,
guards, `ON SIZE ERROR`/`ROUNDED` clauses, and helper calls match the
expectation/lowering manifest and typed IR; tests must not merely look for an
expected `PIC` marker.

Implementation order: Phase 0 adopts the source numeric descriptor and probes
each pinned compiler/profile with boundary and rounding programs; Phase 1
records it in typed IR and diagnostics; Pass 7 adds the profile mapping;
Pass 8 seals numeric ABI fields into the canonical descriptor; the generated
COBOL verifier checks structure and semantics; only then may a profile claim
numeric support. A profile that cannot provide a type's complete operation
set must reject that type at semantic/profile validation rather than silently
weaken its contract.

##### Canonical ABI descriptor encoding, fingerprints, and evolution

Define the descriptor as a versioned semantic record, not a serialized dump of
a C++ struct and not a generated-name signature. Its canonical encoding must
be identical in the provider interface, consumer call record, link manifest,
and expectation manifest. Reuse the interface artifact's canonical field
ordering, UTF-8 and escaping rules, integer encoding, and duplicate-field
rejection; include an explicit descriptor-schema version so changing the
encoding itself cannot masquerade as an ABI-compatible update. Encode
structural type IDs by their canonical type-graph representation, never by a
pretty-printed spelling whose punctuation or alias normalization can change.

The descriptor must encode explicit presence/state for every optional or
unknown fact. In particular, `non-throwing`, `may-throw`, and an unknown throw
effect are three distinct values; `void result` is different from a missing
descriptor field; absent exception context is different from a zero-filled
context; and a zero-length layout is not a wildcard. A parser rejects missing
required fields, unknown required enum values, duplicate fields, invalid
references, and incompatible schema versions before making any signature
visible to semantic resolution.

Compute independent digests from canonical inputs:

- `source_interface_hash`: exported source symbols, signatures,
  receiver qualification, ownership/borrow and failure semantics, plus public
  template identity and constraints, the referenced template-body hash, and
  `interface_version`.
  It excludes private implementations, host paths, timestamps, and physical
  details that do not alter the public source contract.
- `abi_fingerprint`: canonical physical ABI descriptor, including canonical
  provider/symbol identity, ABI-frozen external entry spelling, receiver
  presence/type/mode/projection, ordered explicit-argument and hidden-slot
  layouts, result/context/payload layouts, ownership
  transfer, `logical_call_contract_version`, target profile ID/version,
  descriptor schema version, and `abi_version`. It excludes source locations, local paragraph/data names,
  comments, temporary allocation order, and private implementation body bytes.
- `template_body_hash` and `implementation_hash`: separately identify
  instantiable generic bodies and private code/artifact content so consumers
  can invalidate the right work without conflating implementation edits with
  physical ABI changes. A public template-body edit changes its body hash and
  the source-interface reference, invalidating dependent specializations;
  neither change implies a physical ABI change by itself.

Keep these semantic digests separate from the interface-artifact integrity
digest. The latter hashes the complete canonical serialized artifact payload
with only its own integrity field omitted; it detects corruption or
substitution, but is not a compatibility decision. Each semantic digest hashes
only its named canonical projection with its own domain prefix.

Use SHA-256 over domain-separated canonical byte strings (for example,
`CBLC-SOURCE-INTERFACE-v1\0`, `CBLC-ABI-DESCRIPTOR-v1\0`,
`CBLC-TEMPLATE-BODY-v1\0`, and `CBLC-IMPLEMENTATION-v1\0`, followed by the
canonical payload). Store the complete digest in serialized artifacts; a
truncated digest may be used in cache filenames only, with full-digest
verification on lookup. A digest is a fast mismatch detector, not proof of
compatibility: the linker validates versions and compares canonical
descriptors field-for-field after the digest matches. Never accept a caller
because its source name or truncated fingerprint happens to match.

Version changes follow three independent rules. Increment
`logical_call_contract_version` when target-neutral receiver placement/mode,
logical argument ordering or meaning, result completion/ownership transfer, or
exception-context semantics change. Increment `interface_version` when a public source-level
signature, ownership/borrowing, failure, exception, template-constraint, or
other promised source semantic changes. Increment `abi_version` when any
physical entry name, slot order/presence, picture, usage, sign/scale, group
order, pointer/handle encoding, payload/result layout, profile contract, or
physical ABI descriptor interpretation changes. If one change affects more
than one contract, bump every affected version. A body-only private edit changes the implementation hash;
it does not justify bumping the public interface or ABI versions unless its
observable contract or exported layout changes. Do not infer compatibility
from a numerically greater version: compatibility must be declared by an
explicit adapter whose source and target descriptors are both pinned and
tested. Unknown versions, fields, profiles, or effects fail closed before
rendering/linking.

The ABI fingerprint must be reproducible across compiler hosts and builds:
serialize canonical IDs and profile-owned layout facts in the specified byte
order; do not hash `sizeof`, native endianness, pointer values, object padding,
absolute paths, compiler memory layout, unordered-map iteration, or
human-readable diagnostic text. A conformance test should independently
re-encode fixtures and verify known digest vectors, then mutate each
ABI-significant field one at a time and each ABI-irrelevant field separately.
This tests the descriptor contract rather than merely comparing two values
produced by the same potentially faulty implementation.

The present `cblc_struct_layout_fingerprint()` in
`src/cblc/transpiler_cblc_storage.inc` is a recursive 64-bit FNV-style hash of
source-level field facts, and `layout_fingerprint` is stored as
`unsigned long long` in the current type signature. It is useful as an
in-process structural-change check, but it is not this exported ABI
fingerprint: it does not encode a complete profile-lowered physical descriptor,
the physical call slots, frozen external entry name, or descriptor/ABI
versions, and it cannot be compared field-for-field. Preserve it only for the
limited internal use that its input facts actually justify; introduce a
separate canonical ABI descriptor and full-digest representation for
serialized interfaces and link validation. Do not relabel or widen the old
hash and imply that its omitted inputs have thereby been covered.

#### Source provenance, diagnostics, and generated-source mapping

Every typed operation, CFG edge, lifecycle action, ABI slot, COBOL module-IR
node, and rendered COBOL range must retain machine-readable provenance. A
single source span is insufficient for generated code: one cleanup block may
be justified by a scope exit and several live declarations; a template
specialization has both a template-definition site and an instantiation/use
site; an ABI field may originate from an exported signature rather than a
single statement. Represent provenance as an ordered set of origin records
with roles such as `primary_source`, `template_definition`,
`template_instantiation`, `scope_exit`, `lifecycle_operation`,
`abi_declaration`, `imported_provider`, and `compiler_generated`. The primary
origin is used for the first diagnostic label; secondary origins explain why
the node exists.

The current `t_transpiler_source_span` records a path and start/end line and
column, and `t_transpiler_source_map_entry` pairs a CBL-C span with a COBOL
span. `transpiler_context_map_cblc_to_cobol()` and its reverse counterpart
currently return the first containing pair. That is a useful initial mapping
API, but it cannot by itself represent one-to-many or many-to-one origins,
resolve overlapping generated ranges deterministically, distinguish
compiler-generated text from user text, or retain template/cleanup origin
chains. Keep the API as a compatibility projection while the backend moves to
provenance records attached to typed IR and module-IR nodes.

Define source locations against canonical module IDs, not absolute checkout
paths. Internally retain half-open UTF-8 byte-offset ranges `[start,end)` as
the stable identity and derive human locations only from the exact hashed
source snapshot. For CBL-C source diagnostics, line and column are 1-based;
line starts follow the lexer, and a column counts Unicode scalar values from
the start of that line (a tab is one source scalar, not a display-width
expansion). The source contract accepts LF and CRLF line endings, treating
CRLF as one line break; reject a lone CR rather than allowing platform-specific
line counting. Reject malformed UTF-8 before semantic analysis, and do not
normalize Unicode or rewrite source bytes before computing spans. An LSP
adapter converts scalar columns to the protocol's negotiated UTF-16 position
units; it must not change the canonical byte-offset identity. A terminal may
render display-cell alignment separately, but display width is never used as
an internal coordinate.

Generated COBOL mappings likewise use half-open byte offsets in the exact
rendered artifact as identity. Their human line/column view is derived from
the selected source-format/profile encoding and physical-line rules, not from
the host text API. Each compiler-family/version adapter declares whether its
reported column is a byte, code-unit, or character position and converts that
coordinate to a generated-artifact byte offset before source-map lookup.
Unknown column semantics or out-of-range locations remain external-tool
diagnostics with raw coordinates; never guess a CBL-C span. Persist both the
source hash and normalized module ID with serialized provenance so a stale map
cannot silently point at a new version of the file.

Have COBOL module-IR construction attach provenance before rendering. The
renderer records the generated byte/line/column range for each emitted node
as it writes it, then emits a source-map artifact containing module/profile
identity, generated artifact hash, source snapshot hashes, origin-role lists,
and generated ranges. Preserve a mapping for declarations, statements,
paragraphs, ABI/linkage fields, and synthetic lifecycle/exception cleanup.
Formatting or line wrapping may change generated positions but must not change
the underlying origin identities. Use deterministic mapping precedence for
overlaps: exact generated token/node, enclosing COBOL node, then compiler-
generated origin with related source labels. Do not return whichever mapping
happens to be first in an insertion-order array.

For multiple containing ranges of the same precedence, select the smallest
range, then the most-specific module-IR node kind, then the lowest stable node
ID. Return one primary origin and the complete ordered related-origin set,
deduplicated by canonical origin ID and role. Inverse lookup returns every
generated range for a source range, sorted by artifact ID and generated byte
offset; it is explicitly one-to-many, not an arbitrary first match. A query
outside all mapped ranges returns `unmapped` rather than the nearest range.

All compiler diagnostics use one structured form: stable diagnostic ID,
severity, phase, primary origin, zero or more related origins, target profile,
and a concise message. Diagnostics must distinguish user errors (parse,
semantic, template, lifetime, ABI, unsupported-profile) from internal
invariants and external-tool failures. Sort independent diagnostics by
canonical module ID, primary source byte offset (or a stable artifact/node key
when no source origin exists), phase ordinal, diagnostic ID, canonical subject
ID, and the ordered related-origin key. Assign each record a deterministic
instance key from those semantic fields plus the exact source-snapshot digest
(or artifact/input-snapshot digest when there is no source origin) and the
selected concrete specialization/instantiation context when applicable. This
key is deterministic for one frozen input snapshot, not a persistent
cross-edit diagnostic handle. Merge exact duplicate
instance keys emitted by multiple verifier paths; do not merge diagnostics
whose related origins or specialization context differ. Parallel pass
completion order must not change diagnostic identity, de-duplication, or order.
Keep a versioned diagnostic catalog whose message template and typed
parameters are separate from the stable code/instance identity; terminal
color, localization, formatting, and path presentation are not semantic
identity. Never expose pointer values, temporary absolute paths, or
hash-table iteration order in stable output.

Map failures at each stage deliberately:

- Parser/semantic/CFG/ABI/profile diagnostics point to source tokens or
  declarations, with imported provider and template-definition spans as
  related labels.
- The internal COBOL-subset validator reports generated line/column and the
  mapped primary/related CBL-C origins. If no mapping exists, label the
  generated runtime/ABI node and report that provenance is unavailable; do
  not guess a nearby source statement.
- A COBOL compiler adapter is versioned by compiler family/version and parses
  that tool's diagnostic format. It translates reported source positions
  through the generated-source map. Preserve raw command, stdout, stderr,
  exit status, and unparsed diagnostics as build artifacts; unknown formats
  remain explicit external-tool diagnostics, not fabricated CBL-C locations.
  Compile acceptance is a separate structured result: it requires a completed
  supervised invocation with the tool-adapter-mapped successful process
  result, every required output present and validated against its manifest,
  and compliance with the pinned diagnostic policy. A recognized error
  diagnostic rejects acceptance even if the process exits zero. `allow`
  retains recognized warnings without rejecting; `reject` rejects on any warning;
  `promote_to_error` treats a recognized warning as an error. For unclassified
  diagnostics, the tool policy must choose `reject` or `inconclusive`; neither
  can satisfy compile evidence. Human-readable text or a diagnostic parser
  alone never overrides process status or output validation.
- Link-planning errors identify both the consumer call/import site and the
  provider export/interface location when known. Missing provider data is
  labeled as unavailable rather than assigned a fabricated source span.

Artifact publication is transactional with respect to diagnostics: parse,
semantic, profile, manifest, and structural-validation failure must leave no
success-shaped interface, COBOL module, source map, or link manifest. Keep
failed diagnostics and raw external-tool output in the isolated test/build
directory for inspection, but never overwrite the last known-good artifact.
The normal command-line exit code is derived from structured diagnostic
severity/category; a successful COBOL parse alone cannot clear an earlier
semantic or ABI error.

Add focused traceability tests: `BACKEND-PROVENANCE-001` checks source-to-IR-
to-CFG-to-module-node-to-rendered-range edges for a direct call, synthetic
cleanup, imported call, and template specialization; `BACKEND-PROVENANCE-COORDINATES-001`
checks UTF-8 byte offsets and scalar columns for ASCII, multibyte BMP and
supplementary characters, combining sequences, tabs, LF/CRLF, and malformed
UTF-8, plus scalar-to-UTF-16 LSP conversion and byte/code-unit/character
compiler-column adapters; `BACKEND-SOURCE-MAP-001`
checks both directions, multiple origins, overlap precedence, line wrapping,
stale source hashes, and generated-only nodes; `BACKEND-DIAGNOSTIC-ORDER-001`
permutes module and parallel-pass discovery while requiring byte-stable
diagnostic order and paths; `BACKEND-TOOL-DIAGNOSTIC-001` maps known compiler
diagnostics and preserves unknown ones verbatim; and
`BACKEND-ARTIFACT-ATOMIC-001` injects failure after each build stage and
proves that no partial success artifact replaces a prior valid output.

#### Resource budgets, cancellation, and bounded compilation

The repository already has narrow template guards: `s_cblc_template_depth_guard`
in `src/cblc/transpiler_cblc_struct_parse.inc` checks recursive depth, while
`src/cblc/transpiler_cblc_unit_parse.inc` initializes per-translation-unit
limits of 1,024 instantiations and depth 32. Preserve and test those controls;
this roadmap is not proposing removal. They do not yet constitute an
invocation-wide policy: they do not cover source/AST growth, import graphs,
typed-IR/CFG/dataflow work, diagnostics, rendered bytes, cache summaries, or
external compiler processes. Phase 0 must inventory other existing guards and
their effective scope before consolidating them under the policy below.

Every compiler invocation must have explicit, configurable limits for work
whose growth is controlled by source or imported artifacts. Template nesting
is only one such limit. Bound at least source files and aggregate source bytes,
tokens/AST nodes, import-graph nodes and edges, template specializations and
substitution work, typed-IR/CFG nodes, dataflow transfers, diagnostics,
generated module count and bytes, and retained external-tool output. Use
checked arithmetic before incrementing or multiplying counts and before
reserving any compiler-owned buffer. A counter overflow is a resource-limit
diagnostic, never wraparound or an implicit unlimited value. Limits apply to
the compiler itself; they do not alter the semantics or object-size limits of
valid generated programs.

Represent the active limits in one immutable invocation policy, with profile
defaults and optional CLI/build-configuration overrides. Document each limit's
unit, default, accepted override range, and failure diagnostic. Do not use
host-specific memory probes as the language's resource contract: enforce
portable deterministic work/count budgets, and use an optional host memory
watchdog only as an operational safeguard reported separately. Reaching a
configured budget produces a structured `resource_limit_exceeded` diagnostic
with the dimension, configured limit, observed count, phase, and source or
artifact origin when available. It must not be reported as a syntax/type error,
silently truncate work, or produce plausible success-shaped COBOL.

Budget accounting must be deterministic across worker counts and discovery
order. Before parallel work begins, derive each module's phase quota from the
immutable policy and canonical module ID; never let a shared racing counter
decide which module gets the remaining budget. Charge stable logical units
such as visited IR nodes, CFG edge transfers, and unique specialization keys,
not elapsed time or worker completion order. Enforce per-module quotas during
the pass. At deterministic phase barriers, sum completed module charges in
canonical ID order against the invocation total; report the first canonical
module whose prefix exceeds that total. This gives parallel execution a
bounded per-module overshoot and the same selected failure as serial execution.
A cache hit must carry and validate the same resource-summary data as a fresh
pass, so warm-cache builds cannot bypass an input's configured semantic limits
or change which module deterministically receives a failure.

Cancellation is cooperative at bounded pass boundaries and within potentially
long worklists, template expansion, rendering, and external-tool supervision.
Check cancellation without committing partial pass results. Cancellation has
a distinct non-success process result and structured diagnostic; it is never
translated to an empty module, successful no-op, or user semantic error. On
resource exhaustion, cancellation, or external-tool timeout/output overflow,
discard the current pass's unpublished artifacts, preserve the previous
known-good artifacts, and retain diagnostics plus capped raw tool output in
the isolated build directory. A COBOL compiler wall-clock timeout is an
external-tool policy, not a deterministic compiler semantic budget; report it
as a tool timeout with the command/profile and do not claim a CBL-C source
location unless the tool supplied a mapped position.

Exercise exact boundary behavior at limit minus one, the limit, and limit
plus one for every budget dimension. Include deeply recursive/cyclic imports,
high-fan-out template graphs, repeated equivalent specializations, CFG/dataflow
growth, diagnostic floods, generated-output expansion, and a deliberately
non-terminating or output-flooding compiler adapter. Run the same fixtures
serially and with varied worker counts, cold and warm cache, and reordered
file discovery. Prove deterministic diagnostics, no integer wraparound or
hang, bounded retained output, cancellation latency at documented check
points, no leaked temporary storage, and no partial artifact publication.
Resource defaults must be high enough for the supported 1.0 conformance
corpus; report corpus maxima and headroom before freezing defaults.

#### Incremental implementation sequence and gates

1. **Inventory and seams (Phase 0):** map parser, semantic resolution,
   lifecycle parsing, call/ABI matching, exception lowering, declaration
   emission, paragraph emission, rendering, and validation to current source
   files. Document each stage's current input/output and all global or mutable
   state. Define the pass dependency/invalidation graph and transactional
   publication contract from the preceding subsection before enabling cache
   reuse or parallel scheduling. Add pass-level tracing in test/debug builds
   without changing emitted program behavior. The trace records pass/schema
   IDs, input digests, verifier results, invalidated analyses, and published
   artifact digests; it must not expose host paths or change semantic IDs.
   Resolve the COBOL memory-model conflict by running the typed-pool and
   native-allocation probes in "Heap and pointer ABI decision and
   implementation plan"; satisfy `BACKEND-PORTABILITY-PROFILE-001` with
   independent generated-source parsing plus compile/link/run evidence for
   every claimed heap capability; publish exact profile capabilities and
   resource limits before freezing the 1.0 profile matrix. Freeze module-scope constant
   versus dynamic initialization, the `.cbi` link-effect record/hash schema,
   canonical memory-region IDs and alias/escape rules, complete global
   read/write effect metadata, cycle rules, root exception record, bootstrap
   ownership, and normal/failing shutdown order from the
   program bootstrap subsection; define their cache invalidation boundaries.
   Do not permit global owning-class lowering until its link-time initializer
   graph and failure-unwind probe pass, including
   `BACKEND-EFFECT-REGIONS-001` for its global read/write and alias model.
2. **Typed source-operation IR (Phase 1):** freeze the internal schema version,
   canonical-ID rules, effect/outcome states, and verifier contract in the
   preceding subsection, then introduce a typed, immutable backend
   operation/value/place view over the existing translation unit. Add
   `BACKEND-TIR-VERIFY-001` before widening the opcode set. First cover
   loads/stores, scalar assignment, checked access, calls,
   source-order argument evaluation/staging, constructors/destructors, return,
   and throw, including owned-versus-borrow results and structured effect
   summaries. Generic bodies may retain
   dependent member/overload/lifecycle lookups, but each must name its canonical
   candidate set and substitution operands. Verify definitions, uses, types,
   projections, sequencing, and failure summaries after construction and each
   transform. Reject unresolved/overflowed source text operands instead of
   reparsing them during emission.
3. **Specialization and concrete dispatch (Phase 1/3):** structurally
   instantiate generic bodies before CFG construction. Resolve each dependent
   call, conversion, operator, and lifecycle operation against the concrete
   substitution; record exact canonical targets and concrete effects. Memoize
   by canonical specialization key, enforce recursion/depth/work budgets, and
   distinguish same-key recursive calls from expanding instantiation cycles.
   Compute recursive call-SCC summaries with the declared effect lattice,
   conservative imported-record states, and deterministic work budget; pass
   `BACKEND-EFFECT-SCC-001` before CFG construction. Validate acyclic by-value
   layout/lifecycle dependencies separately. Verify no open types or dependent
   lookups remain in emitted functions. Compare
   pre/post-specialization typed IR and call graphs, including `vector<string>`
   and nested cases, before allowing CFG construction.
4. **CFG construction (Phase 1/4):** build normalized basic blocks,
   terminators, and typed control-flow edges from concrete specialized
   functions for current `if`, `while`, `try/catch`, calls, checked accesses,
   allocations, construction, logical-operator short-circuit regions when
   adopted by the language standard, and exits. Every may-fail operation receives
   explicit outcome edges and defines results only on success. Calls' and
   operations' failure edges use the effects selected after specialization.
   Move lexical cleanup calculation from parser-attached
   per-statement lists to edge-derived lifecycle operations. Phase 1 proves
   non-exceptional exits and passes `BACKEND-DATAFLOW-001` against the
   independent finite-state reference solver before ownership lowering is
   enabled; Phase 4 extends the same CFG to every throw,
   call-failure, and payload edge. Do not construct an ownership CFG from a
   generic body and later patch in different concrete callees.
5. **Ownership and lifecycle operation lowering (Phase 1/2):** lower storage
   reservation, construction completion, type-directed copy/move, assignment,
   destruction, and deallocation separately using Pass 3's concrete dispatch
   table. Make scalar and class test traces inspectable. Then implement vector
   live-range transitions and prove `vector<string>` through actual COBOL
   execution; nested vectors must show each concrete lifecycle edge rather than
   inherit a scalar/value-like fallback.
6. **Logical ABI and profile projection (Phase 0/1/4):** first inventory and
   reconcile the current ABI documentation and emitted call layouts. Pass 6
   builds the target-neutral logical contract for every local, imported,
   library, method, and constructor entry, including slot order, result/context
   presence, and payload semantics, while recording `interface_version` and
   `logical_call_contract_version` but not assigning the physical ABI version.
   Before projecting any logical slot, Pass 7 reads the export-pinned
   `abi_version` and requires an exact projection in the selected target
   profile. Imported consumers preserve the provider's version; no default,
   highest-version, or host-derived selection is allowed. Pass 8 freezes
   ABI-visible names, composes the final descriptor with that version, and
   computes the profile-bound fingerprint. Do not insert a second semantic
   copy/allocation during physical marshalling. Verify post-profile-ABI
   CFG/lifetime equivalence, or rerun those analyses if an explicitly modeled profile thunk
   adds a lifetime-affecting operation. Project the logical heap handle and
   allocation contract through that same profile manifest; generated
   application operations must not bypass the selected heap adapter.
   Phase 1 proves non-throwing/scalar and ordinary class calls; Phase 4 proves
   the complete throwable payload/context path and passes
   `BACKEND-ABI-ARGUMENT-FAIL-001` for staged argument construction, ownership
   transfer, and cleanup. Cross-module signature
   mismatches fail before rendering.
7. **COBOL module IR and name allocation (Phase 1/0 verifier work):** first,
   verified CFG structuring produces a versioned control-region plan and
   passes `BACKEND-CFG-POSTDOM-001`, `BACKEND-CFG-PLAN-VERIFY-001`, and
   `BACKEND-CFG-LOWERING-001`. Profile lowering then creates a closure-complete
   name-request set for source/ABI entities and every generated declaration
   and paragraph required by control regions, activation layouts,
   lifecycle/frame state, exception/cleanup lowering, heap/runtime adapters,
   and dispatch. Derive activation-layout and other generated planning
   manifests using stable logical identities and request IDs, without COBOL
   spellings; their field/paragraph requests must already belong to that
   complete set. Allocate and independently verify the canonical name table
   only after this closure is complete, then freeze it before module-IR
   construction. If a later transform discovers a name-consuming node, restart
   the dependent planning/allocation transaction; it must not append a name
   to the frozen table. The module-IR builder maps every mutable activation
   field and access to its manifest entry and frozen name-table reference; run
   `BACKEND-ACTIVATION-STORAGE-BOUND-001` and
   `BACKEND-ACTIVATION-LAYOUT-EMISSION-001` against the independently parsed
   emitted declarations/accesses and recursive runtime traces. Implement the
   canonical deterministic allocator in Section 6.6 and pass
   `BACKEND-NAME-ALLOCATION-001` golden vectors, `BACKEND-NAMES-001` end-to-end
   allocation permutations, `BACKEND-NAME-DIGEST-COLLISION-001` forced
   collision groups, and `BACKEND-NAME-MAP-001` serialized-map checks.
   Before module-IR construction, freeze and independently verify the name
   table, then derive and seal the semantic expectation manifest from verified
   typed IR, CFG/lifecycle, ABI/profile descriptors, and that frozen name
   table—not from module IR or rendered output. The module-IR builder then
   maps declaration, linkage, paragraph, call, and branch-target decisions to
   structured nodes referencing canonical IDs and frozen names; verify that
   IR before a pure renderer serializes it. Independently parse the exact
   rendered bytes, compare both IR and parsed output to the already-frozen
   semantic expectations, and emit the separate validation-correlation report
   only from those observed representations. Test serialize/deserialize and
   parse/serialize/parse stability plus name-map validation per target
   profile. Existing exact-text goldens remain formatting checks only and
   cannot provide semantic expectations.
8. **Profile matrix and retirement:** run the same typed operations, ABI
   fixtures, and output manifests across every required COBOL profile. Remove
   old text-substitution or direct-rendering paths only after coverage shows
   all supported operations route through the validated pipeline. Keep compiler
   C++ limited to compiler transformations and ABI marshaling; generated COBOL
   executes the behavior.

#### Backend exit criteria

The backend architecture is ready for 1.0 only when:

- every supported source operation has a resolved typed-IR operation and
  source span before lowering, with complete effects/outcomes and a passing
  verifier; no semantic operand remains available only as reparsed text;
- every generic definition has a typed dependent-operation representation,
  and every concrete specialization resolves dependent overloads and lifecycle
  actions before its CFG is constructed;
- every reachable control-flow exit has a CFG edge and an auditable cleanup
  plan, with exact live-object state and reverse destruction order;
- every lifecycle operation and generic dispatch target is type-directed and
  visible in a machine-readable call graph;
- every generated call, result/context record, and module entry agrees with
  the versioned ABI manifest;
- `new`, `new[]`, `delete`, `delete[]`, string storage, and container growth
  lower only through the versioned logical heap contract; allocation is
  distinct from construction, typed handles cannot escape their ownership or
  borrow rules, and every enabled profile has evidence for failure, release,
  and rollback behavior;
- each final linked executable has one compatible heap domain shared across
  all modules; imported calls preserve handle identity and route access,
  destruction, and release through the owning domain, while incompatible or
  foreign-domain handles fail closed;
- the common-profile heap strategy has passed its required 1.0 allocation
  probes with published slot/extent limits, or the profile claim has been
  narrowed explicitly; native-allocation capabilities are named, tested, and
  cannot leak into common-profile output;
- the pass dependency graph declares consumed/produced artifacts and analysis
  invalidation for every enabled pass; injected stale cache results and
  downstream-only changes are rejected or reused exactly according to that
  graph, and clean and incremental builds produce equivalent validated
  artifacts;
- the COBOL module IR passes profile checks before rendering, and rendered text
  reparses to an equivalent structure;
- the semantic expectation manifest is independently derived before module-IR
  construction from verified typed IR/CFG/lifecycle, ABI/profile descriptors,
  and frozen names; it contains no module-IR node IDs or rendered ranges, and
  changing it cannot alter renderer output;
- the independent output parser and validation-correlation report establish
  complete, unambiguous expectation-to-IR/output coverage and source-map range
  agreement without feeding observed output back into semantic expectations;
- the renderer's profile-pinned encoding, line endings, continuation/wrapping,
  and source-map bytes are deterministic across hosts and fail closed rather
  than truncating or silently changing source format;
- the frozen name table is generated from canonical keys, profile-validates
  without collisions, and is byte-identical across request/order/cache
  permutations; ABI-frozen external spellings remain exact or fail explicitly;
- repeated translation with identical source/profile/configuration produces
  structurally and byte-for-byte deterministic artifacts, excluding explicitly
  normalized source-map paths;
- every compilation phase is bounded by documented deterministic budgets;
  serial/parallel and cold/warm-cache runs fail consistently at the same
  resource boundary, and cancellation never publishes partial success;
- profile compiler acceptance and execution tests pass for each promised
  runtime claim, including nested managed-container and exception paths; and
- no shipped C++ code supplies a public library operation or generated-program
  runtime behavior.

### 6.7 Module graph, separate compilation, and link planning

The backend must treat a program as a resolved graph of source modules and
versioned contracts, not as an unordered set of files whose generated COBOL
happens to share `CALL` names. Parsing a module, producing a COBOL artifact,
and proving that artifacts can be linked together are separate milestones.
The module graph owns source identity and dependency order; the interface
artifact owns exported source/ABI contracts; the link planner owns final
external-target resolution. None of these decisions belongs in the COBOL text
renderer.

#### Canonical module identity and graph construction

Give every source unit a canonical module ID independent of its absolute host
path. Import paths are lookup inputs resolved against declared source roots;
after resolution, the build records the canonical provider ID and a
normalized project-relative source identity. Recommended 1.0 rules are:
normalize separators to `/`; remove `.` components; resolve `..` only while
remaining inside the selected source root; strip the source extension; and
form the ID from the package/root ID plus the exact-case relative path. Module
IDs are case-sensitive on every host. A case-insensitive filesystem must
detect case-only collisions during graph discovery and report them, not make
the program's meaning host-dependent. Resolve internal symlink aliases to
their in-root target before assigning identity, reject root escapes, and
diagnose two imports that attempt to define the same canonical module twice.
Do not let current working directory, drive-letter spelling, directory
enumeration order, or import search order change an already-resolved module ID
or ABI symbol. Two different modules with the same short filename must never
alias merely because their basenames match.

Construct the dependency graph before emitting any target artifact:

1. Resolve every import to exactly one module under the configured roots.
   Missing and ambiguous imports are distinct diagnostics with both the
   importer span and searched-root context.
2. Reject duplicate canonical module IDs and duplicate source ownership before
   interface registration. Do not silently keep the first or last file.
3. Build explicit edges for source imports, generated template dependencies,
   standard-library/runtime modules, and external COBOL programs. Keep these
   edge kinds distinct in diagnostics and manifests.
4. Produce a deterministic dependency order using canonical IDs as the stable
   tie-breaker. A module is emitted only after all required interfaces and
   concrete type dependencies are available.
5. Initially reject import cycles with a complete cycle path. Reject dynamic
   module-initialization cycles even if a later type-only import cycle is
   permitted. A future type-only cycle feature must distinguish
   declaration/interface edges from executable initialization and value edges,
   and must specify initialization order before any cycle is accepted. Never
   break a cycle by making a partially registered symbol visible.

##### Program bootstrap, module-scope objects, and shutdown

The 1.0 source contract must decide module-scope initialization explicitly;
recommended behavior is to support constant-initialized scalar data and
non-trivial owning objects, while rejecting any initialization dependency
cycle before COBOL rendering. Do not rely on COBOL `VALUE`, `WORKING-STORAGE`
persistence, source-file order across program units, or an operating-system
loader to run CBL-C constructors/destructors.

The linker builds an initialization graph from the complete executable before
emitting entry-point code. Its nodes begin with a non-allocating root
exception/control record, followed by heap/runtime-service bootstrap, each
non-trivial module-scope object initializer, and each module-init barrier
needed to expose that module's initialized state. The root record is available
before any failing operation, including heap bootstrap; it must not depend on
managed allocation or a COBOL startup value the target profile does not
guarantee. It contains an inline, non-owning bootstrap error code and enough
fixed storage to represent the profile's heap-bootstrap failure without
allocating an exception payload; arbitrary owning exception payloads are not
constructed before the heap is ready. It also initializes the run-unit
activation-depth counter, declared activation limit, and activation-strategy
identity before the first generated call; these fixed control fields do not
allocate. A compiler-managed activation arena, if selected, is provisioned
only after the heap is ready and reports failure through the declared startup
path before `main`. Edges include: root record before every potentially failing
bootstrap action; heap service before any managed allocation; source
declaration order among dynamic initializers in one module; service readiness
before any initializer that uses that service; and exact global-access
dependencies discovered from each initializer's transitive effect summary.
An import or call edge alone does not order module initializers: cross-module
initialization edges come from declared global/service effects. Effects record
the canonical globals/services read, written, or constructed.
Local call/effect analysis supplies these facts transitively. An imported
function reached from a module initializer must publish complete effect
metadata. An unknown or missing initializer-callee effect is never presumed
side-effect-free and cannot be
accepted merely by ordering the provider first; the initializer is rejected
with a source-mapped `unsupported_dynamic_initialization` diagnostic.
Opaque external calls are disallowed from dynamic initializers unless an
explicit effect contract supplies a complete summary.

For every access to a global with dynamic initialization in a transitive
summary, add an edge from that global's successful initializer to the
accessing initializer. This applies to writes as well as reads because the
target object's construction must be complete before another initializer can
mutate it. The initializer's own construction of its destination is the node's
output, not a dependency edge; a read of that same global through its global
identity before completion is rejected, while constructor access through its
initializing `this` place is governed by ordinary object-lifetime rules.
Constant-initialized globals are established in the explicit pre-bootstrap
data phase; writes to immutable globals are rejected.

After deduplicating nodes by canonical module/symbol identity, the link planner
topologically sorts the graph by choosing the smallest canonical module/global
ID among currently ready nodes. Multiple valid orders therefore produce the
same order independent of discovery path or worker scheduling; declaration
order is preserved within a module. A cycle diagnostic prints the complete global/module
dependency path and the initializer access/call origin for each edge. Import
cycles and value-initialization cycles remain distinct diagnostic classes.
The finalized graph, node effects, chosen order, and initializer ABI entries
are recorded in the link manifest and contribute to the link-plan digest.
No coarse provider-before-consumer initialization order is added. This keeps
runtime initialization order tied to actual source-visible dependencies and
avoids cycles caused only by unrelated imports.

The generated entry bootstrap runs exactly once per linked run unit, before
`main` or any exported application entry that can observe module globals:

1. Explicitly initialize the statically allocated root exception/control
   fields, fatal-state marker, activation-depth counter/limit, and selected
   activation-strategy ID; this straight-line, no-throw step must not allocate
   or invoke user code or depend on implicit COBOL initial values.
2. Initialize the shared managed-heap domain. If it fails, record its typed
   bootstrap failure in the inline root slot without allocating.
3. Initialize required runtime services in their frozen dependency order,
   recording completion only after each service succeeds.
4. Execute initializer nodes in the frozen link order, recording each object's
   construction-completion state only after its constructor succeeds.
5. Mark a module `READY` only after all of its dynamic module-scope objects
   have completed; a module may never be observed as partially ready.
6. Enter the source entry point only after the whole initialization graph is
   ready.

On initialization failure, do not enter `main`. Preserve the original typed
exception, destroy only successfully completed global objects in exact reverse
completion order while retaining the original exception payload. Report the
uncaught exception before finalizing any runtime service needed by the
reporter; reporting must be no-throw, must not allocate from the managed heap
or invoke user code, and may inspect the payload only while its owner is live.
Native output failure is recorded as secondary termination evidence and cannot
replace the original exception or change its profile termination mapping.
Then destroy the exception payload through its verified type-directed
lifecycle entry, finalize only successfully initialized runtime services in
reverse order, check/release the heap domain last, and follow the profile's
uncaught-startup-exception termination mapping. Heap bootstrap is the first
potentially failing service initialization after the root control record; no
other runtime service or object initializer runs before heap readiness. If
heap bootstrap itself fails, there is therefore no initialized runtime service
or object to unwind and no managed payload to destroy; the inline bootstrap
error remains available to the profile's non-allocating termination mapping.
Destructors/finalizers remain non-throwing; a cleanup failure during startup
unwind follows the fatal second-exception policy and cannot replace the
initialization exception. No failed module is retried and no module-init entry
is executed twice. Ordinary internal calls made during an initializer may
call functions in the same initializing module, but may not recursively
invoke module initialization or observe a dependency whose state is not
`READY`.

After normal `main` completion, capture its normalized success/failure result
and verify the run-unit activation depth returned to zero, destroy all
successfully initialized globals in reverse construction order, run
runtime-service finalizers in reverse initialization order, perform the heap
leak check in diagnostic builds, and release the heap domain last. The
captured entry result is then mapped to the profile's process-completion
operation. If an uncaught source exception leaves `main`, preserve it while
unwinding module-scope objects. Before finalizing runtime services, report the
exception with the same non-throwing, non-allocating reporter while its payload
and required reporting services are live; native reporting failure cannot
replace the primary exception. Then destroy the exception payload through
type-directed lifecycle dispatch. Finalize services in reverse order,
check/release the heap, and terminate with the profile's uncaught-exception
mapping. Fatal double-exception termination still skips all remaining cleanup
as required elsewhere. A program that cannot prove the required initializer
effect closure, once-only bootstrap, exception-report/payload lifetime,
unwind order, and heap-service lifetime is rejected by the selected profile.

#### Uncaught-exception reporting ABI

Generate uncaught reporting as a private COBOL paragraph in the executable's
startup/termination program unit, not as a C++ runtime helper or an implicit
call to a user-defined `what()`/formatting method. Its input is a read-only
borrow of the live root exception record: canonical throwable identity,
source-origin record when available, and the profile-defined terminal category.
Invoke it exactly once only when a source exception escapes startup or the
source entry point after ordinary cleanup. Normal completion and exceptions
handled within CBL-C produce no uncaught report. A fatal double exception
bypasses this reporter and all remaining cleanup, then takes the profile's
fatal whole-run-unit termination path directly; attempting to format or
report the active exception during `FATAL` is forbidden.
The baseline report includes the stable exception identity and source origin;
it does not inspect or stringify arbitrary payload fields. Optional payload
rendering requires a separate versioned, type-directed reporter descriptor
whose implementation is proven no-throw, non-allocating, and free of user
callbacks; absent that descriptor, payload values are omitted. The report
paragraph uses only the selected profile's declared native COBOL output
constructs and frozen literals/fields, with no generated C++/OS call or
standard-library formatting dependency.

The reporter cannot mutate, clear, detach, or retain the exception frame or
payload. Its generated call graph is closed and statically verified before
rendering; every invoked paragraph/entry must be marked no-throw and
non-allocating, and cannot transitively call user code or lifecycle operations.
The report paragraph returns only the profile-declared output disposition (or
has no status if that COBOL construct cannot expose one). Any observable
output failure is secondary evidence and never replaces the primary exception,
changes the fatal/uncaught termination mapping, or suppresses payload
destruction and heap teardown. The reporter ABI/profile digest and paragraph
identity are bound into module IR, the generated-COBOL expectation manifest,
and the artifact manifest. A profile unable to provide the required native
output/termination behavior must declare that capability unavailable rather
than silently calling a host helper.

The current context integration already tracks import providers through
`resolved_index`, imports public type/data/function stubs, validates selected
exception metadata, merges method/constructor/destructor signatures, and
registers translation-unit exports. It is a useful seam, but it is not yet a
serialized build graph: in-memory registration, source-path lookup, and
signature export do not by themselves prove deterministic separate
compilation or link compatibility.

#### Versioned interface and artifact contract

Define a deterministic interface artifact (proposed extension `.cbi`) for every
compiled module. It is a compiler artifact, not a new source language. Use a
versioned UTF-8 canonical serialization with specified field/key ordering,
escaping, integer encoding, and duplicate-field rejection. Hash the canonical
payload with SHA-256, excluding the hash field itself. Validate the complete
artifact on read before registering any of its symbols. At minimum it
contains:

- format version, compiler semantic version, target profile ID, and source
  module ID;
- canonical source-snapshot identity, exact source-content hash, and source
  byte length, plus the public-interface hash; keep these separate from the
  implementation hash so private implementation edits do not masquerade as
  public ABI changes. Declaration spans are half-open byte ranges with their
  declared coordinate convention, bound to this source identity/hash; they
  are provenance references, not source text embedded in the interface;
- each exported symbol's canonical ID, source name, kind, visibility, overload
  identity, and declaration span/source-map reference;
- structural type references and template parameter structure, never just
  lossy display-name strings;
- function, method, constructor, destructor, and data signatures, including
  receiver presence/type/access/base projection/lifecycle state, explicit
  argument order/modes, result kind/layout, ownership/borrow mode, constness,
  exception effect/set, managed-handle ABI version/domain requirements, and
  required runtime dependencies;
- exported throwable-type records containing the canonical source type key,
  versioned runtime `exception_type_id`, exception ABI and identity-schema
  versions, payload-descriptor identity, lifecycle-operation owner/provider
  references, and ordered direct-base identity/access/projection/path edges.
  Function throw sets reference these full type records; `unknown` remains
  distinct from a complete empty set. These records are validated and
  registered atomically with the rest of the interface;
- external dependency records containing the canonical external declaration
  ID and frozen COBOL program spelling; the logical signature and exact
  physical ABI projection; one required call mode (`non_throwing`,
  `status_to_exception`, or `exception_aware`); separate application-status
  success/failure values and native call-availability policy; a complete
  status-to-canonical-exception/payload mapping including unknown-status
  behavior; per-status validity guarantees for every `out`/`inout` result;
  declared ownership transfers and effects; and, for exception-aware calls,
  exact exception ABI/schema versions, context slot, throwable type/payload
  descriptors, and lifecycle owners. Every callback record includes canonical
  callback symbol/signature and ABI version, frozen entry spelling, result and
  status/payload slots, synchronous/asynchronous mode, retention lifetime,
  invocation cardinality/order relative to other callbacks and external
  `out`/`inout` mutations, maximum simultaneous re-entry depth, callback
  argument alias/ownership modes, permitted task context, and exact
  exception-to-callback-result translation plus failure-continuation policy.
  Also record the
  contract basis (`compiler_verified_provider`, `trusted_external_assertion`,
  or `profile_guarantee`), canonical authority/contract ID and version, exact
  contract-artifact digest, verification or attestation reference where
  applicable, and the trust policy under which the record may be used. A
  missing, contradictory, or unknown required field makes the dependency unusable;
  external-call records are validated atomically with imports and are bound
  into source-interface/ABI/effect hashes according to whether each field is
  a source contract, physical layout, or implementation effect;
- canonical target-layout descriptors and fingerprints for exported records,
  classes, exception payloads, and concrete specializations;
- interface and ABI versions, compatibility policy, and any explicit adapter
  identity; and
- template definition identity plus a versioned body/typed-IR reference when
  import-time instantiation is supported. A signature-only stub cannot stand
  in for a missing template body.

Separate the public source-interface hash, target-specific ABI fingerprint,
template-body hash, and private implementation hash. Formatting, timestamps,
host paths, and discovery order must not affect those hashes. An ABI-affecting
change includes field order/type, `PIC`/`USAGE`/sign/scale, argument mode/order,
result/context slots, ownership transfer, exception payload representation,
generated entry identity, and target profile. A private body-only change may
invalidate dependent execution artifacts without changing the public ABI hash.
Version mismatch is an explicit incompatibility diagnostic; never guess a
layout from the current compiler's host structures or silently regenerate an
imported provider under a different profile.

##### Interface artifact canonicalization and atomic ingestion

Use a canonical JSON-based `.cbi` envelope for the initial interface format,
with an explicit schema ID and major/minor version. Keep this format separate
from the user-facing manifest and do not deserialize it into C++ compiler
structures by copying bytes. The writer uses UTF-8 without a BOM, no
whitespace outside strings and no trailing newline, fixed field order for every
record, and arrays sorted by canonical identifier bytes. Avoid
unordered JSON objects for semantic maps: encode them as sorted arrays of
`{key,value}` records so duplicate keys can be detected before registration.
Encode IDs, offsets, sizes, and arbitrary-precision bounds as canonical
decimal strings (no leading zeroes except `"0"`); encode booleans/enums with
fixed schema values. Preserve source/display strings as their exact Unicode
scalar sequence; do not apply host-locale case folding or normalization while
serializing. COBOL/profile names have their separately specified
canonicalization rules.

The envelope has a fixed ordered shape:

1. `artifact_kind` (`CBLC-CBI`), `schema_version`, compiler
   semantic-version range, selected target-profile ID/version, canonical
   provider module ID, and canonical source-snapshot ID with exact source
   SHA-256 and byte length;
2. the canonical `payload` containing exports, structural types, signatures,
   target-specific projections, template-body references, and dependency IDs;
3. a distinct canonical `link_effects` section for link-time initialization
   analysis. This is private link metadata, not a source-level export or
   callable ABI contract. For each exported callable and every callable
   reachable from a module initializer, it records the canonical symbol ID,
   visibility/entry kind, effect-summary schema version, summary state
   (`complete` or `unknown`),
   transitive global read/write/construction sets, runtime-service effects,
   allocation/deallocation effects, failure/exception state, and transitive
   callee IDs. For exported callables, failure and exception facts must agree
   with the declared public effect/throw contract, which remains part of the
   source-interface/ABI hashes; private body-derived global/service effects
   remain in `link_effects_hash`. For each module-scope object it records the
   canonical global
   ID, constant-versus-dynamic initialization, and (when dynamic) the exact
   initializer entry plus a reference to its callable effect-summary record,
   and the selected no-throw destructor entry for managed objects. A
   `complete` summary must be
   conservative and closed over all direct and transitive callees; `unknown`
   must never be encoded as empty effect sets. Every summary is bound to the
   provider implementation-closure digest and source snapshot it describes.
   The implementation-closure digest covers frozen source and declared
   compilation inputs only; it excludes the `.cbi` artifact, generated COBOL,
   and link manifest to avoid a recursive hash dependency.
   External providers must supply a versioned effect contract with equivalent
   completeness; otherwise an initializer that reaches them is rejected.
   Destructor entries must match the type lifecycle descriptor and be
   non-throwing; a missing or throwing global destructor contract rejects that
   owning global before bootstrap is emitted;
4. domain-separated `source_interface_hash`, `abi_fingerprint` (per
   profile/physical ABI version), `template_body_hash` where applicable, and
   `link_effects_hash`. `source_interface_hash` includes canonical throwable
   source identities, public hierarchy/access edges, and public throw effects;
   `abi_fingerprint` includes runtime exception-ID encoding/version, payload
   layouts, lifecycle linkage, and context ABI projection. The first three cover only their declared source/API,
   physical ABI, and public template inputs; `link_effects_hash` covers the
   canonical effect records and exact implementation closure. Private effect
   changes therefore invalidate link planning without changing source or ABI
   hashes unless an exported contract changed. The generated COBOL artifact
   hash is not part of `.cbi`: it is not known at Pass 8 and is bound later by
   artifact and link manifests after rendering; and
5. an integrity descriptor naming the digest algorithm and the SHA-256 digest
   over canonical artifact content with the integrity field omitted.

Each semantic hash has a fixed, documented domain prefix and only its
declared canonical fields as input; hash field values are never included in
their own preimage. A private implementation edit changes the provider
implementation/artifact identity but does not alter the public source-interface
hash or ABI fingerprint unless an exported contract changed. A profile projection or
ABI-frozen external-name change affects the ABI fingerprint and artifact
integrity hash, not a profile-neutral source-interface hash. A public template
body/reference change updates its template-body/public-interface dependency
hashes according to the cache contract. A SHA-256 integrity digest detects
content corruption; it is not publisher authentication. Remote/distributed
artifact authenticity, if required, belongs in a separately versioned signed
package/attestation contract rather than being inferred from a plain hash.

Semantic consumer reuse is keyed by the normalized source-interface and
relevant ABI/template hashes; link planning separately keys initialization
closure by `link_effects_hash`. The enclosing artifact-integrity digest pins
the combined `.cbi` bytes for transport, but a change to only link-effect
metadata does not force source re-resolution or consumer code generation.

The reader validates into a temporary, unregistered interface graph in this
order:

1. Enforce configured byte, nesting, string, array-count, and record-count
   budgets before allocating proportional memory. Reject invalid UTF-8/BOM,
   duplicate object keys, duplicate IDs, duplicate sorted-array keys,
   non-canonical integers, unknown required schema fields/enums, and malformed
   digests. A supported schema version may preserve namespaced optional
   extension records, but must reject unknown required extensions and never
   silently ignore a field that can affect ABI, visibility, ownership,
   exception effects, profile, or dependency closure.
2. Re-serialize the parsed semantic artifact canonically and require bytewise
   equality with the input, then recompute and compare its integrity digest and
   each independently defined semantic hash. This catches alternate key
   order, escapes, whitespace, duplicate-field parser behavior, and forged
   embedded hashes before symbols become visible.
3. Validate artifact-kind/schema support, compiler semantic compatibility,
   provider module identity, selected profile/version, ABI and descriptor
   versions, all structural type references, declaration-span bounds against
   the declared source byte length, and template-body/runtime dependency
   content hashes. Validate every `link_effects` record against its canonical
   symbol/global/callee IDs, summary schema/state, exported failure/exception
   facts against the public declaration, implementation-closure digest, and
   `link_effects_hash`; reject incomplete summaries wherever a
   dynamic initializer depends on them. The provider writer must independently
   compute and verify each `complete` summary against its finalized typed call
   graph and exact source/implementation inputs. It must also verify every span and
   coordinate against the exact frozen source bytes whose hash is recorded. A
   consumer with those exact source bytes available rechecks the hash and
   coordinates; a consumer without them validates only the canonical source
   identity/hash/length and span bounds, retains the span as an unverified
   location claim, and must not show source text or claim a source-mapped
   diagnostic until it obtains bytes matching that hash. A profile mismatch
   or unsupported compatibility range is an explicit diagnostic, not a
   request to re-lower or reinterpret the provider.
4. Check uniqueness and internal consistency across the complete export set:
   canonical symbol IDs, overload identities, frozen entry names, type/layout
   IDs, lifecycle entries, exception payloads, and all required dependencies.
   Compare cross-references field-by-field; digest equality is not a substitute
   for descriptor validation.
5. Only after all checks succeed, atomically publish the temporary export/type
   records to the consumer's symbol/interface graph and the validated
   `link_effects` records to the link-planning graph. Any failure discards the
   entire temporary graph; no prefix of exports or effect records is
   registered and no dependent module is compiled against a partial interface.

The writer performs the inverse transaction: build a complete immutable
payload from the finalized export graph and Pass 8 name/ABI tables, plus
implementation-bound `link_effects` from verified call/effect closure; sort
every semantic array; canonical-serialize; compute domain-separated hashes; create
the integrity descriptor; parse and validate its own output with the strict
reader in a fresh context; write to an isolated temporary artifact; then
publish only through the build's atomic artifact-commit mechanism. A failed
serialization/read-back/publish leaves the previous committed interface
untouched. The strict reader and writer may share schema constants and
primitive codecs, but conformance tests use an independent reference
encoder/decoder so a shared ordering or escaping bug cannot certify itself.
Publishing a validated `.cbi` makes the provider's source/ABI contract
available to semantic consumers and its verified `link_effects` available to
link planning; it does not assert that provider COBOL has been rendered,
compiled, or linked. The generated-output digest is established by the
artifact manifest after rendering and verified against the selected provider
output at link planning.

Keep intermediate interface artifacts, generated COBOL modules, source maps,
and expectation manifests together under a build directory isolated by
canonical source identity and target profile. Write artifacts to temporary
files and publish them only after parse, semantic, ABI, and target validation
all succeed; a failed build must not leave a valid-looking partial interface
or stale COBOL file. The manifest records dependencies and artifact hashes so
the linker and verifier can prove which exact inputs produced each output.
Under the recommended 1.0 contract, support dynamic module-scope class values
through one module-init entry per module with explicit dependency records;
initialize each object once in graph order and finalize in reverse completion
order. Until the bootstrap/effect-closure gates pass, reject dynamic
module-scope initializers rather than relying on COBOL working-storage
initialization.

#### Separate compilation and external-call resolution

Separate compilation has three explicit products: a source interface, one or
more generated COBOL program units, and a link manifest. The compiler may
compile a provider before a consumer only after the provider interface is
complete; the consumer resolves against that immutable interface and records
the exact provider symbol ID and ABI descriptor. At final link planning:

- every external `CALL` target resolves to exactly one generated module or an
  explicitly declared runtime/external entry;
- duplicate program IDs, duplicate exported symbol definitions, unresolved
  calls, incompatible interface/ABI/profile versions, and ambiguous aliases
  fail before COBOL compilation or final link;
- provider declarations and consumer call descriptors match field-for-field,
  including argument count/order/physical mode, result slot presence, optional
  exception context, payload layout, ownership semantics, and target profile;
- the executable's transitive module/runtime dependency closure is complete;
  unused modules are not linked implicitly by directory scan; and
- all module heap-pool requests are merged into exactly one compatible
  executable heap-domain descriptor using Section 6.6's normalized `pool_key`,
  provenance, and aggregation rules: distinct static reservations use checked
  sums, the largest required contiguous extent is preserved, and dynamic
  capacity comes from one executable-wide policy. Duplicate canonical type
  IDs with different layout/lifecycle contracts, incompatible handle/domain
  versions, aggregation overflow, or resource ceilings outside profile limits
  fail before rendering; the merge is invariant under module/link order and is
  covered by `BACKEND-HEAP-MERGE-001`;
- link order and generated `PROGRAM-ID` allocation are deterministic and
  independent of parallel compilation completion order.

##### Canonical per-executable link manifest

The link manifest is the complete, immutable input contract for producing one
final executable; it is not a list of filenames for the linker to search. Use
one canonical link manifest per executable root. A multi-root build may publish
an umbrella record containing the sorted set of those manifests, but it must
not merge their heap/runtime domains or let one root pull another root's
private modules into its closure.

Each per-root manifest contains at least:

| Record | Required contents |
| --- | --- |
| Header | Schema/version; canonical root module and entry symbol IDs; target-profile ID/version and pinned physical ABI version; runtime-catalog identity/version; link-planner algorithm version; and domain-separated link-plan digest. |
| Module artifact | Canonical provider module ID; source-interface hash; ABI fingerprint/descriptor identity; profile and ABI versions; exact generated COBOL artifact IDs, frozen `PROGRAM-ID` values and content hashes; required provider dependencies; and artifact class. Paths are resolved separately under the isolated staging root and are not semantic identities. |
| Resolved callable | Consumer module/symbol/operation IDs and provenance; resolved generated-provider or catalogued runtime/external declaration ID; exact ABI descriptor digest and frozen `CALL` spelling; and the call kind/protocol. Every call in the reachable typed call graph is represented exactly once. |
| Dependency edge | Canonical from/to identities and edge kind (`source import`, `call`, `template body`, `type/layout`, `lifecycle`, `module initialization`, or `runtime package`). Preserve edge kinds and origins; do not collapse them into an untyped provider list. |
| Link-unit order | Exact ordered sequence of generated COBOL program-unit artifact IDs and catalogued runtime/library units passed to the linker, plus the dependency constraints and deterministic tie-break rule that produced it. It is derived from the verified closure, not directory enumeration; any profile-specific ordering rule is declared and versioned. |
| Entry and initialization | Exactly one executable entry symbol for this root; exact transitive module-init order and dependency-edge references; normal/failure shutdown entries; the root exception-control and uncaught-reporter paragraph identities with reporter ABI/schema and profile output-policy digests; and the selected process/runtime/termination mappings. |
| Heap/runtime domain | The single merged heap-domain descriptor and digest for this executable, every contributing module pool request with provenance, and the exact runtime-service closure/version set. No other root's domain descriptor is admissible. |
| Exception dispatch domain | The exact reachable throwable-type closure; canonical source identities and runtime IDs; ABI/identity-schema versions; payload descriptor and lifecycle-owner references; validated direct-base edges/projections; and the deterministic type-to-handler matching table/digest used by generated dispatch. No consumer may add a hierarchy edge or select a private type record outside the root closure. |

Derive the exception dispatch domain from the root's reachable typed call graph
and validated interfaces: include statically declared call throw sets, every
exception named by an external adapter's recognized/unknown status and
call-availability mappings, every declared exception-aware external throw
set, every declared reverse-callback throw/translation/consumption set, source
catch targets required for matching, and transitive base,
payload-layout, and lifecycle-operation providers. The dependency graph must
retain provenance from each included type/provider back to the call or catch
that requires it. An external COBOL dependency may contribute exception types
without contributing a generated COBOL module artifact; it still cannot
contribute an unowned or unresolved type. Compute the closure per root, not
from the global project catalog.

Dependency edges also carry an explicit link disposition: `required_artifact`,
`interface_only`, or `proven_non_emitting`. The root closure follows only
`required_artifact` edges plus the exact emitted callable/runtime/init targets;
a source import, type/layout dependency, or template-body availability record
does not by itself force the provider's COBOL program unit into the executable.
Concrete generated calls, lifecycle entries, dynamic module initialization,
and catalogued runtime services do require their implementing artifacts. Every
non-followed edge retains the proof/reason that it is interface-only or
non-emitting, so omitted code cannot be hidden as an unused directory file.

Canonical arrays are sorted by their specified canonical IDs, while any
semantically required provider-before-consumer or initializer order is stored
as an explicit ordered sequence with its dependency proof. The manifest
contains no current-directory value, absolute checkout path, directory-scan
result, worker order, timestamp, or unpinned runtime resolution. The link-plan
digest is computed over canonical serialization with its own digest field
omitted. It binds generated module hashes, resolved call targets, ABI/profile
contracts, runtime catalog, root identity, initialization plan, exact link-unit
order, merged heap domain, exception dispatch domain, and the root's
exception-control/reporter/termination tuple; it excludes the final
artifact-manifest digest, attempt evidence, and commit record to avoid a
recursive hash dependency. The artifact manifest binds this verified
link-plan digest after all generated module hashes are known.

An independent link-manifest verifier recomputes the reachable closure from
the declared root and typed call/dependency records, validates each provider
artifact/interface/ABI/profile tuple, checks uniqueness and exact call-edge
coverage, and recomputes runtime, initializer, heap-domain, and exception-type
closure/hierarchy/table aggregation.
The linker adapter receives only the verified manifest's exact generated
artifact set in its `Link-unit order` sequence and ordered invocation inputs;
it may not add files by wildcard,
basename, or search-directory discovery. Missing, extra, duplicate, stale,
or cross-root artifacts fail before launching the COBOL linker. For multiple
roots, shared immutable provider artifacts may be referenced by each
manifest, but each root's entry, closure, heap domain, runtime instance, and
link-plan digest remain independently verifiable.

##### External entry identity and ABI-frozen COBOL names

Keep four identities distinct throughout compilation and linking:

1. the canonical source module ID, which identifies the provider independent
   of its checkout path;
2. the canonical source symbol ID, which identifies one exported function,
   method entry, runtime entry, or other callable declaration;
3. the generated COBOL entry identity, which names the physical program that
   implements that callable ABI; and
4. the COBOL spelling used in `PROGRAM-ID` and literal `CALL`, which is an
   ABI-visible, profile-specific value rather than a semantic lookup key.

The provider owns assignment of its generated external entry spelling. In
Pass 8, allocate it deterministically from the canonical module and symbol
IDs, entry kind, ABI version, and target-profile naming policy. If the source
or an explicit interop contract pins an external spelling, record that exact
spelling as ABI-frozen and validate it against the profile. Serialize the
provider's `.cbi` interface only after Pass 8 has frozen all exported names
and sealed the descriptor fingerprint; no provisional name may escape for
consumers to compile against. Include the resulting spelling and the
name-policy/profile version in the exported ABI descriptor, target-specific
ABI fingerprint, serialized interface-artifact integrity hash, and provider
artifact manifest. Do not include this profile-specific spelling in the
profile-neutral `source_interface_hash`; a profile-only spelling change must
not invalidate source-level semantic resolution. If source explicitly pins
an external name, hash that declaration as part of the public source contract,
while the resolved spelling remains part of the target-specific ABI
fingerprint. Any policy change that alters the generated spelling is an ABI
change requiring an explicit ABI-version transition; it must not be hidden as
a formatting-only compiler update.

An importer resolves a source-level reference against the serialized public
interface and stores the provider's canonical symbol ID and ABI fingerprint
in its typed call descriptor. At COBOL lowering, it obtains the physical
`CALL` spelling only from the matching imported interface/name table. Never
sanitize the import's source spelling independently, choose a provider by
basename, or use a string-keyed fallback when canonical resolution fails.
Before rendering, verify that the imported interface/name-table entry's
provider module/symbol IDs, source-interface hash, profile ID, external
spelling, ABI version, layout fingerprint, and name-policy version agree.
Do not require or infer a generated-COBOL artifact hash at this stage. At link
planning, verify that the selected provider artifact's exact hash and ABI
descriptor match the final artifact manifest and the consumer's resolved
interface. A stale or absent interface/name mapping is an ABI diagnostic; a
missing or mismatched generated artifact is a `link_artifact_failure` with a
link-plan diagnostic. Neither
case permits guessing a program name or provider.

The link planner consumes symbol IDs and ABI descriptors first, then checks
their frozen COBOL entry spellings. Resolve each call to exactly one provider
definition or one catalogued runtime/external declaration; ensure a provider
artifact is selected only once even when multiple import paths reach it;
check duplicate definitions, duplicate `PROGRAM-ID`s, unresolved or ambiguous
entries, ABI/profile/version mismatch, and declared runtime dependency closure.
Report both consumer call/import origin and provider export origin. Sort the
link closure and diagnostics by canonical IDs, not by generated COBOL spelling
or directory enumeration. A collision between two different ABI-frozen names
in a profile's link-global namespace is fatal unless the language's explicit
interop contract declares a same-target alias with identical ABI; the linker
must not silently append suffixes, redirect a call, or rewrite an already
compiled provider. Internal data and paragraph names remain governed by their
program-local name-table domains and do not cause unrelated providers to be
recompiled.

The build cache keys each name map by canonical module ID, target profile and
name-policy version, compiler semantic/backend version, and ABI version. The
link plan additionally keys the ordered provider artifact hashes and runtime
catalog version. Recompiling a private function body may change the provider
artifact hash while leaving exported ABI names/fingerprints unchanged;
consumers can remain semantically reusable but must be relinked against the
new provider artifact. Changing an exported signature, ABI-frozen name, target
profile, or name policy invalidates the interface and all affected consumers.

The ABI descriptor in Section 6.6 is the single source of call shape. Imported
effects that are absent from an older interface must be conservative: a call
with unknown exception behavior is treated as potentially throwing, and an
unknown ownership mode is rejected rather than inferred. If a compatibility
adapter is permitted, the interface names and versions that adapter
explicitly; the linker must not apply an adapter based only on a matching
spelling.

##### Build coordinator transaction and deterministic scheduling

Implement one build coordinator above module compilation, artifact storage,
and link planning. The coordinator owns a deterministic `generation_key`, an
ephemeral `attempt_id`, the immutable input snapshot, dependency graph, staged
output set, cancellation token, and final commit record; individual module
workers may not publish directly into the current successful build. Derive the
generation key from canonical project/source identities, selected
roots/entry point, CBL-C compiler semantic/backend versions, target-profile
contract, artifact-affecting options, and the content hashes of all declared
artifact-producing inputs. A COBOL compiler used only to validate generated
source is an evidence tool, not an artifact-producing input. Validation-plan,
test-run, and validation-tool identities belong to attempt evidence unless a
compiled executable is explicitly declared as a deliverable artifact. Do not
put wall-clock time, worker count, temporary directories, or host absolute
paths in semantic identity.

Distinguish the deterministic `generation_key` (content/configuration and
artifact-producing toolchain identity) from an ephemeral `attempt_id` used
only for private staging and lock ownership. Retries of identical
artifact-producing inputs share a generation key but never reuse another
attempt's unverified temporary
directory. Concurrent attempts
for different snapshots cannot overwrite each other's staging data.

##### Source snapshot capture and input closure

Define two disjoint input closures before parsing. The **artifact input
closure** includes selected roots and entry points, imported source and
template bodies, copybooks, explicit configuration, the target-profile
contract, embedded standard-library catalog/source version, and the CBL-C
compiler/backend identity that produces the declared artifacts, plus the
artifact-producing pass-registry projection and its digest. The
**validation evidence-input closure** includes the full pass-registry digest,
validation plan, harness, test/reference data, and exact external COBOL
compiler/linker/runtime package IDs, versions, invocation options, executable
digests, or trusted package-manifest digests. The artifact projection is bound
to generated identity; the full registry is bound to attempt evidence. A
machine-local resolved executable path is bounded evidence, not semantic
identity. Where a tool binary can be replaced in place, pin and hash it or
verify its cryptographically identifying package manifest. Resolve artifact
inputs and evidence inputs to their own canonical identities and reject
ambiguous aliases or forbidden case/separator collisions under the selected
profile. Directory enumeration is
sorted by canonical identity. Build outputs, cache directories, and temporary
files are excluded unless explicitly declared as inputs; implicit discovery
outside declared roots is an error. The child environment starts from an empty
or profile-defined allowlist; every inherited value that may affect generated
output is declared and included in the artifact-input or evidence-input
identity according to which operation consumes it. Do not
follow symlinks/reparse points during discovery or open unless a declared input
policy explicitly permits and pins the resolved target within an allowed root.

Apply the following capture protocol independently to the artifact-input and
evidence-input closures. Each closure has its own sorted membership, frozen
bytes/verified tool identities, retry count, and manifest digest; never merge
the records into one snapshot. Only the artifact-input digest feeds the
`generation_key`; the evidence-input digest is bound to the attempt commit and
validation evidence.

Capture each closure into the attempt's private immutable snapshot before
semantic analysis or validation:

1. Enumerate the complete declared input set and record canonical identities.
   For each file, open with the selected no-follow/root-containment policy,
   copy the exact byte stream to attempt staging while hashing it, and record
   length plus available file identity/change metadata before and after the
   read. Metadata is a race detector, not a substitute for content hashes.
2. Re-enumerate the declared roots and re-read/re-hash each declared file
   input. If membership, canonical identity, file identity, length, or content
   digest differs from the staged pass, discard the whole snapshot and retry
   from enumeration. Additions, deletions, renames, and edits during capture
   invalidate the attempt; never keep only the apparently unaffected files.
3. Bound capture retries and bytes/files scanned using the deterministic
   resource-budget policy. Exhaustion reports `stale_build_inputs` with the
   changed input set and attempt identity. No partial snapshot may enter the
   semantic or artifact cache.
4. Serialize artifact-input records and hashes into the canonical artifact
   source-snapshot manifest, verify its digest independently, then make the
   staged snapshot read-only/immutable through the filesystem adapter. Compiler
   passes and workers consume only these staged artifact inputs. External tools
   consume staged source/test inputs and the exact pinned tool identity recorded
   in the evidence-input manifest; they never reopen live source or test-data
   paths. Bind the artifact snapshot digest into the `generation_key`, artifact
   manifest, and current-generation reference.

The contract is an exact, revalidated set of file bytes, not a claim that a
mutable multi-file worktree was captured through an OS-wide atomic snapshot.
An editor may change files after snapshot completion; the build remains bound
to its captured inputs, and current-worktree consumers must revalidate the
snapshot digest before reuse. A build requiring repository-transaction
consistency must pin an immutable revision/snapshot provider as an explicit
input rather than infer one from timestamps or directory state.

Capture the validation plan, full pass-registry digest, output-parser and
correlation schemas/policies, harness, test data, reference-model inputs, and
external validation-tool identities as a separate attempt evidence-input
manifest using the same bounded enumeration, hashing, and immutable-staging
rules. Its digest belongs in the attempt commit record and validation evidence,
not in the deterministic artifact `generation_key` or artifact-manifest digest
unless one of those inputs actually participates in generating a declared
artifact. The artifact-producing registry projection is captured with artifact
inputs and contributes through `artifact_pass_registry_digest`; the full
registry/evidence digest never substitutes for that projection. A test-plan,
validation-only-registry, data, or tool-only change can reuse matching
generated artifacts but must rerun affected validation gates; it cannot reuse
old evidence for the changed inputs.

Use this transaction sequence:

 1. Capture the exact artifact-input roots, configuration, profile contract,
    CBL-C compiler/backend identity, runtime catalog, and selected program
    roots using the artifact snapshot protocol above. Capture the validation
    plan, full pass-registry digest, harness/data, parser/correlation schema,
    and external validation-tool identities separately in the evidence-input
    snapshot. Reject changes to either closure during its
    capture and restart that snapshot from enumeration. The artifact build is
    bound to its complete, revalidated bytes; it does not claim an OS-wide
    atomic revision across a mutable worktree.
2. Resolve canonical module IDs and imports, construct the typed dependency
   graph, calculate reachable modules from the requested program roots, and
   reject missing/ambiguous providers, disallowed cycles, duplicate module
   identities, and untrusted/stale imported artifacts before starting backend
   workers.
3. Allocate an isolated, unique staging namespace for this `attempt_id`, bound
   to the deterministic `generation_key`. Read prior cache entries only
   through their versioned envelope and digest checks. A worker receives
   immutable source/interface inputs and a cancellation token; it writes only
   attempt-local temporary outputs.
4. Schedule modules whose required provider interfaces are committed and
   immutable. Independent ready modules may compile concurrently. A consumer
   becomes runnable only after the provider has completed semantic/interface
   validation, Pass 8 has frozen exported names and ABI descriptors, and its
   `.cbi` has passed strict writer read-back. Worker completion order must not
   affect graph order, names, diagnostic ordering, selected artifacts, or
   output bytes.
5. For each worker result, validate the entire declared output set against
   the generation manifest: expected module/profile/schema IDs, source and
   dependency digests, ABI/interface compatibility, path containment, and
   artifact integrity. A successful exit code alone does not make an artifact
   reusable. Promote validated module outputs only within this attempt's
   staging area. They may enter a content-addressed pass cache only after that
   cache envelope is independently validated; workers never publish into or
   replace the current successful generation.
6. When all reachable modules are complete, derive the final link closure
   from canonical call/dependency IDs, merge runtime and heap-domain
   requirements, validate exactly one entry point per executable root, and
   produce and independently verify the canonical per-root link manifest;
   pass `BACKEND-LINK-ORDER-001`, `BACKEND-LINK-MANIFEST-VERIFY-001`, and
   `BACKEND-LINK-ROOT-001` before linker invocation. Reject unreferenced
   implicit
   directory inputs; explicit libraries/runtime packages enter through
   declared catalog identities and versions.
7. Compile and link only the closure named by that manifest using the exact
   tool pinned in the evidence-input snapshot. Capture bounded tool output,
   exit status, compiler identity, and evidence digests. Verify every expected
   validation output exists, is a regular file under the isolated validation
   root, and matches its run manifest before execution. Such temporary
   compiler-produced files are not generated-source artifacts; if a compiled
   executable is explicitly declared as a deliverable, its exact toolchain
   identity and output digest become inputs/artifacts in a separately named
   artifact class and key.
 8. Run required build-time validation/execution gates in a separate
    attempt-local sandbox bound to the generation key. The harness reads the
    frozen attempt evidence-input snapshot, never live test-plan/data paths;
    test data and generated executables cannot mutate source inputs, shared
    cache entries, or another concurrent build. Evidence outputs are captured
    separately and bound to the exact evidence-input-manifest digest.
 9. Write and independently verify the deterministic artifact manifest,
    binding the artifact source snapshot, module graph, interfaces, generated
    COBOL, name/ABI maps, runtime closure, link plan, and artifact-producing
    compiler/backend identity. Keep
    attempt-specific tool/test evidence outside this generation manifest and
    the content-addressed artifact directory; bind its digest only in the
    attempt's commit record.
10. After required validation gates pass, create an immutable
    attempt-specific commit record binding the artifact-manifest digest, exact
    validation-evidence record/set digest, validation-plan identity, full
    pass-registry digest, and expected artifact IDs. Atomically publish/reuse
    the complete artifact generation by `generation_key`, publish that commit
    record, and then update
    one small current reference to both. If any earlier stage fails or is
    cancelled, preserve the previous reference and all artifacts/evidence it
    reaches. Orphan cleanup is a separate bounded pass that proves artifacts,
    commit records, and evidence are unreachable from every retained
    successful reference before removal.

##### Coordinator state machine and cancellation linearization

Represent coordinator progress with a closed state machine, not a collection
of booleans:

`created -> snapshotting -> graph_resolved -> compiling -> link_planned ->
linked -> validated -> prepared -> committing -> committed`.

`committed` is the coordinator's successful lifecycle state; it maps to the
public result `terminal_state = success`. The other lifecycle terminal states
map directly to the public `failed`, `cancelled`, and
`commit_recovery_required` values. Do not expose `committed` as a competing
public terminal value or report `success` before the current-reference
replacement has been confirmed.

Before the reference-replace linearization point, an ordinary failure or
cancellation transitions through `aborting` to exactly one terminal state:
`failed` or `cancelled`. If storage prevents proving whether replacement
linearized, the terminal state is `commit_recovery_required` instead.
`prepared` means every required output and validation record has been
independently verified, the deterministic artifact manifest is durable in
attempt staging, and the complete canonical attempt-commit-record payload and
its digest are durable in attempt staging. This staged payload is not yet the
published immutable commit record and is not visible to readers. `prepared` is
the only state allowed to enter `committing`. `committing` covers
publication-lock acquisition, stale-input and current-reference revalidation,
immutable generation binding, immutable attempt-commit-record publication,
and current-reference replacement. A generation directory or attempt commit
record published before the current reference changes is only an orphan, not a
successful build. The current reference is the sole success/visibility marker;
an immutable attempt commit record binds validation evidence but does not make
an attempt current by itself.

Cancellation is checked at every declared safe point through `prepared` and
while waiting for publication locks. Once the coordinator enters the final
critical section, it checks cancellation one last time before making the
current-reference replacement call. If cancellation is already requested, it
aborts without changing the reference. After that check, the replace operation
is the commit linearization point and must run to a classifiable result; do not
interrupt it halfway or convert a completed replacement into `cancelled`.
If replacement succeeds, transition to `committed` even if cancellation arrived
while the operation was in flight. If it fails, re-read and validate the
current reference under the lock: the old reference means the attempt did not
commit and may finish as failed/cancelled; a complete reference to this
artifact generation and commit record means it committed and must report
success. If storage failure makes
the reference state impossible to determine, return
`commit_recovery_required`, retain both old and new generation data, disable
publication/GC for that project until recovery resolves the reference, and
never report either success or cancellation based on assumption.

Startup recovery validates the current reference, immutable artifact generation,
attempt commit record, and all bound evidence before enabling readers or garbage
collection. A valid current reference is authoritative; unreferenced complete
generation directories, commit records, and evidence are orphans eligible only
under the normal lease/retention rules. A missing,
malformed, or digest-mismatched reference is a storage recovery error: preserve
all candidate generations and require explicit repair or a deterministic
recovery procedure that can prove which committed record was last durable.
Recovery must not select the newest directory by timestamp or enumeration.

##### Artifact-store commit protocol and filesystem capabilities

Define "atomic publication" against an explicit local-filesystem adapter
contract; do not assume that a sequence of ordinary file writes is atomic or
portable. The compiler infrastructure adapter must provide exclusive lock
creation, no-follow/root-contained opens, complete write and flush, same-
filesystem immutable rename/no-replace, atomic replacement of a small
reference file, and the strongest file/directory durability operation the
platform exposes. Keep platform-specific calls inside this narrow C++ build-
tool boundary; generated COBOL and standard-library code never depend on
them. Probe or document required filesystem capabilities at the selected
build root. If the filesystem cannot meet atomic replacement, path containment,
or locking requirements, fail with a storage-capability diagnostic instead
of weakening the commit contract.

The Phase 0 capability matrix has one row per supported OS/filesystem/build
profile, not one generic "POSIX" row. For each row record the adapter operation,
the semantic guarantee required, the platform mechanism and its documented
limits, the exact filesystem/version/mount configuration exercised, the
crash/failure test proving the guarantee, and the tagged failure returned when
the guarantee is unavailable. At minimum, cover:

| Adapter operation | Required semantic guarantee |
| --- | --- |
| Root-contained open | Reject symlink/reparse traversal and path escape during resolution/open; string-prefix checks are insufficient. |
| Exclusive lock and owner check | Exactly one publisher owns a project/profile lock; stale ownership is reclaimed only after positive proof the owner is dead. |
| File and directory flush | Report whether artifact bytes, reference bytes, and directory-entry updates are durable at the profile's promised crash boundary. |
| Immutable generation bind | Atomically bind one generation key to one complete same-filesystem directory with no-replace behavior. |
| Attempt commit/evidence bind | Atomically publish one immutable attempt commit record that binds the artifact-manifest digest to the exact validation-plan/evidence set and full pass-registry digest; never rewrite a prior record. |
| Current-reference replace | Readers see either the old complete record or the new complete record; in-place truncation is never observable. |
| Lease/retirement gate | A reader lease and GC retirement transition cannot both win; process death handling cannot reclaim a live reader. |

The matrix must distinguish guarantees that are atomic from those that are
merely durable after a flush, state any power-loss assumptions, and identify
unsupported network/removable filesystems explicitly. A platform mechanism is
not evidence by itself: the named test must exercise the exact adapter path and
filesystem profile.

Publish a validated generation as follows:

1. Place staging and final generation directories under the same verified
   build root/filesystem. Reject symlink/reparse-point traversal and
   cross-volume moves. Write every artifact to a fresh attempt-specific
   staging path, flush contents, verify the canonical generation manifest and
   exact artifact set, and flush directory entries where supported.
2. Acquire a bounded per-project/profile publication lock using exclusive
   creation. The lock record contains the attempt ID and owner identity, but
   not a timeout-based claim of ownership. A contending build waits
   cancellably within a configured bound. A lock is reclaimed only when the
   adapter proves its owner is no longer live; age alone is never sufficient.
3. While holding the lock, re-read the declared artifact-input snapshot and
   artifact-producing compiler/backend identity immediately before
   publication. If they changed since this attempt compiled, return
   `stale_build_inputs` and publish nothing. Validation-tool identity is bound
   to the attempt evidence and verified for each invocation, but changing it
   does not stale generated-source artifacts. Re-read and
   validate the current-generation reference under the same lock before
   replacing it. This prevents an older slow build from replacing output after
   a newer snapshot has already been committed. Source files may still change
   immediately after this check because external editors do not participate in
   the compiler lock; therefore a reference identifies the exact committed
   snapshot, not a promise that the mutable worktree still matches it. Any
   consumer requiring a current-worktree build must compare the reference's
   input-snapshot digest with a fresh snapshot before reuse.
4. Bind the deterministic generation key to exactly one immutable generation
   directory using a no-replace operation. The directory path is keyed by the
   generation key; its manifest contains the canonical manifest and artifact
   digests. If the key is already bound, verify its complete manifest and
   artifact set: an identical result is an idempotent cache/publish hit; a
   different artifact digest for the same key is a
   `nondeterministic_build_output` failure. Do not create a second generation
   under that key or overwrite the existing one.
5. Revalidate the staged attempt-commit-record payload and its digest against
   the independently verified evidence set, then publish it immutably under its
   attempt-specific ID with no-replace semantics. It binds the `attempt_id`,
   artifact generation key and manifest digest, validation-plan
   ID/version/digest, full pass-registry digest, evidence-record IDs/digests,
   and expected artifact IDs. This publication happens after immutable
   generation binding and before current-reference replacement; it is not
   reader-visible as the current attempt until the reference names it. Keep it
   outside the content-addressed generation directory: repeated validation of
   identical artifacts may create a different valid evidence/commit record
   without changing the generation or indicating nondeterministic compiler
   output. If publication fails, retain any already-bound generation as an
   unreferenced orphan and leave the prior current reference unchanged.
6. Create a temporary current-reference record beside the published
   generation. It contains the generation key, input-snapshot digest,
   artifact-manifest digest, commit-record ID/digest, schema, and reference
   integrity digest. Flush it, then recompute the declared artifact-input and
   compiler/backend snapshot. If it no longer matches, discard the temporary
   reference and return `stale_build_inputs`. Then atomically replace the
   current reference in one adapter operation; never truncate or write the
   live reference in place. The prior reference remains valid until that
   operation succeeds. Release the lock only after the new reference and
   required directory metadata are durable to the level promised by the
   profile.
7. A reader validates the current reference, acquires leases for the immutable
   artifact generation and referenced commit/evidence records through the
   adapter's lease gate, then rechecks that none is retiring before opening
   artifacts. Lease acquisition and retirement must be mutually exclusive at
   the gate; a reader that loses this race retries from the current reference.
   It validates the artifact manifest, every referenced artifact, the commit
   record, and its evidence digests before use and holds the leases until
   finished. It never infers the current build by directory enumeration. A
   crash before reference replacement leaves the old generation current and
   the new generation/commit record as orphans; a crash after replacement
   exposes only a complete committed artifact-and-evidence set.
8. Garbage collection uses the same publication coordination and lease gate:
   mark non-current artifact generations, commit records, or evidence records
   as retiring, prevent new leases, recheck the current reference and retained
   commit records, and remove an object only after it is unreachable and all
   existing readers release their leases. Reclaim a stale lease only when
   process liveness is positively established by the adapter; when that cannot
   be proven, retain the data. A grace period alone is not proof that no reader
   still uses an artifact.

The adapter exposes these operations as tagged results with explicit
`already_exists`, `busy`, `unsupported_atomicity`, `path_escape`, `owner_live`,
`stale_owner`, and I/O outcomes. Reference replacement additionally returns
`replace_committed`, `replace_not_committed`, or `replace_outcome_unknown`; the
last outcome requires the coordinator's reference-revalidation/recovery path,
not a guessed success or failure. No code may silently fall back from atomic
replace to delete-then-rename, from no-follow access to string-prefix path
checks, or from a lock to “last writer wins.” Unit tests use a fake adapter to
inject failure at every operation; release evidence additionally exercises
the real local filesystems and replace/flush behavior promised by each
Windows/POSIX build profile.

Represent the coordinator result as `{terminal_state, failure_causes,
diagnostics, evidence}` rather than an overloaded exit code. Terminal states
are `success`, `failed`, `cancelled`, and `commit_recovery_required`. Failure
causes are a sorted set from `source_or_semantic_failure`,
`interface_or_ABI_failure`, `target_validation_failure`,
`link_artifact_failure`, `external_tool_failure`, `resource_limit_exceeded`, `stale_build_inputs`,
`nondeterministic_build_output`, and `storage_capability_failure`. Cancellation
may coexist with completed failure causes; `commit_recovery_required` is
neither success nor an ordinary failure until startup/reconciliation establishes
the reference state. Preserve all diagnostics and bounded external-tool
evidence for the failed attempt, but never give a failed/cancelled state a
success-shaped manifest. A retry always gets a new ephemeral `attempt_id`; it
retains the same `generation_key` only when its declared source snapshot,
artifact-affecting configuration, profile contract, and artifact-producing
compiler/backend identity are unchanged.
Its validation plan/evidence may differ and is recorded by the new attempt
commit record. It cannot resume from a partially written module
without revalidating its complete cache envelope.

Parallel scheduling is an optimization over the deterministic dependency DAG,
not a semantic mode. Ready-set selection uses canonical module IDs; worker
count changes only which ready node runs when. Merge diagnostics and manifests
by stable keys after workers join. Apply the single accounting algorithm in
“Resource budgets, cancellation, and bounded compilation”: reserve the
configured per-module/per-phase quotas in canonical module-ID order before
dispatch, charge stable logical work units, and reconcile each deterministic
phase barrier against the invocation-wide limit in canonical order. Do not
introduce a separate scheduler-local estimate or racing global counter. Cache
hits contribute their validated resource summaries exactly as specified by
that policy. Cancellation checks occur at declared pass safe points and at
bounded intervals inside long worklists; a worker that has not reached a safe
point must not expose a partial artifact.

##### Failure collection and deterministic arbitration

Schedule a frozen ready frontier rather than releasing dependent work whenever
an individual worker happens to finish. At each barrier, derive the complete
set of tasks whose prerequisites are satisfied, sort by canonical
`(module_id, pass_id, artifact_id)`, reserve their configured quotas in that
order, and dispatch that same set under any worker count. Join the entire
frontier before deriving the next one. Newly ready tasks wait for the barrier;
this prevents completion timing from changing the admitted task set or failure
selection.

Each task returns a structured immutable result with exactly one execution
outcome and the associated records:

| Task outcome | Meaning and required record |
| --- | --- |
| `completed` | All declared outputs passed verification and are immutable at the declared attempt/cache visibility boundary. A cache hit is recorded as a completion disposition, not a different semantic outcome. |
| `failed` | The pass ran and returned one or more typed causes, sorted diagnostics, deterministic resource charges, and the registry-declared propagation scope. No output is visible. |
| `blocked` | A prerequisite artifact/analysis is unavailable or invalid. Record the exact missing input IDs and root failed task IDs; do not synthesize a source diagnostic or run the pass body. |
| `cancelled` | The task observed cancellation at a declared safe point before committing its result. Its scratch outputs are discarded; already completed immutable task results remain individually valid but cannot make the cancelled build attempt successful. |
| `not_admitted` | The scheduler did not start the task because an invocation-scoped failure or cancellation closed admission at a barrier. Record the barrier and cause; distinguish this from a failed prerequisite (`blocked`). |
| `not_applicable` | The pass's declared applicability predicate evaluated false from verified inputs. Record the predicate/version and inputs; a missing prerequisite is never reclassified as not applicable. |

The coordinator derives task outcomes in canonical task order after each frozen
frontier joins; completion time cannot change a task's result category. Never
infer failure scope from diagnostic text. Workers never set a process-global
"first error" or cancel a peer by completion race. A `task`-scoped failure
applies to that task and its missing declared output; direct consumers become
`blocked` through ordinary prerequisite checks, and further blocking propagates
only along their own unsatisfied inputs. A `dependency_closure` failure marks
the transitive consumer closure of its declared module/artifact domain blocked
at the next barrier, retaining the original root task IDs. An
`invocation`-scoped failure prevents admission of later frontiers; tasks
already admitted still return a final outcome. These rules make propagation
explicit while keeping blocked descendants distinct from additional failures.
Independent tasks already admitted in the frontier finish, and independent
branches remain eligible at later barriers. Record blocked descendants with
the root failed task IDs rather than fabricating source diagnostics for work
that never ran. Invocation-fatal failures stop admission at the next barrier
after the current frontier is joined; their exact task/result identity is
selected canonically. Cancellation
stops admission at the next safe barrier, asks all admitted tasks to reach their
declared cancellation safe point, and discards their unpublished partial
outputs.

Do not collapse simultaneous errors into whichever worker returned first.
Represent terminal state separately (`success`, `failed`, `cancelled`, or
`commit_recovery_required`) from the sorted set of failure-cause tags (source/
semantic, ABI, target validation, tool, resource limit, stale input,
nondeterministic output, or storage). Preserve every diagnostic and cause from
the completed frontier. If a CLI needs one primary diagnostic, select the
first under the already-defined stable diagnostic total order and retain the
full set for APIs/manifests. If a non-cancellation failure and a cancellation
request coexist, report the terminal result according to the state-machine
linearization point and keep both the observed failure causes and cancellation
fact; cancellation must not erase a completed failure, and completion order
must not choose between them. Process crashes/host exhaustion remain
operational outcomes, not deterministic source diagnostics.

##### Pass concurrency and invocation-state isolation

Do not equate a configured worker count with safe parallel execution. Enforce
the concurrency class in the Section 6.6 pass registration descriptor:
`parallel_safe`, `invocation_serial`, or `process_serial`. Default an
unclassified/legacy pass to `process_serial`. The coordinator may overlap only
`parallel_safe` work with immutable inputs and outputs, diagnostics, resource
charges, and scratch state owned by that task or module. A serial pass runs
under its declared invocation/process lane; the scheduler must not bypass the
lane to meet a worker-count target.

Move mutable parser, semantic, emission, and standard-library usage state into
an invocation or module context passed explicitly through the pass APIs. Keep
shared tables immutable after initialization; mutable shared caches require a
documented synchronization/publication contract and may not influence
name/diagnostic/resource ordering. Never use a process-global current module,
template parameter list, constructor-initialization tracker, reverse-translation
condition table, active diagnostic sink, or current cancellation token. Phase 0
must record each existing mutable global, its owner/reset rule, and the pass
lane that protects it during migration. Current evidence includes
`g_cblc_constructor_parse_state` and `g_cblc_active_template_parameters` in the
CBL-C parser, the `g_cobol_reverse_conditions` table in reverse translation,
`g_standard_library_strlen_limit`, and the global generation mutex in
`transpiler_parallel.cpp`; the actual list must be regenerated from source at
Phase 0 rather than treated as exhaustive. Static `const` keyword/catalog data
is not mutable pass state.

Until a pass has an isolation proof and concurrency test, leave it serialized;
it may still participate in a parallel build DAG alongside independent proven
passes. `worker_count` is an upper bound, not a promise of overlap or a semantic
input. Report effective parallelism only as non-semantic telemetry. This permits
safe staged migration from the current mutex-guarded generation path without
claiming thread safety based on output determinism alone.

Cross-module exception propagation and object lifetime use the same checked
contract. A throwing provider and catching consumer must agree on exception
ABI version, dynamic type identity, payload descriptor, and cleanup ownership.
An imported type's destructor/copy/move operations may be called only through
exported lifecycle metadata with matching layout and visibility. Missing
lifecycle metadata is a semantic/link diagnostic, not permission to byte-copy
or omit destruction. The double-exception fatal rule remains global across
module boundaries and cannot be converted to a normal return by an adapter.

##### Cross-module exception identity and catch matching

Keep three identities separate in the type/interface/ABI records:

- **Canonical source type identity:** the canonical module ID plus declaration
  symbol ID and, for a concrete generic exception type, canonical structural
  template argument IDs. Same spelling in another module is a different type;
  importing the same declaration preserves the same identity. This identity
  does not depend on source path, discovery order, display spelling, payload
  layout, or generated COBOL names.
- **Runtime `exception_type_id`:** the versioned dispatch encoding derived from
  the canonical source identity and exception-ABI/type-identity schema. The
  complete canonical identity remains in the interface record; a finite ID or
  digest is not equality proof on its own. Domain-separate the encoding and
  validate collisions over the complete linked exception closure before
  rendering. Resolve a collision only with the declared deterministic,
  collision-checked scheme; otherwise fail the link with both identities.
  Never choose a winner by module order, silently alias two types, or use a
  host hash/address.
- **Payload descriptor identity:** the canonical field/lifecycle/layout
  descriptor and its versioned digest, including concrete payload size,
  representation, ownership, and construct/copy/move/destroy entry IDs. A
  payload-layout change does not turn one source type into another, but any
  mismatch between provider, consumer, and linked descriptor rejects the ABI.

An exported throwable type record must include these identities, its owning
module/type declaration, exception ABI and identity-schema versions, payload
descriptor, lifecycle-operation owners/visibility, and ordered direct-base
edges. Each base edge records the canonical base type identity, access, and
base-subobject projection/path ID required to bind a caught base view. Runtime
matching uses the verified linked hierarchy; it does not assume a base begins
at offset zero or infer ancestry from names. The selected 1.0 inheritance
policy must define accessible/ambiguous paths and cycles; if multiple
inheritance is excluded, reject it semantically and do not emit a partial
hierarchy. Provider and consumer records for the same canonical type must
agree field-for-field after canonicalization.

At link time, merge type records by full canonical identity, verify exactly one
compatible definition/provider where required, validate every base edge and
lifecycle owner, detect dispatch-ID collisions, and construct one deterministic
catch-matching table sorted by canonical IDs. The root's exception closure
includes not only statically declared CBL-C throw sets and catch handlers, but
also every exception type introduced by a reachable external adapter:
recognized and unknown status mappings, call-availability failures, and the
declared throw set of an exception-aware external ABI, plus exceptions
reachable through declared callback edges and their callback-to-outer-call
translation maps. Include required base
types and payload/lifecycle providers even when the external COBOL program
itself has no CBL-C module artifact. An adapter mapping to a type outside the
root closure, with missing/incompatible type ownership metadata, or with an
unresolved dynamic type is rejected before rendering/link; it must not be
omitted from the table because it originated outside CBL-C. Handler order remains source
order after semantic reachability checks; table sorting cannot change catch
precedence. The link manifest binds the merged hierarchy/table digest and all
participating module-interface digests. A throwing provider, imported
catcher, and generated COBOL dispatcher must all use that same frozen table.
Missing, stale, duplicate-conflicting, inaccessible, ambiguous, or cyclic
metadata is rejected before rendering or link; no runtime fallback to string
comparison or catch-all is allowed.

##### External COBOL call and exception adapters

Every external COBOL dependency has an explicit, versioned call contract; an
untyped raw `CALL` may not be treated as non-throwing or exception-aware by
guessing from its name or returned fields. Support three declared modes:

1. **Non-throwing contract:** the provider is verified or externally
   guaranteed not to return a source-level failure. `compiler_verified_provider`
   is allowed only when the provider implementation and complete transitive
   call/effect closure are available to the compiler and pass the ordinary
   effect/exception verifier; an interface declaration or test name alone is
   not proof. A `trusted_external_assertion` is an explicit trust assumption,
   not a compiler-proven fact: pin its canonical contract identity, version,
   content digest, authority, and required trust/attestation policy in the
   build inputs. A digest detects content changes but does not authenticate its
   publisher; remote or organizational trust must use the separate pinned
   package/attestation policy. A `profile_guarantee` may assert only behavior
   actually guaranteed by the selected COBOL profile/toolchain contract and
   still requires profile evidence. The boundary always states how native
   COBOL call-resolution/availability failure is handled; it may not silently
   become normal success.
2. **Status-to-exception adapter:** the declaration identifies the status
   argument/result field, exact success values, every recognized failure value,
   and each value's canonical exception type and typed payload conversion.
   Unknown status values have a declared `external_call_error`/invalid-status
   mapping, never success-by-default. Copy any status/payload fields before
   another COBOL operation can overwrite them.
3. **Exception-aware CBL-C ABI:** the external entry accepts the exact
   versioned CBL-C exception context/frame ABI and uses the same canonical
   throwable identities, payload descriptors, hierarchy, and lifecycle owners
   from the linked interface records. It is incompatible with a raw status
   signature or a different exception ABI unless a separately declared
   adapter translates between them.

For an exception-aware call, pass only the designated caller-owned child frame
and its versioned ABI descriptor; do not expose a pointer to a parent/caught
frame or permit the callee to retain the context after return. On ordinary
return the child frame must be `EMPTY`, and the caller's parent frame and
activation identity must be unchanged. If an exception escapes, the child
frame must be exactly `PROPAGATING`, with one complete published payload whose
canonical dynamic type is in the declared throw set and linked root closure;
its payload descriptor, lifecycle owner, ABI version, parent-frame identity,
and completion marker must match the frozen records. `RAISING`,
`HANDLER-BINDING`, `HANDLER-ACTIVE`, `CLEANING`, or `FATAL` cannot be returned
as an ordinary call outcome. Validate these facts immediately after the call,
before reading success-only results, matching handlers, or releasing staged
arguments. Any malformed state, undeclared/unlinked dynamic type, incomplete
payload, stale context, or parent-frame mutation is an ABI violation and takes
the selected profile's terminal fatal path; do not attempt to interpret,
byte-copy, destruct through guessed metadata, or replace it with a synthesized
source exception. Trace the violation with caller/callee IDs and frame token
without exposing payload bytes. Asynchronous retention is unsupported unless
a separate future ABI explicitly defines context ownership and lifetime.

##### Reverse callbacks and re-entry

Treat every external-to-CBL-C callback as a distinct reverse call edge, not
as an incidental effect of the forward external call. Its descriptor pins the
canonical callback declaration, signature/physical ABI, entry name, mode,
registration and deregistration owner, invocation context, maximum active
re-entries, invocation cardinality and ordering, argument alias/ownership,
callback-failure continuation, and whether callback state may be retained.
The external contract defines whether a callback occurs zero times, once, a
bounded number of times, or an unbounded number of times; it also states its
ordering relative to other callbacks and each mutation/read of external
argument/result buffers. A missing bound stays unbounded in effect and resource
analysis; the compiler may not assume one callback or one-shot behavior.
Phase 0 freezes the supported lifetime model. Absent an explicit 1.0
persistence/ownership ABI,
only synchronous callbacks whose registration expires before the initiating
external call returns are accepted. Retained/asynchronous callback requests
fail with a stable semantic diagnostic rather than keeping a caller frame or
borrow alive implicitly.

Each invocation obtains its own activation and child exception frame under
the active call context, and follows the ordinary activation-limit, local
storage, borrow, and exact cleanup rules. It must not reuse or overwrite the
initiating call's parent/caught frame. A callback exception may be caught
inside the callback's CBL-C body. If it escapes that body, it cannot unwind
through the foreign COBOL stack: the callback adapter must either terminate
through the selected fatal path, or translate the complete typed payload to
the exact callback status/payload ABI declared by the external contract. In
the translation mode, the external provider must explicitly declare whether
it consumes the callback failure or propagates it through the outer call; a
propagating result must map back to the same canonical exception type and
payload descriptor in the outer adapter. The compiler rejects a callback
contract that cannot preserve the declared exception identity/lifetime or
that leaves callback-failure handling unspecified. Add the declared callback
effects to the external call's conservative transitive effect/exception
summary, including repeated invocations and any mutation visible through
aliased arguments. The external call's output-validity guarantees must account
for every legal callback/mutation ordering and callback-failure continuation;
if no single guarantee is valid for all allowed schedules, report the affected
output unspecified on that status. No C++ unwind, host
exception object, hidden global current-frame pointer, or implicit callback
thread is permitted.

Keep the external program's returned status distinct from COBOL's
`CALL ... ON EXCEPTION` condition, which reports call-resolution/availability
failure rather than interpreting an application status field. The profile
must prove the selected native COBOL construct and its observable outcome.
For a status-mapped call, first complete argument staging, then perform the
call once, capture the returned status/output prefix, and only then construct
the mapped exception in the designated empty frame. The call's effects record
both status failures and call-resolution failure separately. A mapped failure
publishes no success-only result; caller-owned temporaries are cleaned on the
exception edge, but already performed external side effects or transferred
bytes are not claimed to roll back. If exception payload creation itself
fails, use the `RAISING` fatal policy. No exception may escape across a foreign
entry that does not declare the exception-aware ABI; catch/translate it in the
generated adapter or reject the boundary before rendering.

The adapter descriptor and external dependency record bind the canonical
external declaration ID, frozen COBOL program spelling, argument/result
layouts and ownership, failure mode, status mapping version, exception ABI/
type/payload records where applicable, effect summary, target-profile
capability, and contract provenance/trust basis. Generated-program behavior
may rely on an external assertion only under the consuming build's explicit
trust policy; conformance reports must label that dependency as assumed rather
than compiler-verified. The compiler verifies the descriptor and generated
adapter, not an unavailable foreign implementation. A callback that re-enters CBL-C must additionally identify the
invocation/task control context and obey activation and exception-frame rules;
an undeclared callback or foreign unwinding mechanism is rejected. Generated
adapter COBOL remains inspectable and is the only source of application
behavior; C++ compiler code may validate or marshal the descriptor but cannot
translate application statuses or construct exception payloads at runtime.

#### Template body and specialization ownership

An imported generic declaration has two separate artifacts: its public
template identity/constraints and its instantiable body. Resolve and validate
the definition's visibility before substituting arguments. The semantic
specialization key is structural and includes the canonical template ID,
canonical argument type IDs, and relevant language-level capability/conversion
choices. It excludes target profile, physical layout, and generated names, so
the same source specialization has one semantic identity across profiles. The
compiler's semantic-version compatibility check belongs in the cache envelope,
not in the source-level key. A separate generated-artifact cache key combines
that semantic key with compiler backend version, target-profile ID/version,
ABI/name-policy versions, and imported artifact hashes. This allows semantic
specialization data to be reused only when valid while ensuring profile-
specific COBOL and ABI projections are rebuilt independently. Neither key may
depend on readable mangled spelling alone.

Give generated specialization routines an explicit owner and linkage class.
For the initial design, a specialization instantiated for a consumer is
private to that consuming module and receives a deterministic
module-qualified COBOL name; equal specializations in two independent
consumers may be emitted separately without symbol collision. A specialization
may be shared/exported only when an explicit concrete-instance export names
one canonical provider and other modules import that exact instance. Do not
rely on linker-specific COMDAT/weak-symbol behavior or silently choose the
first object file. Record every specialization request, owner, dependency, and
emitted call edge in the link manifest. Missing imported bodies, inaccessible
definitions, recursive expansion, depth overflow, and conflicting explicit
instance exports fail during semantic/module planning, before COBOL text is
emitted.

#### Cache and reproducible incremental rebuilds

Add incremental caching only after deterministic non-cached builds are proven.
Cache immutable pass outputs by stage rather than using one target-bound key
for all compiler work:

- The source/semantic cache key includes canonical module ID, source-content
  hash, compiler semantic version, imported public-interface hashes, imported
  template-body hashes when used, and standard-library source-interface
  versions, and the source numeric-contract schema/rule version. It excludes
  target profile, physical ABI fingerprints, generated names, timestamps,
  absolute paths, host process state, and source-discovery order.
- The specialization/CFG/lifecycle cache key extends that semantic key with
  canonical specialization-graph identity and the versions of the semantic,
  effect, ownership, and lifecycle rules. A logical-ABI-lowering output also
  keys on `logical_call_contract_version`, which versions argument/result
  semantics and exception-context rules. These keys remain target-profile-neutral; a profile must not cause a
  new semantic specialization identity.
- The profile-lowered module/artifact key adds target-profile ID/version,
  imported ABI fingerprints, selected physical `abi_version`, descriptor
  schema version, name-policy version, standard-library ABI catalog versions,
  logical heap-handle ABI version, numeric ABI-projection version, numeric
  operation-lowering catalog version, physical projection inputs, and
  backend/lowering and renderer versions, plus the canonical
  `artifact_pass_registry_digest` for the artifact-producing pass closure.
  This key owns generated names, COBOL module IR, rendered COBOL, source maps,
  and expectation manifests; these outputs are rebuilt independently for each
  profile.
- The link-plan key includes ordered provider artifact hashes, the runtime
  catalog version, the merged heap-domain descriptor/pool-request digest, and
  the selected profile. A private provider implementation
  change rebuilds that provider and relinks dependents as needed, but does not
  force semantic re-resolution of unrelated clients when exported source
  interfaces are unchanged.

Keep the profile's semantic contract and its external tool-compatibility policy
separate from the selected validation-tool identity. The profile contract
digest covers accepted COBOL syntax, layout/calling rules, capability mappings,
and runtime semantics; the tool-compatibility-policy digest covers allowed
compiler families/versions and invocation constraints; the validation-tool
digest covers the exact compiler/linker/runtime package identity, version,
executable or trusted package-manifest digest, and invocation options. A change
only to the selected external compiler
executable/version within the same explicitly supported profile does not
invalidate source semantics, specialization/CFG/lifecycle, generated COBOL,
the artifact `generation_key`, or artifact-manifest digest. It does invalidate
all compile/link/run evidence produced by that tool, which must be rerun and
rebound to the new evidence-input identity. Temporary compiled objects and
executables are rebuilt; if one is explicitly declared as a deliverable
artifact, its toolchain digest belongs to that artifact class's key. Changing
only the tool-compatibility policy invalidates tool selection/acceptance
evidence, not generated COBOL. If the version change alters the profile
contract, runtime semantics, or declared source-format/capability mapping,
bump the profile contract/version and invalidate profile lowering, module
IR/rendering as applicable, the artifact-generation identity, and all
downstream compile/link/run evidence. Never infer compatibility from a version
string alone: validate the executable/package digest and the profile's declared
supported-version constraint. Validation-plan/test-data changes likewise rerun
affected gates without changing an otherwise identical generated-COBOL
artifact.

Every cache envelope identifies the producing pass/schema, all consumed
artifact digests, and its own integrity digest. A hit validates artifact
integrity and the compatibility contracts relevant to that stage before
reuse. Compare clean and incremental manifests and executable behavior
byte-for-byte/semantically as specified; correctness must never depend on stale
output being present.

For a successful pass that emits deterministic warnings or notes, its cache
entry also stores the complete structured source-diagnostic records produced by
that pass: stable diagnostic code/instance key, severity, phase, canonical
subject/specialization identity, typed message parameters, and primary/related
provenance IDs. It does not store terminal formatting or host paths as
diagnostic identity. On a hit, replay these records through the current
versioned diagnostic catalog and map them using the exact source snapshot
hashes bound to the cache envelope; do not rerun only the formatter or derive
locations from the mutable worktree. If a diagnostic-schema change makes the
stored typed record unreadable, invalidate that pass record (or run a
versioned, verified record migrator) rather than omit or approximate the
diagnostic. Infrastructure cache-hit/miss/quarantine events use a separate
telemetry channel and never enter the source-diagnostic set or its ordering.
Failed passes with no complete verified output are not cached as successful
results; a cache hit must not convert a prior failure into a successful empty
artifact.

Treat the cache as an optional optimization, never as an authority or an output
store. Define the following result policy:

| Cache condition | Required action |
| --- | --- |
| Entry absent or evicted | Ordinary cache miss; recompute from the verified inputs. |
| Valid key, schema, digests, resource summary, and stage verifier | Reuse only that pass output; continue downstream validation as normal. |
| Wrong key/schema/version, invalid encoding, digest mismatch, incomplete envelope, or failed stage verifier | Reject the entire entry before registration, record a stable cache event in infrastructure/evidence diagnostics (not source diagnostics), quarantine or remove only that entry through the cache adapter, then recompute from source. Never partially salvage it. |
| Path escape, symlink/reparse traversal, or cross-profile entry | Refuse to open/use the entry and report a security/artifact diagnostic; do not fall back to an outside path. A clean source rebuild may proceed only after the rejected entry is isolated. |
| Cache unavailable, read-only, full, or cache publication fails | Preserve build semantics by continuing without caching; report the cache I/O outcome separately from source diagnostics. A failed cache write creates no visible entry and cannot invalidate a complete build. Output/publication-store failures are not cache failures and remain fatal to publication. |
| Same full key concurrently produces byte-identical verified output | Atomically retain one immutable entry; the losing writer verifies and reuses the winner. |
| Same full key produces different verified output | Fail the affected build with `nondeterministic_build_output`; do not overwrite, choose a winner, or hide it as a cache miss. |

Publish an entry by writing a fresh attempt-local temporary record, flushing and
validating its complete envelope/output set, then atomically binding the full
cache key to the immutable entry with no-replace semantics. The lookup key must
retain/verify its full canonical digest; a shortened filename is only an index
hint. Readers validate after opening under a cache-entry lease or equivalent
adapter guard. Eviction marks an entry unavailable to new readers and removes
it only after existing readers release their lease; inability to prove
unreachability or reader death means retaining the bytes. Eviction changes only
performance: it is indistinguishable from a miss. Cache telemetry, hit order,
storage path, and write timing are excluded from semantic hashes and output
manifests.

Cache pass outputs and build publication remain separate transactions. A
successfully completed, independently verified pass may be cached even if a
later pass in its attempt fails; a failed, cancelled, or partially validated
pass may publish nothing. A cache hit never supplies stronger validation
evidence than the envelope binds to the exact output, toolchain/profile, and
test identity. In particular, cached translation or structural checks do not
count as fresh COBOL compiler acceptance or execution unless their independent
evidence record is valid for the exact artifacts and declared test matrix.

#### Module and linker test gates

Extend the module tests in Section 8.2 with stable IDs and fixtures for:

- missing import, ambiguous same-name providers, duplicate canonical IDs,
  duplicate imports, path aliases, and case/separator normalization across
  POSIX and Windows roots;
- deterministic graph order under source/file enumeration permutations;
  direct and transitive cycles; and rejection without partial registration;
- public versus private type/function/method/field/lifecycle visibility and
  imported signature completeness;
- interface serialization round trips, malformed/truncated/version-unknown
  artifacts, source/interface/ABI hash changes, private-only edits, and target
  profile mixing;
- caller/callee mismatches for every ABI field: argument type/order/mode,
  result presence/layout, exception context, thrown-type set, payload layout,
  ownership, lifecycle metadata, and ABI/profile version;
- unresolved external calls, duplicate `PROGRAM-ID`/export, explicit external
  runtime dependencies, complete transitive dependency closure, and stable
  link manifests;
- imported template body available/unavailable, inaccessible body, same
  specialization requested by multiple consumers, explicit specialization
  export ownership, recursive/cyclic body dependencies, and deterministic
  specialization names/call graphs;
- cross-module class construction, copy/move/destruction and exceptions,
  including failure during provider construction and cleanup in the consumer;
- clean build versus cache hit, private implementation edit versus public
  interface edit, cache corruption, and relocation of the checkout while
  preserving canonical module IDs and output; and
- compile/link/run of provider-consumer executables under every required
  COBOL profile, including negative fixtures that must fail before the COBOL
  compiler is invoked.

The gate is not passed by resolving imported signatures in a single in-memory
`t_transpiler_context`. It requires separate compiler invocations consuming
serialized interfaces, independent link planning, and compiled/executed
provider-consumer fixtures.

## 7. Ordered roadmap and release gates

The 1.0 feature inventory in Sections 4–6 and the test catalog in Section 8
remain the full target; this ordering changes implementation priority, not
scope. Do not start a later phase merely because an earlier feature translates
or emits expected marker text. Each phase exits only on its evidence gate.
Fast parser/lowering tests remain useful, but runtime claims require compiled
and executed generated COBOL. Build the test harness and CI baseline before
using future green tests as release evidence.

### Phase 0 — Reconcile the repository baseline and establish test/CI evidence

**Deliverables:** one dated status matrix; corrected/reconciled language,
ABI, template, exception, library, and COBOL-profile documents; an adopted,
versioned source numeric type/operation contract and per-profile numeric
lowering/capability matrix; normal entry-point completion/process-status and
hosted command-line argument contracts; current build and test baseline;
explicit supported target/compiler profiles; first CI
workflow; compatibility policy; public standard-library inventory; a
source-to-COBOL compiler-pass inventory with mutable-state ownership and
concurrency classifications, artifact/evidence output ownership classes,
canonical artifact-pass-registry digest projection, full validation-registry
evidence identity, and backend migration seams; canonical
module-identity and revalidated source-snapshot rules; a proposed serialized
interface-artifact schema and link-manifest contract; the build-generation
transaction, deterministic parallel-scheduling, cancellation, and
atomic-publication contract from Section 6.7, including a filesystem capability
matrix for each supported local build root/profile; a canonical
expectation/artifact-manifest schema including the canonical name-table artifact
schema, name-key byte codec, allocator algorithm/version parameters, and
provider-frozen external-name projection; a versioned correlation-report
schema for expectation-to-module-IR/output joins, with complete-coverage and
cardinality rules; and validation-evidence separation and commit/publication
rules; an inventory of
current source spans, source-map coverage, diagnostic IDs, external compiler
diagnostic handling, and deterministic compiler resource-budget/cancellation
policy; and an audit of COBOL identifier generation, profile limits,
namespace/collision rules, ABI-frozen external names, and every current
consumer that rewrites or truncates identifiers.

**Work:** determine which claims are proven, source-translated, structurally
checked, COBOL-compiled, and executed; do not conflate these levels. Re-run
initialization/build/test on the current tree and preserve the 2025
`docs/build_status.md` report as historical evidence while appending a fresh,
dated result. Repair the discrepancy between the old `libft` submodule report
and the current checkout. Audit `docs/abi_spec.md` return slots, implicit
method/lifecycle receiver presence/order/access, exception context, ownership
representation, string layout, and status compatibility. Reconcile
`docs/cblc_language_standard.md` receiver-before-arguments evaluation and
left-to-right explicit-argument sequencing with the backend call contract.
Freeze the source-level return construction rule for prvalues, named locals,
explicit moves, and merged expressions: select copy, move, or direct
construction and define any permitted elision independently of COBOL profile.
Record the selected lifecycle operation and moved-from state in the typed-IR
contract; ABI marshalling must preserve it or model any required staging as
explicit typed operations with their own failure and cleanup edges.
Before Phase 1 name allocation, freeze the versioned name-key byte encoding,
candidate/hash/collision algorithm, canonical name-table artifact schema, and
all profile stem-normalization, namespace, identifier-length, and digest-prefix
parameters. Generate independent golden key/name vectors, forced-collision
fixtures, and cache-invalidation rules; provider-published external spellings
must be fixed before consumer compilation and agree with the link manifest.
Resolve its currently unspecified `&&`/`||` short-circuit behavior as an
explicit source-language contract (recommended: left-to-right short-circuit),
and reconcile the decision with typed-IR evaluation order and CFG lowering
before claiming the logical-condition gate.
Freeze normal entry-point completion independently from subprogram return:
adopt or revise the recommended `void main()`/`int main()` result and
fallthrough rules, define the portable process-status category, and record
profile-specific raw status mappings. Reconcile the source rule with
`docs/cblc_language_standard.md`, the entry-completion module-IR operation, and
each target-profile manifest; require `BACKEND-ENTRY-EXIT-001` before claiming
entry-return or cross-module return correctness.
Freeze hosted program-argument semantics separately from the compiler tool's
own argv. Recommended API: `process::arguments()` returns an owning
`vector<string>` excluding the executable name, preserves empty arguments and
all argument boundaries, and decodes arguments as strict UTF-8. The Windows
adapter converts native UTF-16 strictly; POSIX adapters accept UTF-8 bytes
only. Invalid native encoding raises `invalid_encoding` when the API is read;
it is never replaced or locale-converted. Once the host has formed the
argument vector, quotes, backslashes, whitespace, and shell metacharacters are
ordinary content, not shell syntax. Define allocation/error behavior and
supported count/size limits in the language/profile contract; require
`BACKEND-PROGRAM-ARGV-001` before claiming hosted CLI portability.
Reconcile ANSI-85/fixed-format documentation with current GnuCOBOL/free-format
tests by naming explicit profiles rather than silently treating one as the
other. Inventory all source-controlled work counters, recursion/depth guards,
allocation sites, diagnostic accumulation, generated-output growth, and
external compiler invocation paths. Propose deterministic per-phase and
per-module resource budgets, default/override policy, cancellation points,
tool timeout/output caps, and stable resource-exhaustion diagnostics. Measure
the 1.0 corpus before selecting defaults; do not choose limits from one host's
available RAM. Freeze the activation-exhaustion ABI before Phase 1: the
catchable call-site `stack_overflow{limit, requested_depth}` payload, its
non-allocating publication, configured/profile maximum relationship, static
per-function child-frame computation, profile maximum frames per activation,
terminal behavior for a corrupted/missing reserved slot, profile-projected
maximum activation-record bytes, total live activation-storage budget, and
the native-stack or compiler-managed-arena capacity evidence. Bind the same
values and physical frame layout into the target-profile capability record,
ABI/profile artifact key, and expectation manifest; prove the storage limit
at and one byte/unit above its bound and prove activation limit/limit+1 paths
on every claimed strategy. Adopt the source
numeric descriptor before freezing backend
arithmetic: exact integer ranges, float precision/exception behavior,
conversion and rounding rules, signed division/remainder, shift semantics,
and typed failure outcomes. Reconcile the host-width and `COMP-2` claims in
`docs/abi_spec.md` with the target-neutral source contract; probe each pinned
COBOL profile for arithmetic capacity, `ON SIZE ERROR`, `ROUNDED`, intermediate
precision, and comparison behavior. The current source tree has selected
lowercase `string` for the owning standard-library class and removed the
competing compiler-managed string data kind and separate class-helper registry
paths; raw character storage remains `char[]`/character pointers. Phase 0 must verify this break
with source, registry, negative-compatibility, and template-resolution tests,
regenerate the checked-in feature-showcase COBOL that still calls the removed
`CBLC-STRCMP-STRING` helper, and record any retained helpers as raw-buffer
interop rather than as class behavior. Do not list the spelling/data-kind
removal as future work or claim class lifecycle/ABI completeness from that
source-level migration. The latest worktree search found no literal
`CBLC-STRCMP-STRING` reference in checked-in generated COBOL, `.cblc` source,
or manifests; regenerate the showcase outputs and validate their declared and
emitted call sets rather than treating this text search as conformance proof.
For backend naming, trace every spelling conversion
and fixed-size COBOL-name field from declaration through references, imports,
generated paragraphs, and manifests. Record each profile's collision domains
and distinguish fixed external ABI names from private generated names; no
current sanitizing helper is accepted as a collision-safe allocator without
this evidence.

Create the test architecture as a deliverable, not a pile of new unit tests:

1. Keep fast source/parser/semantic/IR/COBOL-structure tests.
2. Add an end-to-end fixture runner: write source to an isolated temp
   directory, invoke the transpiler, validate and retain generated COBOL,
   compile with a pinned COBOL profile, execute it with explicit closed or
   fixture-backed stdin, a separately specified logical program-argument
   vector, and a program-environment fixture distinct from the compiler/tool
   environment, then assert exact argument boundaries, normalized
   process-status category,
   profile-promised raw status mapping, byte-exact stdout/stderr, declared
   file effects, and termination behavior. The runner must not inherit a
   developer terminal or silently normalize stream bytes; require
   `BACKEND-STDIO-001` and `BACKEND-TOOL-SUPERVISION-001` before accepting
   execution evidence. Require `BACKEND-TOOL-FS-ISOLATION-001` before
   accepting generated-program file-I/O evidence, and
   `BACKEND-PROGRAM-ARGV-001` before accepting command-line behavior evidence.
   Require `BACKEND-HOST-SERVICE-001` before accepting environment, clock, or
   random-service conformance evidence, and `BACKEND-TOOL-LIMIT-001` before
   accepting generated-program execution evidence.
3. Add an expectation manifest derived from verified typed IR/CFG, lifecycle
   plans, ABI descriptors, profile facts, and frozen names—but not from COBOL
   module IR or rendered text. Compare both the module IR and independently
   parsed output against it; use hand-authored reference vectors for
   profile-sensitive decisions.
4. Add the two-domain fault-injection architecture in Section 8.1: C++ hooks
   only for compiler-infrastructure failures, and test-only CBL-C/generated-
   COBOL probes for application allocation, construction/copy/move,
   comparator/hash, I/O, and destructor failures. Add counted RAII types,
   deterministic event traces, and death-test support; prove test controls
   never enter production artifacts.
5. Add controlled output-mutation tests and ensure every mutation is rejected
   by the structural/manifest validator or an executable semantic test.
6. Add required CI jobs for the maintainer target, Windows, POSIX, and every
   selected COBOL compiler/profile. Missing COBOL compilers are a skip only in
   explicitly non-required local profiles; they fail a required CI job.

For backend inventory, trace one representative source operation through
parsing, canonical type/symbol resolution, template specialization, lifecycle
and exception lowering, ABI selection, COBOL declaration/paragraph creation,
rendering, validation, compilation, and execution. Record which file and data
structure owns each decision, where diagnostics/source spans are retained,
and whether the decision is currently repeated in multiple passes. Record how
generated declarations, cleanup, template specializations, and ABI fields map
back to their multiple source origins; identify source-map overlap behavior,
path normalization, and which compiler-tool diagnostics can be mapped today.
Identify which existing emitters may be wrapped by typed-IR adapters and which
must be replaced because they resolve names, infer ownership, or select ABI while
printing text. This is an inventory and seam definition, not authorization for
a broad backend rewrite during baseline repair. For module handling, trace a
provider and consumer through independent compiler invocations: import-path
resolution, canonical IDs, export registration, interface serialization and
validation, consumer semantic resolution, template-body access, ABI descriptor
comparison, link-manifest construction, COBOL compile/link, and execution.
Record which existing context fields are sufficient and which data is lost
when the process ends; in-memory `resolved_index` and signature tables do not
count as a serialized interface format. Trace the complete build-generation
transaction as well: identify who snapshots inputs, owns staging directories,
publishes validated cache entries, preserves the last successful generation,
orders parallel diagnostics and resource-budget charges, and garbage-collects
unreachable outputs. Verify that failure/cancellation at every stage cannot
make an incomplete provider or executable appear current; add the
`MOD-BUILD-TRANSACTION-001` harness even if parallel builds and cache reuse are
not enabled yet, so the contract is tested before those optimizations ship.

Keep test artifacts under ignored/temp paths and do not create or refresh
`automated_tests.log`. Inventory every standard-library entry with the full
ABI fields and two classifications defined in Section 2.1: public API status
(native `.cblc` typed return/exception API or legacy C-like helper) and
implementation/ABI surface (canonical `.cblc`, versioned implementation
interface, compatibility adapter, Generated COBOL ABI, C++ registry/generator,
compiler infrastructure/boundary glue, generated COBOL runtime behavior, or
test-only tooling). Record all logical arguments, result slots, exception
context, legacy status fields, ABI version, failure policy, and ownership
behavior. Keep production C++ limited to compiler infrastructure, registry/
generation, and boundary glue; C++ test drivers are test tooling only. Do not
create a second public-library or generated-program runtime implementation in
C++.

**Exit gate:** clean-checkout initialization/build and existing tests have a
fresh reproducible result; CI is configured and visible; all support claims
have one status and evidence level; no advertised `.cblc` method is called
generally supported while its lifetime/element contract remains restricted;
every ABI entry is classified and any status output is proven to belong to a
versioned compatibility boundary; no profile is described as portable without
matrix evidence. Module IDs, import-root/path normalization, cycle policy,
interface versus ABI hash inputs, template-body distribution, specialization
ownership, and link-manifest fields are specified without yet claiming that
separate compilation is implemented.

### Phase 1 — Build the semantic and lifecycle kernel

**Deliverables:** structural type references; generic capability checking;
explicit `construct`, `copy_construct`, `move_construct`, copy/move assignment,
and `destroy` dispatch; ownership/borrow rules; `new`/`delete` pairing;
defined arithmetic/null/index rules; per-invocation storage and activation
isolation; ABI v1; compatibility adapters; an
internal allocation facade with deterministic failure injection; immutable
resource-limit configuration and deterministic per-phase work accounting;
typed backend-operation IR and CFG-backed cleanup planning as specified in
Section 6.6; a canonical module graph, serialized interface reader/writer,
separate-compilation call descriptors, and pre-render link planning as
specified in Section 6.7.

**Work:** make type identity structural rather than string-only; build the
template substrate needed for ordinary concrete instantiations, including
immutable definitions, substitution, structural cache keys, concrete
checking, instantiation traces, recursion/depth limits, and stable
collision-safe mangling. This is not yet a claim that arbitrary nested type
trees work; Phase 3 proves that recursively. Add `Copyable`, `Movable`, `Destructible`,
`Equatable`, `Ordered`, and `Hashable` capabilities. Define moved-from state,
self-copy/self-move, borrow escape, and object-completion tracking. Generate
lifecycle operations recursively for class members and arrays; reject
unsupported ownership-bearing layouts before COBOL emission, with a stable
diagnostic rather than a byte-copy fallback. Distinguish allocated storage
from constructed objects and track successful subobject/element construction
so failure cleanup only visits completed lifetimes. Never byte-copy a
non-trivial object or pair `new` with `free`/`malloc`.

Assign every mutable local, receiver/argument temporary, result-completion
flag, and cleanup flag to one logical invocation activation. Lower these facts
using the Phase 0-selected profile strategy; verify that recursive calls get
distinct storage and that every return/failure edge releases or restores its
activation exactly once. Reject unmodeled re-entry and any mutable activation
field emitted as shared `WORKING-STORAGE`.

Thread the invocation's immutable resource policy through parsing, semantic
resolution, monomorphization, CFG/dataflow, module planning, and rendering.
Charge deterministic per-module/per-phase counters; check all size/count math
before allocation; bound diagnostic and generated-output accumulation. Give
parallel workers preassigned quotas keyed by canonical module IDs so worker
timing cannot decide which unit receives a limit failure. Record enough
resource summaries in cached artifacts to preserve cold/warm-cache behavior.

Unify logical argument order, return storage, and optional exception context
in the ABI. Keep old numeric statuses only behind named compatibility
adapters. Define signed overflow, float conversion, null access, and checked
index behavior. The minimal lowercase `string` owning/copy/move/destruction operations
needed as a vector element belong here; the full text API and its encoding
rules remain a later phase. Introduce the backend in slices: construct an
immutable typed-operation view from resolved translation-unit data; for every
generic function, structurally specialize and concretize dependent calls,
effects, and lifecycle dispatch; then construct CFGs only for those concrete
functions and derive lifecycle operations and cleanup from their edges. A
non-generic function follows the same pipeline with an empty specialization
step. Adapt the existing emitter to consume
these outputs rather than performing semantic resolution during COBOL text
generation. Keep the COBOL module representation and renderer separate, and
derive the test expectation manifest from verified typed IR, CFG/lifecycle
plans, ABI/profile descriptors, and the frozen name table. Validate the module
representation against that independent manifest rather than deriving the
expectation from the module representation.
Do not require an all-at-once intermediate-representation rewrite; do require
that only one pass owns each semantic decision and that each adapter has an
explicit retirement gate. Resolve imports to canonical IDs before export
registration; serialize complete public interfaces with structural types,
layout fingerprints, exception effects, and target-profile identity; consume
those artifacts in a second compiler process; and reject duplicate, missing,
stale, or profile-incompatible symbols before rendering. Defer incremental
cache reuse until deterministic clean builds and interface hashes are proven.
Carry stable provenance/origin records through typed operations, CFG cleanup,
ABI descriptors, module IR, and rendered source maps; sort diagnostics
independently of module/worker discovery order. A compiler or structural
validation error must map to its source or be explicitly identified as
generated-only, never guessed from a nearby line.

**Exit gate:** counted nested fields and arrays prove exact construction and
destruction for normal paths, self-assignment, move, and failed construction;
type/ABI tests pass across local, member, return, and cross-module values;
unsupported ownership shapes receive deterministic semantic diagnostics. This
is the lifecycle-kernel gate only; it does not establish general container or
exception-payload RAII support before the later gates pass. In addition, each
supported function has a CFG for its supported branches, loops, returns, and
scope exits; every reachable non-exceptional exit has an explicit cleanup
plan, including loop back-edges and `break`/`continue`; cleanup-operation
traces match counted-object ledgers; self/mutual/cross-module recursive
compile/run probes preserve distinct activation-local values and unwind them
in reverse order through the declared depth limit; limit+1 follows the frozen
outcome before callee entry; the typed IR has no unresolved text-only
call/member operands; and COBOL lowering consumes the IR/CFG without changing
evaluation order or selecting an implicit fallback. Full throwing-call,
constructor-failure, catch, and rethrow cleanup edges remain Phase 4 gates.
For the selected activation strategy, the activation-layout manifest, emitted
COBOL declarations/accesses, and recursive execution trace agree on storage
class, field mapping, slot isolation, and cleanup-state reset; a manifest-only
or layout-arithmetic-only pass does not satisfy the activation-storage gate.
The COBOL module representation round-trips through rendering and reparsing
with ABI, control-flow, source-map, and lifecycle-manifest equivalence. A
provider and consumer compiled in separate invocations agree through a
serialized interface and ABI descriptor; corrupt/stale artifacts, mismatched
profiles, duplicate definitions, and unresolved calls fail before COBOL
compile/link. Repeated clean builds produce the same module graph and link
manifest independent of file discovery order. Every supported pass obeys its
documented deterministic work budget; limit-plus-one, parallel-order, and
cold/warm-cache tests agree on the same structured failure; cancellation and
resource exhaustion publish no partial-success artifacts.

### Phase 2 — Prove the normal-path `vector<string>` lifecycle

**Deliverables:** replace the vector prototype's assignment/byte-relocatable
assumptions with the live-element lifecycle rules in Section 6.1; use `string`
as the first non-trivial element; generated-COBOL compile/run fixtures; checked
indexed access and the operation surface needed by this milestone. This is a
normal-path proof checkpoint, not yet a claim that the vector is fully 1.0
supported.

**Work:** complete vector construction, copy/move, assignment, capacity growth,
resize, push/emplace, insert/erase, clear, and destruction for scalar and
managed elements. Keep all representation fields private. Construct only live
elements, destroy erased/popped/cleared/shrunk elements, and keep capacity
slots unconstructed. Use transactional reallocation: stage type-directed
copy/move construction, publish only after success, destroy the old live range
once, then release the old block. Prove self-assignment and self-aliasing.
Replace current source tests for clamped `insert`, ignored invalid `erase`,
negative-size clamping, and pointer-returning indexing with the Section 2.1
1.0 behavior and post-failure state assertions; retain characterization tests
only for an adapter that is deliberately kept. `insert(-1)` and insertion
past `size` throw `out_of_range`; insertion at `size` is valid. Invalid erase
throws without mutation. Negative `resize`/`assign` throws `invalid_argument`;
oversized requests throw `length_error`. `at` and `operator[]` both check
bounds. The selected public iterator/borrow surface is not claimed complete
until Phase 6; use checked access and bounded internal iteration meanwhile.
Keep translation/COBOL marker tests, but add separate fixtures that compile
and execute generated COBOL on required profiles.

**Exit gate:** normal execution of `vector<string>` proves exact values,
deep-copy independence, move-from state, nested scope/destruction, growth,
insert/erase, clear, and allocation ownership through compiled COBOL; each
live element's string lifecycle is called exactly once and unused capacity
slots are never treated as live. No generated-text-only check counts as runtime
proof. Allocation/throw fault injection is finalized in Phase 4, UTF-8 and the
complete string contract in Phase 5, and iterator semantics in Phase 6. Do not
mark `vector<string>` generally supported until those dependent gates pass.

### Phase 3 — Prove nested generic specialization before adding containers

**Deliverables:** recursive structural specialization graph; exact method and
operator dispatch through nested receivers; generated lifecycle-call graph;
depth conformance up to at least 16 generic type constructors; deterministic
specialization-work and graph-size budgets integrated with the compiler-wide
resource policy.

**Work:** implement the process in Section 6.5, then raise the internal depth
limit in explicit steps: first 2, then 4, 8, and finally 16. Depth 2/4 are
engineering checkpoints only, never the release exit gate. At each step test
the corresponding `Vd` chain (`vector` wrapped around `string` to that exact
depth), concrete call-chain selection, copy/move, reallocate, erase,
destruction, dependency ordering, cache reuse, and deterministic mangling.
Exercise `at`/`operator[]` chains without intermediate copies.
For imported templates, consume the versioned body/typed-IR artifact and
enforce definition visibility before substitution. Record one deterministic
consumer-local specialization owner and module-qualified symbol for each
instance; two consumers may independently instantiate the same template
without relying on weak/COMDAT linker extensions. If a concrete specialization
is explicitly exported, require one canonical provider and make other modules
import that exact instance. Verify cross-module dependencies and duplicate
instance exports in the graph before emitting COBOL.
Keep nested positive-path proof separate from the later full exception-failure
campaign. Measure high-fan-out and recursively expanding type graphs against
specialization-count, substitution-work, and depth limits; memoized equivalent
instances charge the canonical unique-specialization budget exactly once.
Materialize the Phase 0-frozen exception-context transition table and
replacement-throw protocol in the logical call/exception ABI: specify payload
publication, catch-by-value binding, rethrow, handler cleanup, context
clearing, child-frame bound, replacement expression staging, payload
construction, cleanup-before-parent-release, reparenting, and failure behavior
at each transition. The one-payload-per-frame ABI must never overwrite the
caught payload or expose partial replacement state; any unsupported
replacement form must receive a stable semantic diagnostic.
Also freeze the activation-local storage contract: each dynamic invocation has
distinct locals/temporaries/cleanup flags, the selected profile strategy and
recursion limit are explicit, and limit exhaustion has one typed outcome.
Require an activation-isolation compile/run probe for each profile; neither
`WORKING-STORAGE` persistence nor host-stack capacity counts as proof.
Freeze exception type identity independently from payload layout: define the
canonical source-type key, versioned runtime dispatch-ID encoding, linked
collision policy, exported hierarchy/base-path metadata, and link-time catch
table. Require deterministic separate-module identity/hierarchy fixtures and
runtime catches before declaring cross-module exceptions supported.
Inventory every external COBOL call and freeze its non-throwing,
status-to-exception, or exception-aware ABI mode, including separate mappings
for returned application statuses and native call-availability failures.
Reject unclassified external entries and require exact adapter descriptors,
effects, ownership, and generated-Cobol conformance evidence.

**Exit gate:** every depth 1–16 parses, instantiates, emits COBOL, compiles,
links, and executes; depths 4, 5, 8, and 16 pass full normal lifecycle and
operator-chain cases. Depth 17 fails at semantic analysis in the default
16-layer profile with `TPL_NESTING_LIMIT` and a complete trace. Generated
routines resolve only to the exact canonical child type. No additional
container family begins before this gate passes.

### Phase 4 — Integrate deterministic exceptions and cleanup with nested values

**Deliverables:** cleanup-aware control-flow representation; complete
exception payload ABI for every throwable type allowed by the 1.0 language;
type-directed payload construct/copy/move/destroy dispatch; exception ABI
versioning; typed standard exceptions; N-th-operation fault injection;
cross-module propagation and double-exception termination proof; versioned
external COBOL call/exception adapters; validated foreign exception-frame
return protocol; and typed synchronous reverse-callback lowering.

**Work:** first reconcile the exception implementation plan's explicit
limitations against the 1.0 type/features matrix. For every throwable layout,
either implement its ownership-aware payload operations or reject it with a
stable semantic diagnostic and document it as outside 1.0. Close the payload
gaps that intersect 1.0: arrays and nested elements, pointer/owner fields,
user-defined destructor dispatch, constructor forwarding for supported
inheritance forms, broader generic lifecycle dispatch, payload size/layout and
version agreement across modules, and destruction after catch or rethrow.
Multiple inheritance is not added by implication here: if it is not selected
for 1.0, make rejection explicit and test it. Do not call the implemented
scalar/string/trivial-struct subset universal exception support.

Route return/break/continue/throw/call failures through generated cleanup;
track partial construction at every nested level; make destructors
non-throwing; define the versioned native-COBOL uncaught-reporter ABI, its
no-user-call/no-managed-allocation contract, exact live-frame reads, and the
profile's output/termination mappings for normal completion, uncaught startup,
uncaught `main`, and fatal double exception. Model report-before-payload-
destruction-before-service-finalization-before-heap-release in the startup and
shutdown CFG and independent expectation manifest; compile/run tests must
observe the same ordering, including heap-owning exception payloads and
reporter-output failure. Inject failure at each allocation and at every
element/base/member/array/nested-container constructor, copy, and move
position. During reallocation, prove the staged prefix is
cleaned exactly once while the original container stays valid; after commit,
prove old storage is destroyed once. Test caught allocation and file errors,
function/method/constructor/imported-module propagation, and every payload
copy/move/destruction/result-slot edge. Preserve the hard rule: a second
exception while throwing, propagating, or cleaning up an active exception
aborts immediately and never replaces the original. Implement and verify each
transition in the exception-context state machine, including failed payload
construction, failed catch-by-value construction, rethrow, handler completion,
and the Phase 0-frozen replacement-throw protocol. The fatal edge must be
terminal in both CFG and generated program, bypass the ordinary uncaught
reporter, and take the profile's fatal termination mapping without invoking
user code or resuming cleanup; it is not an ordinary exception propagation
edge or profile-specific helper call that returns. Repeat nested
call/throw/catch and exact-lifetime proofs across recursive activation frames;
an exception frame, local, or cleanup flag may not be shared by two live
invocations. Inject activation-limit failure from an empty call site, an active
handler, and while a frame is raising/propagating/cleaning: the first follows
the typed call failure, a handler uses its child frame, and the latter states
take the fatal double-exception edge while preserving the original payload.
Merge exported throwable identities and base-hierarchy records at link time;
validate payload descriptors and lifecycle owners, reject dispatch-ID
collisions or stale/inaccessible/ambiguous hierarchy data, and generate the
catch table from that one frozen linked record set.

Integrate the Phase 0 external-call inventory into the same exception/effect
pipeline. For every imported COBOL call, lower exactly one frozen adapter mode:
verified non-throwing, status-to-exception, or exception-aware CBL-C ABI.
Represent application status failures separately from native COBOL call
availability failures in the call's effect summary and CFG. A status adapter
must specify which `out`/`inout` fields are valid, unchanged, or unspecified on
each failure result; do not infer rollback or buffer validity from COBOL
`CALL ... ON EXCEPTION`. Stage arguments once, capture status and required
outputs before another operation can overwrite them, then map/construct the
typed exception before any success-only result use. Generated adapter code
must use the ordinary type-directed payload and cleanup machinery, and must
not allow a foreign exception to cross a boundary lacking the exact compatible
exception ABI. A failed payload construction follows the terminal second-
exception policy. The independent COBOL verifier checks the adapter's call
mode, call count, status read order, validity contract, immediate exception
edge, result publication, cleanup, and ABI identity against the serialized
effect/adapter records. Execution fixtures must include external side effects
before failure and assert only the contractually promised state; the compiler
must not claim those effects were rolled back.

For every exception-aware external call, implement the post-return frame check
before result use or cleanup: accept only `EMPTY` or a complete declared
`PROPAGATING` payload, verify the child/parent frame and activation identities,
and take terminal fatal handling for malformed, stale, incomplete, or
undeclared state. For each synchronous reverse callback enabled by Phase 0,
materialize its callback edge, invocation cardinality/order, aliases, child
activation/frame, and failure-translation policy in the typed call graph,
effect summary, cleanup CFG, interface, and link manifest. Test callback
exceptions caught within CBL-C, translated across the callback ABI, consumed
by the provider, and propagated back through the outer adapter; no path may
unwind through a foreign COBOL frame. Retained/asynchronous callbacks remain
rejected unless their separate lifetime/ownership ABI is explicitly admitted
for 1.0 and receives the same lifecycle, concurrency, and failure proof.

**Exit gate:** exact-once destruction and documented strong/basic guarantees
are proven for normal and exceptional control flow, including nested
`vector<vector<string>>` at depths 4, 5, 8, and 16; every in-scope throwable
payload layout passes or every excluded layout has a deterministic negative
test; all injected failures leave no leaks or duplicate owners; cross-module
catches and external status/availability mappings work; exception-aware
foreign frames are validated before use and malformed-frame death tests
observe terminal profile behavior with no ordinary reporter or post-fatal
cleanup; every admitted callback cardinality/order and exception translation
mode passes effects, exact-cleanup, compile/link/run,
and link-closure tests on claimed profiles; no callback exception escapes
through foreign COBOL; second-exception death tests assert exact termination
behavior. The uncaught reporter passes independent manifest/COBOL mutation
tests and compiled execution on each claimed profile; for heap-backed payloads,
the runtime trace proves reporting while payload/services are live, payload
destruction before service teardown, and heap release only afterward. Startup
uncaught, `main` uncaught, and fatal double-exception outcomes match their
separate portable categories and pinned raw-status mappings. External trust
assumptions are present in the release evidence and are never reported as
compiler-verified behavior.

### Phase 5 — Deliver the complete `string` subsystem

**Deliverables:** one canonical owning `string` class source; documented UTF-8
representation and byte/codepoint APIs; safe mutation/search/slicing,
parsing/formatting, and bounded C interop; minimal `optional<size_type>` for
not-found results; no meaningless status parameters. This is a dedicated
subsystem gate, not a cleanup task that can be folded into container work.

**Work:** consolidate behavior in `standard library/string.cblc`, make
representation private, use `new`/`delete`, and use the selected lowercase
`string` class spelling. Keep raw `char[]`/character-pointer interop distinct
from owning text. Set UTF-8 validity, embedded-NUL,
length/capacity, byte-offset, codepoint, compare, whitespace, and ASCII-case
rules. Implement the Section 4.2.2 methods and Section 4.2.3 failure guarantees.
Retained C-like names become thin, versioned adapters only. Strict `parse_*`
methods return parsed values or throw exact typed errors; never return a
plausible zero or silently truncate. Any legacy scalar parser that retains a
zero-on-error convention must be unmistakably marked as a compatibility
entrypoint and tested separately, not exposed as the safe class parser. Add
views only after their lifetime rules are enforced.

Re-run all `vector<string>` and nested-vector fixtures after each string
representation/API change; string is the leaf whose relocation and destruction
exercises generic lifecycle dispatch.

**Exit gate:** every string method has success/boundary/failure tests, including
UTF-8 and allocation injection; copy/move/self-assignment, overlapping input,
embedded NUL, and interop are proven; all prior vector/nested tests still pass
on every required compiler profile.

### Phase 6 — Complete iterator/borrow semantics and reusable algorithms

**Deliverables:** input/forward/bidirectional/random-access capability model;
public vector iterators; stale/cross-container detection; common algorithms;
borrow-safe `span<T>`/`string_view` where support is enforceable.

**Work:** specify const/mutable iterator behavior, past-end operations,
invalidation for every container operation, and release-mode behavior. Add
owner/generation tracking in checked builds and deterministic `iterator_error`
reporting. Implement `begin`/`end` and complete the 1.0 vector method surface;
then implement algorithms against the weakest required iterator capability.
Do not expose views that can outlive their owner; if static borrow checks are
not sufficient, use bounded callback APIs or defer that view feature without
weakening ownership guarantees.

**Exit gate:** every iterator operation and invalidation rule is tested for
valid, end, stale, and foreign cases; algorithm constraints diagnose unsupported
iterator capabilities at compile time; vector's complete public 1.0 API passes
after iterators are added.

### Phase 7 — Add foundational value types and remaining sequence/ordered containers

**Deliverables:** complete `pair`, `optional`, typed `result`, bounded `tuple`,
and `variant` (or explicitly smaller tagged union); `array<T,N>`, `deque<T>`,
`list<T>`, `forward_list<T>`, `set<T>`, `multiset<T>`, `map<K,V>`, and
`multimap<K,V>` implemented in canonical `.cblc` units.

**Work:** extend the Phase 1 internal allocator facade to all new value and
container storage and use lifecycle-aware transactional construction; keep
user allocator injection deferred.
Implement fixed array semantics, then sequence containers in dependency order:
`array`, `deque`, `list`, `forward_list`. Add ordered associative containers
only after `pair<const K,V>`, iterator stability, and equality/order
capabilities are proven. Use an AVL or other justified balanced tree; never
expose tree shape. Specify unique/multi insertion return values, immutable
map keys, `operator[]` default construction, missing `at`, comparator errors,
complexity, and invalidation. Every new family must work with `string` and
supported non-trivial/nested values, not only integers.

**Exit gate:** method/error inventories in Sections 4.2 and 8 pass per method;
sequence/property models pass; tree invariants survive rotations, duplicate
keys, failed comparisons, and allocation failure; managed values are
constructed/destroyed exactly once. This phase does not yet include unordered
containers, adaptors, or smart pointers.

### Phase 8 — Add unordered containers and container adaptors

**Deliverables:** `unordered_set`, `unordered_multiset`, `unordered_map`,
`unordered_multimap`, `stack`, `queue`, and `priority_queue`.

**Work:** implement bucket/node ownership in native CBL-C; prove hash/equality
consistency, collisions, load-factor rules, staged reserve/rehash, and
invalidation. Implement stack/queue/priority-queue as policy wrappers over
already tested sequences. Specify comparator/hash throws, empty access/pop,
return values, exception guarantees, and stable behavior without promising
bucket iteration order. Preserve all managed/nested element tests.

**Exit gate:** differential models and randomized operation sequences pass;
invariants hold after every insertion/erase/rehash and injected failure; all
managed key/value destruction and iterator invalidation rules are verified.

### Phase 9 — Add unique/shared ownership and complete utility ownership coverage

**Deliverables:** `unique_ptr<T>`, `shared_ptr<T>`, `weak_ptr<T>`,
`make_unique`, `make_shared`, and documented ownership/thread profiles.

**Work:** unique ownership is non-copyable and move-transferring; deletion
dispatches the correct generated destructor and matching `delete`. Shared and
weak owners use one control block with checked strong/weak counts and exactly
once last-owner destruction. Define thread safety as an explicit profile;
document cycles and weak edges. Add custom deleters only if callable and ABI
support are already stable. Exercise these types nested inside the containers
from Phases 7–8 and those containers inside the owners.

**Exit gate:** count overflow, allocation/control-block/pointee-constructor
failure, move/copy/reset/swap, expiration, lock races where threading is
supported, cycles, and cross-module destruction all pass with exact resource
counts. If threading is out of scope, tests prove that unsupported concurrent
use is diagnosed/documented rather than implying thread safety.

### Phase 10 — Complete algorithms, text/math/random and hosted program facilities

**Deliverables:** any algorithms not completed in Phase 6; safe formatting
and parsing; console/CLI, file/path/filesystem, date/time, random/math,
environment/process/exit facilities, each behind portable capability
adapters.

**Work:** apply one error policy across APIs: typed exceptions by default, with
explicit `result<T,E>` alternatives only where deliberately non-throwing.
Total functions return values; no-result mutators return `void`. Keep host
specific calls behind adapters, paths as path values rather than separator-
manipulated strings, and library `.cblc` free of POSIX/Windows branches. Add
deterministic random seeding and explicit unavailable-capability behavior.

**Exit gate:** representative multi-file applications compile and execute on
every required target/compiler profile; path/filesystem, console/CLI, program
environment, process/exit, wall/monotonic clock, random, and error behavior
matches the portable contract. `BACKEND-HOST-SERVICE-001` and the associated
filesystem, stdio, argv, and resource-supervision gates pass for every claimed
profile; no host-service claim is inferred from source translation or a
compiler's acceptance alone.

### Phase 11 — Tooling, packaging, compatibility, and release candidate

**Deliverables:** stable CLI and diagnostics; formatter; package/build
manifest; versioned documentation, examples, migration guide, release
checklist, reproducible artifacts, serialized source-map/provenance artifacts,
versioned adapters for diagnostics emitted by supported COBOL compilers, and
documented resource-limit/timeout configuration with stable exit and diagnostic
behavior.

**Work:** establish 1.x source/behavior compatibility and ABI-version rules;
test clean builds without source-tree library files at runtime; verify embedded
sources are mechanically synchronized; run full conformance, regression,
fuzz/property, memory/lifecycle, output-mutation, and cross-platform suites.
Audit all emitted COBOL against the selected profile. Keep compiler
infrastructure in C++ and public standard-library implementations in CBL-C.
Package and validate interface artifacts, target/profile manifests, link
manifests, dependency closure, and template-body references. Test relocation of
the installed compiler/project without leaking absolute build paths into
module IDs, specialization symbols, cache keys, or serialized ABI data.
Document supported resource-limit overrides, cancellation guarantees,
external COBOL compiler time/output caps, and the distinction between a
deterministic compiler budget failure and a host-tool timeout. Exercise those
settings from the installed CLI and build integrations, not only internal APIs.

**Exit gate:** no unexplained severity-1 correctness/lifetime/portability
issue; every 1.0 feature and failure contract has conformance evidence; clean
install and sample applications succeed across the official matrix; generated
COBOL/ABI and source/library semantics agree.

## 8. Testing and conformance strategy

This section is the 1.0 test inventory, not merely a list of test styles. Each
listed case must become an executable test with a stable ID, input/fixture,
expected result, and assertion. The test ledger must map every normative rule
and every documented failure condition to at least one test. “Exhaustive” means
exhaustive over specified failure modes, boundary classes, and control-flow
edges; no finite suite can enumerate every possible input value. Fuzzing and
generated operation sequences cover that unbounded space, and every discovered
failure becomes a permanent regression test.

### 8.1 Required assertion and test-runner contract

Record each test's evidence level explicitly. A test passing at one level must
not be reported as proving a later level:

| Level | Test action | What it proves (and does not prove) |
| --- | --- | --- |
| Source/semantic | Parse, type-check, and inspect typed IR. | Source legality and compiler decisions; does not prove emitted COBOL is valid. |
| COBOL structure | Parse output and compare names/layout/call graph to the IR manifest. | Structural and ABI-shape agreement; does not prove a real COBOL compiler accepts or executes it. |
| Compiler acceptance | Invoke the pinned compiler/profile on the generated source and link all required modules. | Dialect acceptance and link resolution; does not prove runtime behavior. |
| Execution | Run the generated executable and compare exact result/output/effects. | Successful-path behavior for that fixture/profile; does not prove exceptional paths or portability elsewhere. |
| Failure/lifetime | Inject faults, count lifecycle events, inspect post-failure state and termination. | Specified failure guarantee and cleanup for the injected cases. |
| Matrix/release | Repeat required cases on clean Windows/POSIX builds and every supported COBOL compiler/profile. | Only this level supports a cross-platform/release claim. |

Keep these layers in one fixture pipeline when practical, but report each
stage's result separately. A skipped compiler or an expected-marker match is
not a passing execution test.

Represent every planned test claim as a stable tuple
`(case_id, evidence_level, profile_id, subject_digest, test_plan_digest)`,
resolved in the run manifest to exactly one current state:
`passed`, `failed`, `cancelled`, `blocked`, `unsupported`, `not_applicable`, or
`not_run`. Each actual execution has an immutable attempt record with its
terminal execution result (`passed`, `failed`, or `cancelled`), attempt ID,
exact input/evidence closure, test fixture and oracle digests, tool-result
record, and produced evidence. `passed` is valid only for the named evidence
level; a source or COBOL-structure result cannot be promoted to later levels:
compiler acceptance, execution, failure/lifetime, or matrix evidence.
`not_applicable` requires the catalogued
versioned applicability predicate and its evaluated inputs. `unsupported`
requires an identified missing profile capability; it is not a pass and can
satisfy a release matrix only through an explicitly approved, narrowly scoped
capability exclusion. `blocked`, `cancelled`, and `not_run` never satisfy a
required case. `blocked`, `unsupported`, `not_applicable`, and `not_run` are
manifest resolutions, not fabricated execution attempts. Failed attempts
remain immutable history; a retry may satisfy the current gate only when its
own complete evidence binds to the same required artifact, plan, profile, and
fixture identities.

`subject_digest` is the canonical digest of the exact subject consumed at that
evidence level, not an assumed future artifact: source/semantic claims bind to
the frozen source-input closure and semantic/pass identity; COBOL-structure
claims bind to the expectation and generated-artifact manifests; compiler,
link, execution, and failure/lifetime claims bind to the exact artifact
manifest and relevant executable/input digests. Record those component
digests in the attempt so the subject can be independently reconstructed.
Validation evidence remains outside the artifact manifest and generation key;
the attempt commit record binds the resulting evidence back to the artifact
manifest without a self-referential digest.

The test catalog declares the complete case IDs, required evidence levels,
profile applicability, and required/optional status before execution. A run
manifest enumerates every catalog tuple for the selected profile and accounts
for each as attempted, predicate-proven `not_applicable`, or unresolved;
predicate-false tuples are not members of that profile's required set. Filtered
runs must identify themselves as partial and cannot report suite/release
success.
An independent aggregator derives gate state from case records and the catalog,
not from a runner's summary boolean or pass count. For each required
`(case_id, evidence_level, profile_id)`, only a complete passing attempt bound
to the current artifact and plan satisfies the gate. Missing records,
unapproved `unsupported`, unresolved `blocked`/`cancelled`/`not_run`, stale
digests, or invalid result records make the aggregate incomplete or failed.
The aggregate includes catalog/plan/profile digests, selected and required
tuple counts, the exact satisfying attempt IDs, exclusions with approvals, and
the unsatisfied tuple list. It never upgrades evidence when later stages were
skipped.

For every runtime failure test, assert all applicable properties—not just that
the program failed:

1. Exact exception category/code, or exact `result` error alternative for an
   explicitly non-throwing API.
2. Exception payload and source file/line/column where specified.
3. Object/container/file state after failure, including the documented
   strong/basic guarantee.
4. Constructor, copy/move, and destructor counts, proving exact-once cleanup.
5. No leaked allocation, double delete, stale ownership, or unintended output.
6. Whether execution continues, propagates to a catch, or terminates, exactly
   as specified.
7. Deterministic compile-time diagnostics (diagnostic ID and source spans).

Use this default runtime error mapping; the exact public class names are to be
frozen in the language standard before implementation. No failed operation may
silently clamp, return a plausible success value, or set an undocumented
integer status:

| Failure | Required observable outcome |
| --- | --- |
| Invalid argument/precondition | `invalid_argument` with stable error code and originating source location. |
| Index/range/empty checked access | `out_of_range`. |
| Requested size/capacity exceeds `max_size` | `length_error`. |
| Numeric result is not representable | `overflow_error` (integer arithmetic) or `range_error` (parsed/formatted numeric conversion). |
| Allocation failure | `bad_alloc`; operation leaves the specified valid state and leaks nothing. |
| Malformed text/encoding | `parse_error` for invalid grammar/trailing junk; `invalid_encoding` for malformed UTF-8. Never return a success-shaped zero/empty value. |
| Numeric domain failure | `domain_error` for mathematically invalid operands; `range_error` for valid operands whose result is outside the supported numeric range. |
| Invalid/stale/cross-container iterator | `iterator_error` at runtime; compile-time rejection when statically knowable. |
| I/O/filesystem failure | Portable `io_error` category with a stable subcode; native OS detail is diagnostic only. |
| Wrong `optional`/`result`/`variant` alternative | `bad_optional_access` / `bad_result_access` / `bad_variant_access`. |
| Uncaught exception or exception during active exception cleanup/propagation | Defined termination path and portable process-status category, with any profile-promised raw status mapping; a second exception never replaces the first. |

Normal absence is not a failure: e.g. `find` returns empty `optional`,
`weak_ptr.lock()` returns empty after expiration, and a documented `contains`
returns false. Compile-time-invalid programs produce diagnostics, not runtime
exceptions. Any explicitly non-throwing result API lists its complete error
alternatives in its signature and gets the same exact-outcome assertions.

#### Deterministic fault injection and generated-program tracing

Keep two fault domains separate. Compiler-infrastructure tests may inject C++
allocation, filesystem, scheduler, or tool-process failures into the compiler
itself. Generated-program tests must inject application allocation,
construction/copy/move/comparison/hash, file, and destructor failures through
test-only CBL-C fixtures and compiler-generated COBOL instrumentation; a C++
test hook must never implement or replace the source program's operation,
exception, ownership, or cleanup semantics.

In test mode, assign every injectable semantic operation a stable
`fault_site_id` derived from its canonical module/source operation ID, concrete
specialization key, operation kind, and lifecycle target. Do not derive the ID
from rendered names, source line alone, process addresses, worker order, or an
occurrence counter. One semantic operation may expose multiple injection
boundaries, so identify each boundary separately by a stable
`fault_boundary_id` derived from its `fault_site_id` and its declared semantic
stage. The fixture supplies an immutable fault plan with the test-case ID,
site ID, boundary ID, 1-based occurrence ordinal at that boundary, action, and
typed payload. Actions are restricted to that boundary's declared failure
category and state effect, for example fail allocation with `bad_alloc` before
storage becomes usable, throw the selected exception before construction
completion, fail a comparator/hash before its result commits a container
mutation, or trigger a destructor exception during a termination test. The
probe fires once at the specified occurrence and then disarms; unmatched or
inconsistent site/boundary/ordinal plans are instrumentation-test failures,
not silently ignored. A test process is fresh for each distinct injection
position so one failure cannot contaminate another case.

Place probes at the semantic effect boundary, not at source spelling or a
generic statement counter: allocation before storage becomes usable;
construction/copy/move before the destination completion bit is committed;
comparison/hash before its result controls container mutation; I/O at the
declared operation boundary with exact transferred byte/record counts; and
destruction at the destructor call while preserving the active exception for
double-exception tests. Each boundary declares whether the operation has not
started, has produced a tentative result, has partially changed external or
container state, or has committed its completion/live bit. Partial I/O
injection is legal only when the API contract permits partial transfer, and
must report the exact transferred count and post-failure stream/buffer state.
The hook may record the event or trigger the declared failure, but cannot skip
a real operation and report it as successful. For an injected failure that
models a failure before resource acquisition, entering the ordinary declared
failure edge is the operation's required result; no successful acquisition may
be claimed. In all cases it must preserve the specified state guarantee and
exercise generated cleanup, handlers, and termination paths.

Emit probes and their control table only in a separately identified test
instrumentation mode. Implement test controls as test-only CBL-C source and
generated COBOL state/paragraphs; configure them through the fixture program
before the operation under test, not through POSIX/Windows environment
variables or host allocator interposition. Mark every instrumented artifact
and evidence record as test-only so it cannot satisfy a release-build
compile/run gate. A no-fault instrumented run must match the uninstrumented
program's specified observable state and result, excluding explicitly
test-only trace output. Production output must contain no test control fields,
probe calls, failure-plan symbols, or test runtime dependency.

Instrumentation mode, instrumentation schema/version, and the test-runtime
module digests are artifact-affecting inputs: include them in the instrumented
generation/cache key and manifest. Use a distinct test-only cache and
publication namespace; an instrumented generation cannot replace or become
reachable from the release current reference. A link closure must contain
either one consistent instrumentation build identity or none—never mix an
instrumented module with an uninstrumented provider for a test claim. The
release namespace and release link planner reject test-only artifact kinds
even when their COBOL and ABI digests otherwise validate. Test instrumentation
version changes rebuild the injected artifact and site manifest without
changing target-neutral source semantic identities or release-cache entries.
The fault plan is attempt evidence input, excluded from the artifact key when
consumed as runtime fixture data; its exact digest is bound into validation
evidence. If a fixture embeds a plan as generated constants, that plan becomes
an artifact input and produces a distinct test-only generation key. Never
reuse an instrumented artifact after changing embedded plan values.

Treat instrumentation as a verified, opt-in backend transform over the
already-specialized, verified semantic CFG and lifecycle plan. It may add only
tagged `test_probe` operations, test-only state/calls, and the failure routing
needed to select an outcome already declared for the probed operation. It may
not change source-level effects, overload or lifecycle targets, ownership/live
states, cleanup order, logical/physical ABI descriptors, or add a new exception
category to a non-throwing operation. Reject a fault plan whose requested
failure is outside the operation's verified effect summary; do not make a
non-throwing source operation appear throwable just to test it. In a no-fault
run, removing all tagged probes, test state, and test-only calls from the
instrumented IR/CFG must reconstruct the exact pre-instrumentation semantic
operations, edges, lifecycle states, and ABI projection. With a fault enabled,
the trace must match the declared ordinary failure edge at that precise
boundary, including whether the operation began, what state was committed,
and which cleanup/handler/termination path followed. This equivalence check is
independent of ordinary output comparison so instrumentation cannot mask a
semantic regression by changing both the generated program and its oracle.

Every instrumented build also emits a test-only instrumentation-site manifest
from verified typed IR/CFG/module IR and root startup/shutdown planning
records. Each entry binds the stable `fault_site_id` to the fixture/test-case
ID, canonical module and source snapshot, a source operation or canonical
compiler-generated root-operation ID, concrete specialization IDs when
applicable, operation/effect boundary IDs, `probe_mode`, allowed
failure/state-effect categories, expected trace event kinds, typed target or
failure category, generated probe-node/call IDs, and instrumented COBOL
artifact digest. `probe_mode` is exactly `fault_injection` or
`observation_only`; observation-only sites have no allowed injected failure
and cannot change an operation's effects. The manifest records only static
site identity and provenance; runtime occurrence counts and injected actions
belong to the immutable fault plan and trace. The independent verifier checks that each
declared boundary maps to exactly one intended generated probe, the probe is
at the declared semantic boundary, no unlisted probe exists, and the probe
cannot bypass or replace the underlying COBOL operation. A fixture's fault
plan must resolve every site/boundary pair and allowed action against this
manifest before execution; unknown, duplicate, stale-artifact,
wrong-specialization, or wrong-boundary references fail before launching the
generated program. The site manifest, fault plan, instrumented module, trace,
and result evidence are bound together by their exact digests and marked
non-release evidence. Renderer-only changes
may change probe node IDs and COBOL ranges but must not change the semantic
`fault_site_id`; regenerate and revalidate the site manifest for the new
instrumented artifact.

Compiler-generated root operations receive deterministic synthetic semantic
IDs before module-IR construction. Derive each ID from a domain-separated
encoding of the canonical executable-root ID, operation role (for example,
root-control initialization, heap bootstrap, one canonical runtime-service or
global-initializer node, reporter entry/result, payload release, heap check/
release, or process termination), and the canonical source/service/type
identity or deterministic initializer-graph node ID when present. Where no
source node exists, use a role-specific ordinal from an already frozen
canonical root plan. Do not include COBOL paragraph spellings, module-IR IDs,
rendered ranges, worker/discovery order, host paths, or attempt-local tokens.
These synthetic sites are marked `compiler_generated_root` and bind to the
semantic expectation/provenance record for the operation. Their IDs remain
stable across renderer-only changes; a root-plan, source, profile-contract,
or reporter-ABI change invalidates the appropriate site manifest and
instrumented artifacts. A site that only records shutdown order is
`observation_only`; it never becomes a legal fault-injection boundary unless
the underlying operation independently declares that failure effect.

The test trace is a generated-program observation stream, not a C++ simulation.
Each event records a monotonic test-local sequence number, `fault_site_id`,
`fault_boundary_id` when applicable, event kind, canonical type/specialization
ID, deterministic test-local activation token and parent-activation token,
execution-context token when the profile supports concurrency, deterministic
test-local object token, container depth/index where relevant, completion
state, and exception identity/frame token. `activation_enter`,
`activation_exit`, and `activation_limit_failure` events identify the exact
call boundary and outcome. Activation/object/frame tokens are assigned by test
instrumentation, have no source-program meaning, and are never COBOL addresses,
host addresses, or ABI values. Events outside an activation (for example,
bootstrap/global initialization) encode activation and parent tokens using the
schema's explicit null representation; they never omit fields or use a valid
token value as a sentinel. The harness compares the trace with the expected
construction/completion/destruction ledger, including partially constructed
prefixes, per-activation lifetime ownership, exact-once cleanup, allocation
balance, exception-frame parentage/preservation, and termination. A live
activation token is unique within its execution context; its parent token must
match the call graph, and every nonfatal exit closes it exactly once after
cleanup. An activation-limit failure occurs before the child enter event; a
fatal double exception may leave active tokens unclosed because termination
skips remaining cleanup. Capture trace bytes on a dedicated, versioned
test-only transport separate from application stdout/stderr and uncaught-report
output; never multiplex trace records into an application stream or split a
mixed stream with a prefix heuristic. The transport is declared by the
validation profile and may be a dedicated test-only COBOL file or an
independently captured stream, but it cannot change source-visible output or
application state. If a required profile cannot isolate the transport, its
trace-based evidence gate fails as unsupported rather than silently mixing
channels. Transport identity/version belongs to validation evidence, not the
semantic/generated-artifact key. Do not write or refresh
`automated_tests.log`.

The profile's trace-transport descriptor is a closed, versioned contract, not
just a channel name. It records the transport kind (`dedicated_file` or
`dedicated_capture`), endpoint ownership and isolation rule, raw framing and
encoding, maximum bytes, write/flush points, and the runner's completion and
failure classification. A `dedicated_file` endpoint is a runner-created,
per-invocation path in isolated test scratch, distinct from the fixture's
application-visible working directory and every declared application file;
the runner verifies the resolved endpoint and rejects collisions, links, or
redirections that alias application data before execution. A
`dedicated_capture` endpoint is a separately created and drained process
channel, not a multiplexed stdout/stderr pipe. The descriptor cannot be
overridden by source input or a runtime environment value, and it does not
grant application code access to the test trace. The evidence record binds
the descriptor digest and actual endpoint-kind identity, while the ephemeral
path/handle is recorded only in attempt-local supervision evidence and never
in semantic or generated-artifact identity. Missing, contradictory, or
unverifiable isolation facts make the transport unsupported; they do not
permit a runner-selected fallback.

`event_kind` is a closed, schema-versioned enum; unknown kinds always fail
validation. Root shutdown traces must support
`root_control_initialize`, `heap_bootstrap`, `runtime_service_initialize`,
`module_init_complete`, `module_ready`, `entry_enter`,
`entry_result_capture`, `global_object_destroy`, `fatal_double_exception`,
`uncaught_report_begin`, `uncaught_report_complete`,
`uncaught_report_output_failure`, `exception_payload_destroy_begin`,
`exception_payload_destroy_complete`, `runtime_service_finalize`,
`heap_leak_check`, `heap_release`, and `process_termination_request`. These
records carry stable reporter/exception/service/operation IDs, the relevant
frame or object token, and the expected profile terminal category where
applicable; they never copy exception payload bytes or stringify arbitrary
fields. Every report attempt has one `uncaught_report_begin` and exactly one
terminal report event. `uncaught_report_complete` carries result `written` or
`unobservable`; `uncaught_report_output_failure` carries result `failed` and
is legal only when the profile says output failure is observable. The two
terminal events are mutually exclusive, and no lifecycle/teardown event may
interleave between report-begin and its terminal result. The trace validator
checks the expected reporter result, exact reverse service-finalization order,
payload-destroy completion before heap leak-check/release, and the terminal
request after teardown. The independent supervisor record remains the
authority for the actual process result. On normal completion, emit the
corresponding entry-result, global-destruction, service-finalization,
heap-check/release, and termination events; fatal double-exception traces may
omit remaining cleanup events only after the declared fatal event.

The canonical event record uses a closed field set for each event kind:

| Event kind | Required semantic fields and cardinality/order rule |
| --- | --- |
| `root_control_initialize` | Executable-root ID and operation-site ID; exactly once before any failure-capable operation. |
| `heap_bootstrap` | Heap-domain ID and `ready`/`failed` result; once after root initialization. Failure is terminal for startup and forbids service/global initialization. |
| `runtime_service_initialize` / `runtime_service_finalize` | Canonical service ID, deterministic plan ordinal, and completion result; one initialize attempt per attempted service, and one reverse-order finalize only for each successfully initialized service. |
| `module_init_complete` / `module_ready` | Canonical module and initializer-node IDs; one completion per successful initializer and one `READY` transition only after all that module's initializers complete. |
| `entry_enter` / `entry_result_capture` | Canonical entry symbol ID and activation token; result event carries only the normalized `success`/`failure` category, not host status or arbitrary text. Entry is absent on startup failure. |
| `global_object_destroy` | Canonical global/type ID, test object token, completion result, and frozen initialization ordinal; only successfully initialized objects appear, in exact reverse completion order. |
| `fatal_double_exception` | Original exception identity/frame token and the secondary-failure site ID; exactly one terminal fatal event, with no secondary payload bytes. |
| `uncaught_report_begin` / terminal report event | Reporter paragraph ID, exception identity/frame token, and expected terminal category; one begin and exactly one matching `uncaught_report_complete` or `uncaught_report_output_failure`. Completion carries `written` or `unobservable`; failure carries `failed` only when observable by profile contract. |
| `exception_payload_destroy_begin` / `exception_payload_destroy_complete` | Original exception identity/frame and payload object token; one matching pair after reporting and before service/heap teardown. |
| `heap_leak_check` / `heap_release` | Heap-domain ID and declared check/release result; leak summaries use bounded counts only, never addresses or payload bytes. Emit the diagnostic leak check only when enabled and heap-ready; release occurs exactly once after payload/global destruction and service finalization when bootstrap succeeded. Heap-bootstrap failure emits neither event. |
| `process_termination_request` | Root ID, portable terminal category, and profile-pinned raw-status mapping when applicable; exactly once after required teardown, or directly after the terminal fatal event when cleanup is forbidden. The supervisor separately records observed process termination. |

Omitted, duplicate, out-of-order, cross-root, or contradictory required fields
fail trace validation. Optional fields are allowed only when declared by this
event schema version; unknown fields are not silently ignored.

Define a versioned, bounded trace format with a header binding the test-run ID,
test-case ID, instrumented artifact digest, instrumentation-site manifest
digest, fault-plan digest, and dedicated trace-transport ID/version and
descriptor digest. Event
sequence numbers are unique, contiguous,
and monotonic for the current single-threaded execution profile; every site
and boundary must exist in the bound manifest, and every object-token
transition must be legal under the lifecycle ledger. A normal completion
requires a terminal record. The independent process supervisor records the
actual exit status/termination category and compares it with the trace terminal
record; a generated trace claim alone never proves execution success. A
missing/truncated terminal record is `trace_incomplete`, except when the test
explicitly expects abnormal termination and the supervisor confirms that
result while the final complete event matches the declared fatal site. Timeout,
forced kill, malformed encoding, unknown event/site, non-contiguous sequence,
duplicate token transition, or digest mismatch cannot be downgraded to a
passing partial trace. Derive expected ledgers from an independent fixture
model and fault plan, not from the emitted trace or the instrumentation pass's
own manifest. If a profile permits concurrent generated execution, its trace
schema must identify execution contexts and assert specified happens-before
relations; it must not require one deterministic global event order where the
language defines none. Per-context activation tokens and event order remain
validated; cross-context interaction is checked only against declared
synchronization edges and shared-state guarantees.

Serialize the canonical trace as newline-delimited JSON records using the
`.cbi` canonical text rules: UTF-8 without a BOM, one fixed field order per
record kind, no whitespace outside strings except the LF record separator,
exact Unicode scalar sequences, and one LF separator. IDs/digests use
lowercase hexadecimal; sequence numbers,
counts, and lengths use canonical decimal strings with no leading zeroes except
`"0"`; byte payloads use lowercase hexadecimal with an explicit length. Reject
duplicate keys, unknown required fields, alternate spellings, overflow,
ambiguous delimiters, invalid lengths, and noncanonical escaping before
validating event semantics. Do not serialize COBOL `PIC` or `USAGE` storage
images, host endianness, native pointers, padded groups, or locale-formatted
numbers. If a profile's physical trace transport cannot emit canonical LF
records directly, its test adapter declares and hashes the raw transport, then
decodes it into these canonical records; retain both digests and never rewrite
the raw trace in place. The canonical header binds the encoding/schema version
and artifact/profile identities so evidence from different COBOL layouts
cannot be mixed. Cross-profile tests compare decoded semantic event records
and terminal categories, not profile-specific header digests or raw ABI
storage.

The trace observer itself is non-throwing test instrumentation. Reserve its
bounded storage from the declared test resource budget before entering the
fixture; it must not allocate through the application heap, call public
container/I/O APIs, or run user constructors/destructors. Every append checks
capacity before writing. On capacity exhaustion or sink failure, set a sticky
test-only trace-invalid flag and stop recording, but continue the exact
application operation and configured fault action; never convert observer
failure into an application exception, skip a fault, or suppress a fatal
termination. The harness rejects the run as `trace_overflow` or
`trace_sink_failure`, even if the generated program otherwise produced its
expected result. A test whose trace bound cannot be proven from its bounded
input/operation budget fails preflight or is split into smaller cases; it may
not silently drop events. Flush at normal completion and at designated fatal
events before termination where the profile permits; a failed or incomplete
flush invalidates evidence and never delays or changes the profile's fatal
termination result.

For each injectable boundary, run occurrence ordinals from one through the
observed operation count, plus one past the count to prove the plan is checked
and unmatched injections cannot pass. Assert separately that the intended
probe fired, the generated program observed the required exception/outcome,
and the post-failure state/trace satisfies the API guarantee. C++ compiler
failure-injection tests must report their own phase and test ID and cannot
count as proof that generated COBOL handled an application failure.

Expand each catalog bullet into separate cases with IDs of the form
`AREA-METHOD-CASE-NNN` (for example `STR-find-NOT_FOUND-001` and
`VEC-reserve-BAD_ALLOC_2-001`). Keep each ID linked to its source fixture,
expected typed failure/result, post-failure state assertions, target matrix,
and implementation. Parameterized tests may share code, but each boundary and
fault-injection position must still be reported as an individual case.

The runtime harness must fail the N-th allocation; make the N-th
constructor/copy/move/comparator/hash/file operation throw; inject a callee
exception; inject destructor failure for double-exception death tests; and use
fake filesystem/console/clock backends. For generated-program tests, those
fakes are test-only CBL-C modules or COBOL fixtures; C++ fakes may test
compiler-facing host adapters only and must never provide runtime results to a
generated executable. Counters/traces live in generated test instrumentation
state and are captured by the harness. Tests must not write or refresh
`automated_tests.log`; generated COBOL, executables, traces, and fixtures
belong in ignored or temporary directories. Tests leave tracked source/docs
unchanged.

Every test has a stable ID family: `LEX`, `PAR`, `SEM`, `TPL`, `ABI`, `EXC`,
`LIFE`, `MEM`, `STR`, one family per container, `ALG`, `IO`, `FS`, `PORT`, and
`TOOL`. No item may be skipped/disabled/xfail without a linked issue and an
explicit release decision.

### 8.2 Lexer, parser, semantic, template, and module tests

Create compile-only acceptance/rejection cases and assert diagnostic IDs and
source spans:

- **Lexing:** each supported literal/escape; unterminated string, character,
  block comment; invalid escape; malformed exponent/number; integer literal
  beyond each type limit; identifier at maximum length and one past; case-only
  distinct identifiers; reserved keyword in each declaration position;
  embedded NUL/invalid UTF-8 source policy.
- **Parsing:** minimal valid sample for every grammar production; missing/extra
  bracket, parenthesis, or brace; empty/malformed parameter/template lists;
  malformed `try`/`catch`/`throw`; `return`/`break`/`continue` in invalid
  context; duplicate/unknown class-member syntax; unsupported syntax gets a
  deliberate diagnostic, never a crash or late emitter failure.
- **Names/types/scopes:** undeclared identifier; duplicate local/member/type;
  private/protected access; ambiguous import; invalid pointer/reference
  conversion; const assignment; invalid return type; argument count/type
  mismatch; unsupported overload; uninitialized read; invalid array extent;
  statically detectable nullability violation.
- **Conversions/arithmetic:** every allowed narrowing/widening conversion;
  warning and warning-as-error; signed min/max and one-past; overflow on
  add/subtract/multiply/unary negate/abs; division by zero; min divided by -1;
  negative quotient/remainder; invalid shift count; float precision/range;
  NaN/infinity comparisons/formatting per policy.
- **Templates (`TPL`):** valid explicit, nested, and multi-type instantiation;
  same-key cache reuse and distinct-key separation; missing/extra/unknown or
  inaccessible type argument; failed `Copyable`/`Movable`/`Ordered`/`Hashable`/
  `Equatable` constraint; unsupported pointer/array shape; recursive
  instantiation cycle/depth limit; same name with different arguments;
  imported definition; diagnostic includes definition and use site; stable
  output independent of discovery order. Add the exact nested-type ladder and
  call-chain tests in Section 8.5, proving `vector<vector<string>>` and at
  least 16 nested generic constructors rather than treating depth four/five as
  representative.
- **Modules/ABI (`ABI`):** missing import; duplicate module identity; import
  cycle; public/private visibility; duplicate export; mismatched
  function/type/exception ABI versions; wrong result-slot order; missing or
  unexpected exception context; exception propagation across external entry;
  generated-name collision/reserved-word case; separate compile/link; embedded
  library load in a clean runtime environment.
- **Separate-compilation module gates:** each row is an independently
  reportable conformance test family; positive cases must use separate
  compiler invocations and serialized artifacts, not one shared in-memory
  context.

| Stable test ID | Fixture/failure injection | Required assertion |
| --- | --- | --- |
| `MOD-IDENTITY-001` | Resolve equal basenames under distinct roots, slash variants, case variants on each promised profile, relative-path aliases, and duplicate canonical IDs. | Distinct providers never alias; the declared normalization policy yields stable IDs; true duplicates fail with both source locations before exports are committed. |
| `MOD-SNAPSHOT-CONSISTENCY-001` | Mutate, replace, rename, add, and delete declared artifact-source and evidence-input files during discovery, byte capture, post-capture revalidation, and immediately after each snapshot freezes; include the artifact-producing pass-registry projection and validation-only pass descriptors; inject symlink/reparse traversal, case/separator aliases, concurrent editor saves, and repeated instability through the retry limit. | Only the complete sorted artifact-input closure—including the artifact-producing registry projection—and its exact staged bytes enter the artifact snapshot manifest and `generation_key`; the full pass-registry digest, validation plan/data, and external-tool identity enter the separate evidence-input manifest and attempt commit. An artifact-registry change updates generated identity; a validation-only registry change updates evidence identity without changing `generation_key`. Any change observed during either capture discards and retries that complete closure, then returns `stale_build_inputs` at its bound. Compiler passes read staged artifact inputs; external tools receive staged source/test inputs and are accepted only under the pinned tool identity recorded in evidence. A post-freeze source or test-data edit does not mutate either snapshot; a consumer requiring current inputs rejects mismatched digests. No partial snapshot or unstaged live source/test-data read can be cached or used as validation evidence. |
| `MOD-GRAPH-001` | Permute file discovery/import order; include missing, ambiguous, duplicate, direct-cycle, and transitive-cycle imports. | Graph and topological order are deterministic; missing/ambiguous/cycle diagnostics are distinct and include the full path; failure leaves no partial interface or output. |
| `MOD-INTERFACE-001` | Serialize and reload interfaces containing overloads, classes, fields, lifecycle methods, exception effects, and template bodies; truncate, corrupt, or change the format version. | Round-trip preserves structural signatures and source identity; invalid artifacts fail closed with an artifact diagnostic and are never partially registered. |
| `MOD-INTERFACE-SPAN-001` | Have the provider serialize declarations with exact half-open byte spans and coordinates from its frozen source snapshot; mutate source identity/hash/length, span endpoints, coordinate convention, and line/column values. Import once with the exact source bytes available and once with only the `.cbi`; then attempt source lookup with matching and mismatching hashes. | The provider rejects any span/coordinate not validated against its frozen source bytes before publishing the interface. A consumer with matching bytes rechecks the source hash and coordinates; without bytes it may validate only identity/hash/length and in-range offsets, must retain locations as unverified provenance, and cannot display source text or claim source-mapped diagnostics until matching bytes are obtained. Mismatched or out-of-range spans fail before atomic symbol registration; no-source availability alone does not prevent use of an otherwise valid interface. |
| `MOD-INTERFACE-LINK-HASH-001` | Publish a provider `.cbi` after Pass 8, compile a consumer against it before provider COBOL rendering, then render provider/consumer outputs and build the final link manifest. Mutate or omit provider source-implementation, generated-COBOL, `.cbi` integrity, and link-manifest hashes independently; repeat after a private provider-body edit. | The `.cbi` contains no generated-COBOL output hash; the consumer call descriptor binds the canonical provider symbol, source-interface identity, and ABI fingerprint. Provider COBOL may be rendered after consumer semantic compilation. Only the final artifact/link manifests bind the exact generated provider hash and ABI/profile to the link closure, and any mismatch fails before link. A private provider-body edit preserves source-interface/ABI compatibility and consumer semantic reuse, but changes implementation/output identity and requires relinking the new provider artifact. Integrity hashes, implementation hashes, and generated-output hashes are never substituted for one another. |
| `MOD-INTERFACE-CANONICAL-001` | Compare writer output against independent canonical-encoding/digest vectors; permute export/type/dependency discovery; mutate field order, whitespace, UTF-8/BOM, escapes, decimal-string encodings, duplicate object/array keys, required/optional extension fields, schema/hash domains, and each embedded digest. Exercise byte/depth/record limits and failures after parsing each artifact section but before publication. | Canonical semantically equivalent inputs produce byte-identical `.cbi` bytes and semantic hashes. Non-canonical bytes, duplicate/unknown-required fields, malformed references/digests, and resource-limit violations are rejected with artifact/field context. The reader registers either the complete validated export graph or none; a pre-existing consumer symbol table is unchanged on every failed read. The computed hashes match an independent implementation; integrity hashes are not reported as publisher authentication. |
| `MOD-BUILD-TRANSACTION-001` | Build a multi-module executable with reordered source discovery and worker counts; inject source mutation during snapshot, provider/interface/COBOL/link/test/manifest/commit failure, cancellation at each safe point, cache corruption, tool output escape, and two overlapping generations where the newer one fails. | Equivalent input snapshots yield identical module/link manifests and artifacts independent of worker completion order. A consumer never observes an unvalidated provider interface. Failed/cancelled generations publish no current-generation reference and preserve the prior successful build byte-for-byte; no partial/stale artifact is accepted from cache. Only the selected root's complete transitive closure is compiled/linked, tool evidence is bound to exact inputs, and orphan cleanup cannot remove artifacts reachable by any retained successful generation. |
| `MOD-COMMIT-CANCEL-001` | Request cancellation in each coordinator state, while waiting for a publication lock, after generation-directory binding, after attempt-commit payload staging/verification, after immutable attempt-record publication, immediately before reference replacement, during the adapter replace call, and after replacement but before the caller receives the result. Attempt to enter `prepared` with a missing, incomplete, or digest-mismatched staged record; inject a replace error whose commit result must be recovered by rereading the reference, plus an unreadable/malformed reference. | `prepared` is reachable only after all output/evidence validation and the complete staged commit-record payload pass verification; staging it alone never makes the record visible to readers. Pre-swap cancellation leaves the old reference current; a generation or immutable attempt record already published but not referenced remains an unreachable orphan, never a success. A successfully replaced reference reaches lifecycle state `committed` and returns public `terminal_state = success` even when cancellation arrives in flight. A failed replace is classified only after validating old/new references; an indeterminate or malformed state yields `commit_recovery_required`, preserves all candidate generations, and blocks readers/GC/publication until safe recovery. A failed/cancelled attempt is never reachable through the current reference or reported committed, and startup never chooses by timestamp or enumeration. |
| `MOD-EVIDENCE-BINDING-001` | Build one artifact generation, then validate it in two attempts with distinct test-run IDs, evidence outputs, validation-plan versions, and full pass-registry digests; include a validation-only registry change that leaves the artifact projection fixed. Also replay stale evidence against changed artifacts and corrupt the attempt commit record/reference. | Identical artifact-producing inputs retain the same `generation_key`, deterministic artifact manifest, and artifact directory even when evidence digests differ. Each commit record binds one exact artifact-manifest digest, validation-plan/input digest, full pass-registry digest, and evidence set; the current reference atomically identifies both records. Evidence cannot be reused for changed artifacts, plan, or registry, and validation-only changes rerun required gates without producing `nondeterministic_build_output` for unchanged artifacts. |
| `MOD-PUBLISH-ATOMIC-001` | Use a fault-injecting filesystem adapter to fail before/after staging and flushing the commit-record payload, verifying it, acquiring the lock, binding the generation key, publishing the immutable attempt-commit record, replacing the current reference, acquiring a lease, marking retirement, and deleting. Simulate process termination and restart at each durable boundary. Race identical attempts, a same-key/different-artifact-output attempt, different source snapshots, a stale slow attempt, a reader acquiring a lease while GC retires its generation, active readers, cancellation, and a live/stale lock owner; repeat on each required Windows/POSIX local filesystem and an intentionally unsupported filesystem. | Readers observe either the prior complete artifact-and-evidence commit or the new complete commit, never a partial reference/artifact set; an attempt-commit payload in staging is never reader-visible. Recovery after restart validates and retains the prior reference when replacement did not complete, or validates the complete artifact generation, immutable commit record, and evidence when it did. Failure before record publication leaves no published record; failure after its publication but before reference replacement leaves an immutable unreferenced orphan, while the previous reference remains current. One generation key binds to one immutable deterministic artifact output; same-key/same-artifact binding is idempotent, while evidence may differ only through a new attempt commit record. Same-key/different-artifact output fails as nondeterministic without creating a second binding. Stale snapshots cannot replace a reference committed from newer inputs. Lease acquisition and retirement are mutually exclusive: GC never deletes a generation or evidence held by a live reader, and a losing reader retries. No live lease/owner artifact is reclaimed, no lock is stolen by age, and unsupported atomicity/path semantics fail closed. |
| `MOD-HASH-001` | Change whitespace, private implementation, public signature/ownership, profile-generated entry name, source-pinned external name, ABI layout, exception payload, public template body, compiler/schema version, and target profile independently; corrupt only the serialized artifact-integrity digest. | Artifact-integrity hash detects any serialized payload mutation. Source-interface hash changes for source-contract or public template-body/reference changes and stays stable for profile-only physical spelling/layout changes; it changes for an explicit source-level external-name declaration. ABI fingerprint changes for physical layout/name/profile/ABI-version changes, including a source-pinned name's resolved physical spelling; template and private implementation hashes change only for their respective bodies. A profile-specific generated entry-name change affects the serialized artifact-integrity hash and ABI fingerprint, not the source-interface hash. Whitespace, host paths, timestamps, source discovery, and private names do not perturb semantic hashes. Consumers invalidate by the changed dependency hash; ABI compatibility still requires version and field-by-field descriptor checks, not hash equality alone. |
| `MOD-ABI-001` | Mutate each provider/consumer descriptor field independently: receiver presence/type/access/projection/lifecycle state, argument type/order/mode, return slot, context slot, throw set, payload, ownership, lifecycle metadata, interface/ABI version, and profile. | Every mismatch is rejected before rendering/linking; compatible fields match exactly; unknown imported effects are conservative rather than inferred non-throwing. |
| `MOD-LINK-001` | Emit unresolved/duplicate external names, duplicate `PROGRAM-ID`, missing runtime dependency, and a complete provider-consumer-runtime closure; permute module discovery and parallel completion. | Every call resolves exactly once by canonical symbol ID; duplicate or missing targets fail before COBOL link; the manifest lists exact module IDs, symbols, profiles, provider artifact hashes, and transitive runtime dependencies in deterministic order. Link planning never chooses by basename or filesystem enumeration. |
| `MOD-NAME-ABI-001` | Compile a provider in an isolated invocation, serialize its interface/name table only after Pass 8, then compile consumers in separate invocations; vary consumer order, add unrelated providers, relocate the checkout, and inject two providers with colliding frozen external names. | The provider's exported `PROGRAM-ID`/entry spelling and ABI fingerprint are finalized together before interface publication, identical in every consumer build, and come only from the provider interface. Consumers emit the exact frozen spelling for the resolved symbol ID. Adding an unrelated consumer does not rename/recompile the provider. A true link-global collision fails with both module/symbol IDs unless an explicit ABI-compatible alias contract permits it; no silent rewrite or wrong-provider call occurs. |
| `MOD-TEMPLATE-001` | Instantiate an imported body in two consumers; omit or privatize the body; request duplicate/conflicting explicit concrete exports; create recursive and depth-limited dependencies. | Visibility and body availability are checked before substitution; consumer-local specialization names are disjoint and deterministic; an explicitly shared specialization has exactly one owner; failures precede COBOL emission. |
| `MOD-LIFETIME-EXCEPTION-001` | Construct/copy/move/destroy a managed class in a provider, throw across a module boundary, catch/clean in a consumer, and inject constructor/payload failure. | Exact lifecycle counts and exception payload/context survive the ABI; cleanup runs once; incompatible lifecycle/exception descriptors fail at link planning; a second exception still takes the fatal path. |
| `MOD-CACHE-001` | Compare a clean build, cache hit, private-body edit, public-interface edit, cache corruption, parallel discovery, and checkout relocation. | Valid cache hits reproduce artifact hashes, source diagnostics, and behavior; required dependents rebuild for changed interfaces while unrelated semantic results remain reusable; corrupt/stale entries are rejected and recomputed from frozen source; output identity does not contain host paths or timestamps. |
| `MOD-CACHE-STORE-001` | Race writers for identical and deliberately divergent outputs under one full cache key; terminate writers after each temp-write/flush/bind step; read truncated, stale-schema, wrong-profile, tampered-digest, and path-escape entries; evict during active reads; inject read-only/full/unavailable stores. | No partial entry is visible. Identical writers converge on one immutable verified record; divergent output fails as `nondeterministic_build_output` without choosing a winner. Invalid entries are never registered and trigger a clean recompute; a cache failure does not alter source diagnostics, generated outputs, or success semantics, while output-store failure remains fatal. Eviction is a miss, never deletes a live reader's data, and telemetry/storage paths do not affect semantic hashes. |
| `MOD-PROFILE-001` | Compile and link the same serialized provider/consumer pair under each supported compiler/profile and attempt cross-profile artifact mixing. | Matching-profile artifacts compile/link/execute as specified; unsupported or mixed profiles fail during planning; no untested profile is reported as portable. |
| `MOD-INIT-EFFECT-001` | Compile providers whose exported functions directly and transitively read, write, construct, allocate, throw, and call other local/imported functions; include dynamic globals in multiple modules, harmless effect-free calls, a dependency cycle, an opaque external call, and a versioned external effect contract. Inspect generated `.cbi` records. Mutate summary schema/state, global/callee IDs, each effect set, exception-known state, implementation-closure digest, source snapshot, and `link_effects_hash`; test missing summaries, `unknown` paired with empty arrays, and stale summaries after a private body edit. | The provider independently verifies each `complete` summary against typed call/effect closure and binds it to exact implementation inputs. The linker rejects missing, stale, malformed, or `unknown` effects reached by a dynamic initializer before rendering; provider-first order cannot make them acceptable. Known global accesses create deterministic initialization edges, cycles fail with their full path, and the root error record is usable even when heap bootstrap fails. A private effect-only edit changes link-effect/link-plan identity and relinking requirements, but preserves source-interface and ABI hashes; public contract or physical ABI changes invalidate their corresponding hashes separately. Failed `.cbi` validation publishes no partial exports or effect metadata. |
| `MOD-EXCEPTION-TYPE-INTERFACE-001` | Serialize/import `.cbi` records for exported exception declarations, generic concrete exception types, direct-base edges, throw sets, payload descriptors, and lifecycle owners across independent compiler processes. Mutate or omit each identity/schema/runtime-ID/hierarchy/payload/owner field; include duplicate and stale records, same-spelled distinct types, inaccessible/ambiguous bases, and forced dispatch-ID collisions. Re-seal semantic mutants and recompute canonical hashes independently. | The reader validates the complete exception type graph before atomically registering any symbols. `source_interface_hash` changes for public identity/hierarchy/throw-contract changes; the physical `abi_fingerprint` changes for runtime-ID encoding/version, payload layout, lifecycle linkage, or context ABI changes; integrity digest always binds the exact bytes. Link planning rejects missing providers, conflicting definitions, collisions, and inconsistent base projections before generated dispatch. Failed ingestion leaves the consumer symbol/interface graph unchanged; valid separate-process imports produce identical type records and catch behavior regardless of module discovery order. |
| `MOD-EXTERNAL-ADAPTER-INTERFACE-001` | Publish an external dependency from one compiler process and import it from another. Cover each declared call mode, all mapped status values plus unknown status, distinct call-availability handling, per-status `out`/`inout` validity, ownership/effect summaries, callback symbol/signature, synchronous/asynchronous mode, registration lifetime, invocation cardinality/order, argument alias/ownership, re-entry context, callback failure translation/continuation, exception-aware context/type/payload/lifecycle records, and each contract basis. Independently mutate or omit every descriptor field, authority/contract ID/version/content digest, verification or attestation reference, trust policy, source-contract/physical-ABI/effect category, adapter/schema version; create duplicate IDs or contradictory mappings and re-seal semantic mutants. | The reader atomically accepts only complete, internally consistent descriptors. Importers reproduce the exact call mode, frozen symbol spelling, ABI/effect/exception contract and cannot infer non-throwing behavior from absence of a status field. A plain content hash is treated as integrity only, never publisher authentication; a trusted assertion is accepted only under the declared authority/policy, and an unverified or missing provenance cannot be upgraded to compiler-verified. Hash classification is field-specific: public call/failure semantics change the source-interface contract, physical layout/context/exception representation changes the ABI fingerprint, private implementation-effect changes update link-effect identity, and trust/attestation evidence follows the separately versioned evidence policy. Incompatible or unknown-required records fail before symbol registration, rendering, or link; failed import leaves the prior consumer interface unchanged. Independent processes and link-order permutations yield identical normalized descriptors and diagnostics. |
- **Generated output:** follow the structural, ABI, dialect, compiler,
  execution, and validator-negative-test plan in Section 8.7; assert
  deterministic output, source-map positions, and rejection of unsupported
  COBOL constructs.

### 8.3 Exceptions, cleanup, and ownership tests (`EXC`, `LIFE`, `MEM`)

- Throw/catch each built-in exception type; exact, accessible base, and catch
  all; source-order selection; inaccessible/private base does not match; catch
  by value and supported const-reference; nested try/catch; rethrow preserves
  dynamic type/payload/location; rethrow outside a handler is fatal as
  specified.
- Propagation from direct throw, function, method, constructor, imported
  module, standard-library method, and external-entry adapter; result slot is
  not consumed after throw.
- For every exit (`}`, `return`, `break`, `continue`, exception, failed
  initialization), assert reverse completion-order destruction exactly once;
  repeat for nested scopes, branches, loops, early returns, shadowed locals,
  and temporaries.
- Partial construction: throw at every base/member/array-element constructor
  position; destroy only completed subobjects, in reverse order; do not invoke
  enclosing destructor before full construction.
- Throwable payload matrix: for each 1.0-permitted payload type, throw, copy or
  move it into the exception context according to the specified rule, catch it
  by reference and by value where supported, rethrow it, and destroy it after
  handling. Cover scalar, string, nested managed fields, fixed arrays, owning
  pointer fields, non-trivial user-defined destructors, and nested generic
  values. Assert type ID, ABI version, payload size/layout, exact lifecycle
  calls, and no raw-byte fallback. For any explicitly excluded shape (for
  example multiple inheritance if not selected for 1.0), assert a stable
  compile-time diagnostic instead of partial lowering.
- Exception module ABI: separately compiled thrower/catcher pairs agree on
  payload type ID, layout/size, ownership operations, and ABI version; reject
  mismatches before link/run. Inject failure while constructing/copying the
  payload and assert the source object remains valid and the partially built
  context is cleaned without replacing the active exception.
- Nested-container failure propagation: for every depth in the 1..16
  conformance ladder, fail each outer/inner allocation and each child
  construct/copy/move after every possible completed element prefix. Assert
  the original source and strong-guarantee destination remain unchanged,
  staged child values are destroyed exactly once from deepest completed leaf
  outward, the original exception type/payload survives propagation, and no
  outer cleanup calls a child destructor for an unconstructed element.
- Destructor cases: normal; nested managed fields; arrays; moved-from object;
  compile-time rejection if destructor can throw; injected destructor failure
  during normal exit follows its specified termination policy.
- **Double-exception abort:** while exception A is constructed, copied,
  propagated, matched, or cleaned up, inject B from constructor, copy/move,
  helper call, and destructor paths. Each death test asserts immediate
  termination, A/B locations where available, no handler for B, no replacement
  of A, no normal continuation, and the exact profile-normalized fatal result;
  retain raw host exit/signal data separately.
- Unique ownership: destruction at scope exit/reset/replacement; move transfers
  and empties source; copy rejected; self-move safe; `release` transfers
  responsibility; null dereference throws defined error; wrong delete family
  rejected; detectable use-after-move/borrow escape rejected.
- Shared/weak ownership: copy/decrement counts; exactly-once last-owner
  destruction; weak does not own; live/expired `lock`; reset/swap; unique-to-
  shared conversion; nested/cross-module destruction; cycles/weak-edge break;
  failure allocating object/control block leaks nothing. If threads are in
  scope, race increments/decrements/lock/destruction; otherwise test the
  unsupported-thread boundary.
- Allocation (`MEM`): fail N-th allocation at every site—arrays, control
  blocks, exception payloads, strings, container nodes—and assert `bad_alloc`,
  no leaks, and each operation's strong/basic guarantee.

### 8.4 `string` method and failure tests (`STR`)

Each public method gets ordinary, empty, minimum/maximum size, one-past
boundary, self-aliasing, every allocation-failure point, and cleanup-after-
failure cases. Add these specific tests:

- **Construction/lifecycle:** default empty; empty/ASCII/multibyte valid UTF-8;
  length-delimited embedded U+0000 accepted as valid text while NUL-terminated
  construction stops at the first NUL; invalid continuation/overlong/surrogate/
  truncated input -> `invalid_encoding`; above `max_size` -> `length_error`;
  allocation -> `bad_alloc`; copy deep-independence; move empties source;
  copy/move self-assignment; destructor releases storage once.
- **Length/capacity:** `len`/`byte_length` ASCII and multibyte; codepoint count;
  `max_size` is stable for the configured target profile; capacity invariant;
  reserve below/equal/above; shrink empty/nonempty; clear
  twice; resize smaller/larger/zero; invalid fill byte; capacity overflow;
  allocation failure preserves documented content/capacity guarantee.
- **Access:** `at` first/last/continuation-byte (returns that exact byte),
  negative, `len`, huge index;
  `codepoint_at` first/last/negative/one-past;
  empty front/back;
  valid byte set; continuation/invalid codepoint replacement; codepoint-index
  boundaries; empty begin/end traversal and stale borrow/iterator detection.
- **Mutation:** `assign` invalid encoding/too long/allocation -> respectively
  `invalid_encoding`/`length_error`/`bad_alloc`; append and `+=` include
  self/prefix/suffix aliasing and combined-size overflow; `operator+` returns a
  new value without changing either operand; `push_back` rejects non-ASCII
  bytes with `invalid_encoding`, while `push_codepoint` rejects surrogates and
  values above U+10FFFF with `invalid_encoding`; empty `pop_back` ->
  `out_of_range`; `insert`/`erase`/`replace` test outside-range offsets
  (`out_of_range`) separately from in-range mid-codepoint offsets
  (`invalid_argument`); `replace_all` no match/adjacent/overlapping matches,
  empty old pattern -> `invalid_argument`, and result-size overflow;
  `remove_prefix`/`remove_suffix` count greater than `len` -> `out_of_range`
  and mid-codepoint count -> `invalid_argument`. Every failed mutation asserts
  original bytes and length are unchanged.
- **Search/slicing:** find/rfind start/end/repeated; start at length; empty,
  absent, and longer needle; invalid start; UTF-8 boundary violation;
  `substr(0)`, `substr(len)`, zero count, count past end, invalid position;
  resulting slice valid. Not-found is empty optional, not error/status.
- **Compare/predicates:** equal/unequal lengths; lexical less/equal/greater;
  prefix/suffix true/false/empty operand; contains empty/present/absent; ASCII
  case comparison and non-ASCII bytes without false Unicode folding.
- **Trim/split/join:** whitespace-only, leading/trailing/mixed/empty; exact
  ASCII whitespace set; empty-separator policy; separator at ends/repeated/
  absent; join zero/one/many items; empty items/separator; exact output bytes
  and codepoint counts.
- **Case/encoding:** upper/lower ASCII letters, digits, punctuation, empty,
  non-ASCII behavior; UTF-8 validity after truncation/append; malformed input
  throws `invalid_encoding`.
- **Parsing:** sign/whitespace policy; empty/sign-only; leading zero; embedded
  spaces; trailing junk; decimal/exponent; invalid punctuation; precision
  edge; integer min/max and one-past; long input; negative zero;
  overflow/underflow; NaN/Inf policy. Assert exact value or `parse_error`/
  `range_error`, never accidental zero-success. Prefix parser asserts consumed
  byte offset.
- **Formatting:** zero/sign/min/max/fraction/rounding tie/precision/exponent
  boundaries; locale change; round-trip where promised; deterministic output.
- **Interop:** `to_c_string` terminator/empty/allocation/release; embedded U+0000
  is valid in `string` but makes `to_c_string` throw `invalid_argument`;
  bounded copy exact fit/too-small/zero/invalid capacity/overlap; no partial
  write on strong-guarantee error.
- **Legacy adapters:** each retained `atoi`/`atol`/`atoll`/`strtod`/`strcmp`/
  `strcpy`/`strncpy`/`memcmp`/`strcat`/`toupper`/`tolower` forwards to class
  implementation or an explicitly pinned conversion shim, has behavior-parity
  tests, and exposes no meaningless status. Strict `string::parse_*` methods
  must throw typed parse/range errors; if an old ABI preserves zero-on-malformed
  behavior, only that versioned compatibility symbol may translate the error
  to its legacy result, and the test must identify it as compatibility rather
  than safe parsing.

### 8.5 Per-container method and invariant tests

For every method in Section 4.2.1, test success, empty/default, boundaries,
invalid arguments where applicable, throwing element/comparator/hash,
allocation failure, post-failure state, and iterator/reference invalidation.
Run lifecycle cases with `int`, `string`, and a counted RAII type. Assert a
compile-time diagnostic when element capabilities are absent.

#### Vector lifetime and contract migration (`VEC-LIFE`, `VEC-API`)

Do not treat the current vector tests' permissive behavior as the 1.0 oracle.
The old clamping/no-op inputs become explicit rejection tests for the native
API; keep old-result assertions only for a separately named compatibility
adapter if one is actually required. Add the following independent cases so
allocation capacity, logical size, and object lifetime cannot be conflated:

| Case family | Operations and injected condition | Required evidence |
| --- | --- | --- |
| `VEC-LIFE-RESERVE` | `reserve` empty/populated; capacities below/equal/above size; repeated reserve; fail each allocation/copy/move position. | Capacity changes without constructing unused `T`s; successful relocation records one copy/move construction and one old-object destruction per live element, leaving the same net live-object count; staged elements clean up exactly once and old vector remains intact on failure. |
| `VEC-LIFE-GROW` | `resize`, `push_back`, `emplace_back`, `insert` with spare/new capacity; throw at every new element constructor. | Only requested/new elements are constructed; size advances only after successful construction; failure removes staged tail and restores original size/content. |
| `VEC-LIFE-REMOVE` | `pop_back`, erase first/middle/last/range, shrinking resize, `clear`, repeated clear, destructor. | Removed live elements receive exactly one destructor call; shifts use the required move construction/assignment; no destructor runs for unused capacity or already-destroyed slots. Bulk destruction order is reverse construction order. |
| `VEC-LIFE-COPY-MOVE` | Copy/move construct/assign, self-copy/self-move, aliasing push/insert, source and destination at empty/single/many sizes. | Deep independence, valid empty moved-from source, exact owner count, no duplicate release; assignment commits safely and preserves destination on a failed staged copy. |
| `VEC-LIFE-PARTIAL` | Fail N-th allocation and each constructor/copy/move in construction, copy, assign, insert, and reallocation. | Constructed-prefix bitmap/count matches the trace; cleanup touches only fully constructed slots and runs once in reverse order; old vector unchanged where strong guarantee is promised. |
| `VEC-API-INDEX` | `at` and `operator[]` at `-1`, first, last, `size`, and large index; read and mutation forms. | Both public forms are checked; invalid access throws `out_of_range`; no access or mutation follows the failure; generated signatures use the specified reference/borrow form, never a raw pointer result. |
| `VEC-API-INSERT-ERASE` | Insert at `0`, middle, and `size`; insert at `-1` and `size+1`; erase first/middle/last/range, empty, `-1`, `size`, and invalid iterator. | End insertion succeeds; all other invalid positions throw `out_of_range` (invalid/stale/foreign iterator -> `iterator_error`) and leave values, size, capacity, and lifecycle counts unchanged. |
| `VEC-API-NEGATIVE-SIZE` | Negative resize/assign; zero; valid positive; above `max_size`; arithmetic overflow in growth. | Negative domain input -> `invalid_argument`; above-limit/unrepresentable size -> `length_error`; no request silently becomes zero and the original vector remains unchanged. |
| `VEC-ENCAPSULATION` | Attempt direct source access/mutation of backing pointer, count, capacity, and relocation scratch fields. | User code receives the specified access-control diagnostic; no generated public member exposes mutable storage bookkeeping. Test-only counters use a separate instrumentation path. |

The lifetime matrix runs first on a counted non-trivial leaf, then on `string`,
then on `vector<string>` and nested vectors. It is repeated after the Phase 5
string representation change and Phase 6 iterator/reference implementation.
Every successful call-count assertion is cross-checked against a construction
ledger and live-allocation count, not inferred from final values alone.

#### Nested generic instantiation and call-chain proof (`TPL-NEST`, `LIFE-NEST`)

This is a required conformance suite, separate from ordinary per-container
tests. It must prove nested specialization, not just observe that a few values
print correctly.

Define `V0 = string` and `Vd = vector<V(d-1)>`. Generate one source fixture at
each depth `d = 1..16`. Every depth gets a stable individual case ID
`TPL-NEST-DEPTH-01` through `TPL-NEST-DEPTH-16`; each fixture must parse,
type-check, instantiate, emit COBOL, compile with every required profile,
execute, traverse all `d` vector layers, and verify one deeply nested leaf
value. Run the full lifecycle/error suite at representative depths
1, 2, 3, 4, 5, 8, and 16 once the later exception/lifetime phase is complete;
the Phase 3 gate first runs the positive lifecycle cases at those depths, and
Phase 4 adds the failure campaign to the same fixtures. The other depths still
get compile/link/run smoke coverage so an implementation that happens to work
only at selected depths cannot pass. A 17-layer type must fail in the default
16-layer profile with `TPL_NESTING_LIMIT`, include all instantiation frames
from use site to leaf, and produce no partial generated program. The source
parser must separately accept tightly nested closing
`>` tokens and whitespace-separated closers, while preserving the meaning of
the `>>` shift operator in expression context.

For each tested depth, assert the canonical specialization tree and emitted
call edges. There must be one concrete lifecycle specialization per distinct
type key, no duplicate generated routines, no unresolved placeholder, and no
runtime type-erased dispatch. Reordering source discovery or instantiation
requests must leave names, call graph, and generated COBOL semantically
identical. Reusing a previously instantiated child type must reuse its exact
specialization. Deep valid nesting must not be mistaken for an expansion
cycle; a true non-terminating template expansion must be diagnosed before
emission.

Use the following named behavior fixtures at minimum:

| Case | Source-level action / expected dispatch | Required assertions |
| --- | --- | --- |
| `TPL-NEST-ACCESS-001` | Build `vector<vector<string>>`; evaluate `vv.at(i).at(j).append(s)`. | Trace is outer `at` -> inner `at` -> `string::append`; both `at` calls return borrows, with zero copies/moves of intermediate containers or target string. Correct final bytes; append error propagates unchanged. |
| `TPL-NEST-DEEP-CHAIN-001` | At each representative depth, chain exactly `d` checked `.at(index)` calls through `Vd` to the leaf `string`, then mutate it. | The depth-16 expression parses, resolves, lowers, compiles, and executes; trace has exactly 16 concrete receiver specializations followed by one string call; no intermediate copy, skipped layer, wrong-depth call, or truncation of the call chain. |
| `TPL-NEST-INDEX-001` | Evaluate `vv[i][j]` and mutate/assign the resulting `string`. | Resolve each checked `operator[]` from the static receiver type; exact target identity changes; one string assignment if assigned; no vector copy. Out-of-range outer index prevents inner indexing; out-of-range inner index prevents the final string call. |
| `TPL-NEST-APPEND-001` | `vv.emplace_back()` followed by `vv.back().emplace_back(existing_string)`. | First step constructs one empty inner vector and zero strings; second step constructs exactly one string through the inner vector's element lifecycle. Check return/reference chaining does not materialize a copy. |
| `TPL-NEST-PUSH-COPY-001` | Copy-push an existing `vector<string>` into `vector<vector<string>>`. | Outer `push_back` selects the nested vector copy constructor; that copy iterates live strings and calls string copy construction. Source values unchanged; exact copy/destructor counts. |
| `TPL-NEST-COPY-001` | Copy a populated `vector<vector<string>>`. | Outer copy routine -> one inner copy routine per element -> one string copy routine per leaf, in index order. Mutating copy does not affect source. Inject failure at each child/leaf and assert strong guarantee and reverse cleanup of only completed staged elements. |
| `TPL-NEST-MOVE-001` | Move-construct/assign populated nested vectors, including self-move assignment. | Each vector layer follows its published move policy; storage-stealing moves invoke no child copies, source at each moved-from public boundary is valid/empty, and no resource is duplicated. Self-move leaves one valid owner. |
| `TPL-NEST-REALLOC-001` | Grow/reallocate each outer level while elements are populated. | Generated code uses the selected child copy-or-nothrow-move operation, never byte-relocates `vector`/`string`; staged values are committed once; old values are destroyed once only after commit. Inject failure at each allocation and element index. |
| `TPL-NEST-ERASE-001` | Erase, clear, shrink, and leave scope at each depth. | Destroy only live elements, each exactly once, reverse construction order at every level; free each owned allocation once; stale borrows/iterators fail per invalidation rules. |
| `TPL-NEST-ERROR-001` | Fail outer `at`, inner `at`, leaf mutation, and each nested allocation in separate runs. | Exact error category/code and source location; later calls do not run after an earlier failure; cleanup trace is complete; no wrong-depth handler catches or overwrites the error. |
| `TPL-NEST-TRACE-001` | Add side-effecting argument factories to chained method/operator expressions. | Each operand is evaluated exactly once in the specified language order; overload resolution selects the concrete receiver method; no implicit copy/move/conversion occurs beyond the semantic rules. |

The runtime trace for `vector<vector<string>>` must make the call path
observable in a test build. For example, outer destruction emits
`destroy<vector<vector<string>>>`, then for each live child in reverse order
`destroy<vector<string>>`, then for each live leaf in reverse order
`destroy<string>`, then the two corresponding storage releases. Compare the
trace against the construction ledger, including element indexes, and assert
that partially constructed levels contain exactly their completed prefix.

Also test a mixed wrapper chain such as
`vector<array<optional<string>, 2>>` and a nested associative/value chain such
as `map<string, vector<unique_ptr<string>>>` wherever the relevant 1.0 types
are supported. For every generic container family, instantiate it directly
with `string` and with at least one non-trivial container/value type as its
element or mapped value, when allowed by that family's key/value constraints.
These tests catch assumptions that only hold for `vector` or scalar elements.

The test harness must use a non-copyable counted RAII leaf as well as `string`
to prove that relocation obeys type capabilities, and a throwing counted leaf
to inject construction/copy/move/comparator failures at every nesting level.
For failures, record (1) the first failing depth/index, (2) the exception
identity and payload, (3) successful construction prefix at each ancestor,
(4) reverse cleanup trace, (5) live allocation count, and (6) source and
destination snapshots. This is mandatory at depths 4, 5, 8, and 16, not merely
at depth 1.

- **`array<T,N>`:** N=0/1/many; size invariant; all elements initialized and
  destroyed; copy/move/self-assignment; at negative/last/one-past; empty
  front/back; data/iteration; fill; self/other swap; equality; constructor/copy
  throw.
- **`vector<T>`:** default/count/value/range; zero/one/many; size/capacity/max/
  empty; reserve smaller/equal/larger/overflow; shrink empty/nonempty; resize
  grow/shrink/same; clear twice; assign zero/nonzero/own overlapping range;
  at and operator[] policy; empty front/back; begin/end; push/emplace with and
  without reallocation; empty pop; insert begin/middle/end/invalid; erase
  first/middle/last/whole/invalid; swap; copy/move/self-assignment; aliasing
  push/insert; reallocation invalidation; failure at each allocation and
  element-construction position; strong guarantee and exact destruction of
  old/new blocks; `vector<string>` and nested managed types.
- **`deque<T>`:** empty/single/many; push/pop both ends; alternating-end growth;
  access boundaries; insert/erase every position; copy/move/swap; empty access/
  removal errors; block and element failure; documented end/middle invalidation;
  no contiguous-data assumption.
- **`list<T>`:** empty/single/many; front/back; forward/backward traversal;
  insert/erase each position and invalid/end iterator; push/pop ends;
  remove/remove_if none/some/all; clear twice; reverse/sort duplicates if
  provided; copy/move/swap; allocation failure/node cleanup; non-erased
  iterator/reference stability.
- **`forward_list<T>`:** empty/single/many; before-begin/begin/end; push/pop;
  insert_after/erase_after before/at/after end and invalid iterator; remove_if;
  reverse/order; size behavior/complexity; copy/move/swap; node failure and
  exact destruction.
- **`set`/`multiset`:** empty/single/duplicate/many; ascending order; comparator
  equivalence distinct from equality; insert/emplace result; erase key/iterator/
  range; find/contains/count; lower/upper/equal range below minimum/present/
  gaps/above maximum; clear/copy/move/swap; comparator throw; node OOM; tree
  invariants after rotations; uniqueness/equivalent counts; non-erased-node
  stability.
- **`map`/`multimap`:** ordered-set tests; insertion/access/update; key
  immutability; missing `at` throws `out_of_range`; `operator[]` hit/miss/default
  construction; `insert_or_assign`; `try_emplace` avoids mapped construction
  on hit; duplicate semantics; mapped lifetime; key/value constructor failure
  separately; comparator throw; OOM; `pair<const K,V>` assignment constraints;
  rollback and tree invariants.
- **`unordered_set`/`unordered_multiset`:** empty/collision-heavy/distributed
  keys; equivalent keys hash equally; unique/multi duplicates; find/count/
  equal_range; erase; reserve/rehash same/smaller/larger; zero/invalid load
  factor; buckets/load factor; rehash invalidation; hash/equality throw during
  lookup/insert/rehash; bucket/node OOM; state guarantee; exact node destruction.
- **`unordered_map`/`unordered_multimap`:** unordered-set tests plus key/value
  insertion, immutable key, `at`, `operator[]`, `insert_or_assign`/
  `try_emplace`, mapped lifetime, rehash behavior, collision-heavy sequences,
  hash/equality consistency and exception cases.
- **`stack<T>`:** empty/size/top; push/emplace/pop LIFO; empty top/pop errors;
  copy/move; underlying allocation/element failure; swap and RAII.
- **`queue<T>`:** FIFO order; front/back after each operation; push/emplace/pop;
  empty errors; wraparound if ring implementation; growth failure; copy/move,
  swap and RAII.
- **`priority_queue<T>`:** min/max comparator; duplicates/ties; heap invariant
  after push/emplace/pop; empty top/pop errors; comparator throw; allocation/
  element failure preserves invariant; copy/move/swap.

### 8.6 Utility types, memory, algorithms, and hosted services

- **`pair`:** default/value construction; each member's copy/move/destruction;
  assignment/self-assignment; swap; equality/order capability acceptance and
  rejection; immutable map-key behavior.
- **`optional`:** empty/value; reset/emplace/assignment transitions; checked
  value/dereference while empty throws `bad_optional_access`; `value_or` both
  branches; managed payload destroyed once; failed emplace state; copy/move and
  moved-from validity.
- **`result`:** success/error construction and inspection; wrong-alternative
  access throws named error; error cannot be silently discarded where checking
  is promised; map/and_then propagation if provided; payload lifecycle and
  failing transformation.
- **`variant`:** each active alternative; switching destroys old once; failed
  construction state; index/holds/get; wrong alternative throws
  `bad_variant_access`; copy/move/swap; visitation only if implemented.
- **Algorithms:** empty/single/repeated/sorted/reverse/duplicate input per
  algorithm; already-satisfied/no-match; iterator capability acceptance and
  rejection; invalid/reversed/cross-container ranges; comparator/predicate
  throws; partial-mutation guarantee; copy/move overlap; sort stability if
  promised; exact result and complexity operation-count checks.
- **Allocation/new/delete:** scalar/array pairing; zero extent policy; size
  multiplication overflow; `bad_alloc`; constructor failure at each array
  element; partial array cleanup; wrong delete form rejected where statically
  known; no leak/double free.
- **Errors/formatting/math/random:** every documented invalid argument,
  domain/range/overflow/underflow, malformed format, bad conversion, seed edge,
  and deterministic-sequence guarantee; exact exception/result error and
  state/output guarantee.
- **Files/console/filesystem (`IO`,`FS`):** missing path/permission/invalid
  path; already-open/closed handle; empty/EOF/partial read; full device/disk
  exhaustion/partial write; flush/close failure; invalid record length/seek;
  binary NUL; text newline conversion; directory absent/not-directory/permission;
  create/remove/rename collision; root/relative/`.`/`..`/separator/Unicode
  paths; file changes between query/use. Assert portable error category; native
  code is diagnostic only. Assert exact portable subcode, transferred
  byte/record count, handle state, and file position after failure.
- **Date/time/process/environment:** invalid date/leap day/range overflow;
  unavailable/backward clock policy; timezone/locale independence; missing
  environment variable versus empty; command-line quoting and argument
  count/length boundaries; process exit-code propagation.

### 8.7 Detailed generated-COBOL validation plan

Generated COBOL is a product artifact and must be verified independently at
several layers. The current `transpiler_validate_generated_cobol()` parses the
text into the project's COBOL AST; that is a useful fast gate, but does not by
itself prove dialect conformance, successful compile/link, ABI agreement, or
runtime correctness. Its current implementation is in
[`src/transpiler/transpiler_validation.cpp`](../src/transpiler/transpiler_validation.cpp).
Retain that structural entry point and implement the following pipeline after
emission.

The expected contract and the observed output must be produced through
independent paths. Build semantic expectations from the verified typed IR,
concrete CFG/lifecycle plan, finalized logical and physical ABI descriptors,
profile manifest, and frozen name table. This builder may consume canonical
IDs and approved name-table entries, but must not consume COBOL module IR,
renderer text, emitter helper output, or a COBOL parse. Separately, construct
the COBOL module IR from the verified lowering pipeline; run its verifier;
render it; and parse the exact rendered bytes with the independent closed-
subset reader into an observed graph. Compare both module IR and observed graph
against the semantic expectations. The expectation builder and COBOL reader
must not call the renderer or the lowering functions they are checking. Keep
independent hand-authored reference vectors for profile-sensitive layouts,
calls, and branch/lifecycle mappings: sharing a schema is acceptable, sharing
the code that derives expected decisions is not. Structural agreement proves
only correspondence; compile/link/run and fault-injection tests remain the
evidence for compiler acceptance and behavior.

#### 8.7.1 Implementation architecture

1. **Emit a semantic expectation manifest independently of output lowering.**
   For every generated module, record expected `PROGRAM-ID`,
   divisions/sections, data names and pictures/usages, paragraphs, `CALL`
   targets and arities, entry points, file
   declarations, exception/result linkage, cleanup edges, target profile, and
   source spans. For each function call, record activation acquisition/check,
   push/pop edges, configured/profile bound, and the exact typed
   `stack_overflow{limit, requested_depth}` failure edge and payload layout;
   identify calls whose limit is verifier-proven unreachable. For each
   imported external call, also record the canonical
   dependency/adapter ID and version, declared call mode, frozen COBOL target,
   exact argument/result/context slots, whether native `ON EXCEPTION` handling
   is required, application-status field and exact status-to-branch/type map,
   output-validity conditions, result-publication guard, and effect/exception
   edges. For an exception-aware external call, include the legal return-frame
   states, parent/activation identity check, declared dynamic-type set,
   payload-completion/descriptor checks, and required validation-before-result
   and validation-before-cleanup ordering. For each callback, record the
   reverse call edge, canonical callback entry/signature, invocation frame,
   lifetime, cardinality/ordering around external buffer mutations, argument
   aliases, and callback-exception translation/continuation contract. Derive
   these expectations from the imported `.cbi` descriptor and verified typed
   call/CFG—not from the module IR or emitted adapter text. For
   generic code, also record every canonical specialization
   key, parent/child type edge, instantiated method, exact lifecycle callee,
   operator/member-call chain, and nesting depth. Derive it only from verified
   typed operations/CFG, lifecycle plan, logical and physical ABI descriptors,
   profile manifest, and frozen name table. Do not consume COBOL module IR,
   renderer text, emitter helpers, or parsed COBOL, and do not infer semantics
   from emitted text. Keep hand-authored golden vectors for profile-sensitive
   layout and control-flow decisions so a shared backend rule cannot certify
   itself.
   For each executable root, independently derive startup and shutdown
   expectations from its verified initializer graph/effect summaries,
   lifecycle plan, entry CFG, exception ABI/type records, the versioned
   uncaught-report semantic contract (not the generated reporter descriptor),
   target-profile capabilities, and frozen names. Record root-control
   initialization before failure-capable operations; heap/service/global
   initialization order and `READY` gates; entry eligibility; completed-object
   and service unwind order; uncaught-report paragraph identity and allowed
   frame reads/call/effect closure; report-before-payload-destroy-before-
   service-finalization-before-heap-release ordering; normal entry-result
   preservation; and fatal/uncaught termination categories. Keep stable
   expectation IDs, provenance, and exact source/profile/ABI input digests.
   These records are independent expected values, not a copy of module IR,
   emitted paragraphs, or the reporter descriptor's claims. The semantic
   contract defines permitted behavior; a distinct expected-value derivation
   computes the exact allowed fields, edges, ordering, and terminal mapping.
2. **Tokenize and structurally validate the emitted subset.** Extend
   `transpiler_validate_generated_cobol()` or add a dedicated validator that
   understands source-format columns, comments, literals, words, punctuation,
   statements, and nesting. Validate an AST/structure rather than using
   regex-only checks: reserved words in comments/literals, continued strings,
   periods, and nested scopes make raw text matching unreliable. The validator
   needs only the generated dialect subset, but must fail closed on unknown
   syntax unless the selected profile explicitly allows it.
3. **Compare parsed structure with the manifest.** Check that every expected
   program/data/paragraph/call/linkage item exists exactly once; pictures and
   signatures agree; every call resolves to a generated module or declared
   runtime/external entry; external calls have the exact adapter mode, target,
   physical slots, required `ON EXCEPTION` clause, status reads/comparisons,
   branch targets, and exception/result-publication guards; no success-only
   result is used on a failure path; exception-aware returns validate frame
   state, payload/type/ABI and parent identity before result use or cleanup;
   every declared callback edge obeys its cardinality/order, alias and failure
   continuation contract; no unresolved template name or unexpected public
   entry was introduced. Activation checks occur after argument staging but
   before callee entry/result publication; emitted payload fields and each
   successful push/pop agree with the activation manifest, while an unreachable
   limit edge is omitted only with its verifier-backed proof. Report the
   executable-root startup, normal-exit, uncaught-exit, and fatal paths against
   their independently derived expectations: check root-state initialization,
   heap/service/module-init ordering, `READY` gating, success-result
   preservation, reporter call/effect closure, exception-payload lifetime,
   reverse cleanup, leak-check/release, and the exact profile termination
   operation. Reject any report/read/destroy/finalize/terminate reordering or
   return edge not allowed by the root CFG. These checks must compare parsed
   COBOL to semantic expectations, not merely compare it to the reporter or
   startup descriptor.
   Report the
   `expectation_id`, source origin, and observed generated COBOL byte/line/
   column range in the validation-correlation report, not in the semantic
   expectation manifest.
4. **Compile with real COBOL compilers.** Write output to a unique temporary
   directory and invoke every pinned compiler with explicit source-format and
   dialect options. For GnuCOBOL free-format output, pass `-free` explicitly;
   its manual documents this option and multiple source formats
   ([GnuCOBOL manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)). Use
   the compiler's supported compile/link modes for subprogram and executable
   tests. Capture command, compiler/version, exit code, stdout, stderr, and
   warnings in per-test temporary artifacts. GnuCOBOL acceptance alone does
   not prove another vendor accepts the output.
5. **Run semantic fixtures.** Link and execute programs exercising each
   feature; compare the normalized process-status category, any
   profile-promised raw status mapping, stdout/stderr, file effects, and
   termination behavior against hand-written expected results or reference
   models—not against a prediction extracted from the COBOL being tested.
6. **Separate the layers.** Keep the structural validator pure/in-memory for
   fast unit tests. Put compiler invocation behind a test adapter and runtime
   fixtures in integration tests. Missing compilers may be a visible skip only
   on non-required profiles; they fail the release matrix.

The semantic expectation manifest is derived before module-IR construction and
must not contain module-IR node IDs or rendered source ranges. Give each
expectation a stable `expectation_id` derived from its canonical source
operation/CFG-edge identity, expectation kind, and role; retain source origins
and all input artifact identities needed to re-derive its expected value.
After construction, a separate validation-correlation report joins each
expectation to the verified module-IR node(s) and independently parsed COBOL
node(s) that claim to implement it, then records generated byte/line/column
ranges and comparison outcomes. These are observed correlation fields, not
inputs to or expected values in the semantic manifest. Require unique or
explicitly one-to-many/many-to-one joins with the correspondence policy
versioned; a missing, ambiguous, or unexpected implementation node is a
validation failure, not permission to amend the expectation. Version the
report as `CBLC-COBOL-VALIDATION-CORRELATION` and bind it to the exact semantic
expectation, verified module-IR, rendered-source, source-map, parser-grammar,
and profile-contract digests. Each join row records the `expectation_id`,
observed IR node IDs, parsed-output node IDs, generated byte/line/column
ranges, cardinality rule, comparison rule IDs, and pass/fail result; it does
not copy expected semantic values as an alternate source of truth. Require
complete coverage of every expectation and every relevant emitted node, with
explicitly classified renderer-only syntax such as required delimiters or
continuation artifacts. Unclassified output is a failure. Publish the report
atomically as validation evidence only after parsing and all joins complete;
failure evidence may describe rejected rows but must never be represented as a
successful report. This separation distinguishes (a) a wrong lowering
decision, (b) a correct module node rendered incorrectly, and (c) valid output
rejected by a profile parser without creating a dependency cycle or allowing
generated output to certify its own expectations. Both artifacts are
test/compiler infrastructure, not generated-program runtime dependencies;
correlation/evidence changes do not alter generated-source identity or feed
back into generation.

Use focused source goldens for representative constructs, structural
assertions for ABI/shape, real compiler acceptance, and execution tests
together. Whole-file goldens alone are too sensitive to formatting and do not
prove semantics.

##### COBOL tool invocation adapter contract

Treat compiler, linker, and generated-program execution as external tools with
one platform-neutral supervision interface. The versioned tool-invocation
descriptor supplies the verified executable/package identity, compiler family
and version constraint, exact ordered tool argument vector,
working-directory policy, tool-environment policy and `tool_environment_limits`,
declared outputs, process/filesystem resource envelope, `tool_argv_limits`,
`tool_launch_payload_limit`, and timeout/output limits. It is selected under the
target profile's tool-compatibility policy,
but is not part of the semantic target profile. The target profile remains
authoritative for source format, dialect, and promised COBOL behavior; the
invocation's compiler options must implement those declared requirements and
cannot silently change the target semantics. The adapter passes an argument
array directly to the process API; it must not build a shell command by
concatenating paths or flags. For generated-program runs, keep the tool
argument vector (the executable/runtime-launcher invocation) separate from
`program_argv`, the exact logical arguments intended for the CBL-C entry
point. The adapter serializes `program_argv` only at the host process boundary
and records its exact ordered values or fixture digest in validation evidence;
never pre-quote or concatenate program arguments into a shell string. Define
two independent limits: `tool_argv_limits` belongs to the versioned invocation
descriptor and bounds the external tool's argument count plus native launcher
serialization length (count excludes the separately identified executable;
serialized length includes its path); `program_argv_limits`
belongs to the target-profile source contract and bounds the logical
application argument count plus aggregate UTF-8 byte length, excluding the
executable name. Empty strings count as arguments and contribute zero payload
bytes. Record the argument domain (`tool_argv` or `program_argv`), limit
dimension, configured value, and observed value/units whenever
`argument_limit_exceeded` is returned. A tool-vector excess is evidence-side
preflight failure; an application-vector excess is detected before launching
the generated program and cannot be reported as an application exception.
Before claiming a profile on a host, prove the adapter can faithfully carry
every valid vector up to that profile's declared `program_argv_limits`; a
smaller native launcher capacity makes the profile unsupported, not silently
less portable. Malformed native-encoding arguments remain explicit negative
fixtures: if they are within launcher limits, pass them unchanged to the
generated program's decoder and require `invalid_encoding`, never replacement
text. Apply checks in this order: logical argument count, encoding validation,
then the source-visible aggregate UTF-8 byte limit. Count still applies to
malformed input; malformed bytes have no UTF-8 length. The host-native
serialized-length limit is checked
before spawn and may therefore prevent delivery of a malformed-input fixture;
that is an adapter limit result, not `invalid_encoding`. Do not conflate that
decoder error with a native launcher-length excess.

If the tool argument vector references a response file, its exact bytes are a
declared immutable invocation input: record its digest, encoding/line-ending
contract, and the pinned compiler's response-file grammar and nested-include
policy. Stage it read-only, include all permitted nested response files in the
input closure, and enforce deterministic byte/depth/count bounds with cycle
detection. Reject undeclared or dynamically resolved includes, mutable files,
ambient response-file discovery, and unsupported grammars before spawn; do not
hash only the `@path` argument and assume that it fixes the effective options.
Changing response-file contents changes validation evidence and reruns tool
acceptance, but does not change generated COBOL identity unless the selected
target-profile contract itself changes.

Shell quoting is not a portable escaping
mechanism and is not part of a compiler profile. The Windows and POSIX process
backends may differ internally. If a host API needs command-line
serialization, only the process adapter performs that OS-specific encoding;
it must not invoke `cmd.exe`, PowerShell, or another shell. Fake-tool tests
verify that the child receives the same logical argument boundaries on both
hosts. Both adapters expose the same result categories, limits, cleanup
guarantees, and captured evidence.

The invocation resource envelope is explicit and bounded independently of the
compiler's declared outputs. It includes wall-clock time, stdout/stderr bytes,
maximum file/directory-entry count, per-file logical byte length, aggregate
logical and allocated scratch bytes, maximum relative-path length/depth, and
child-process count. It also declares peak committed-memory bytes across the
contained process tree using the metric
`process_tree_peak_committed_private_bytes_v1`, plus aggregate user-and-kernel
CPU time in nanoseconds using `process_tree_cpu_time_ns_v1`. These are logical
process-tree metrics, not interchangeable per-process RSS/working-set samples.
For memory, at each accounting observation sum committed private bytes for
every live process in the containment domain (root plus descendants), then
take the maximum of those simultaneous sums over the invocation; do not sum
each process's separate peak. For CPU, sum user and kernel CPU time for every
thread in every process from spawn until the whole domain is reaped, including
children that exit before the root. Sample/accounting time is monotonic, and
any measurement interval is included in the declared overshoot bound.
Every generated-program execution has a hard wall-clock timeout and memory
ceiling; CPU-time ceilings are explicit for fault-injection, stress, and other
resource-sensitive tests. Compiler/linker invocations record their configured
memory/CPU policy as well. If a required limit cannot be enforced with the
declared metric, fail before spawn; no runtime evidence is accepted under an
unbounded fallback. Each adapter declares maximum memory and CPU enforcement
overshoot; the harness checks measured use against the configured limit plus
that bound. Required hosts must implement the same metric definitions, or the
different capability is an explicitly separate profile and cannot inherit a
cross-host resource-limit claim. Metric IDs, enforcement bounds, and adapter
capabilities are recorded, not hidden host defaults.
All byte limits count octets. Entry limits count every created directory entry
(including subdirectories and links, with links rejected by the path policy).
Normalize relative paths to `/` separators and count their UTF-8 byte length;
depth is the number of path components, with no Unicode normalization. Process
count is the peak number of live OS processes in the launched descendant tree,
including the root process and excluding threads.
Enforce these limits with a quota-capable isolated volume or sandbox; a post-run
directory scan alone could discover exhaustion too late. The adapter reports
which limits it can enforce before launch. If a required hard limit is
unsupported, fail before starting the tool; do not silently weaken the profile.
Host-specific quota/process APIs remain behind the build-tool adapter, while
the observable limit/result contract is identical across hosts. The selected
limits and adapter capability digest belong to the attempt's evidence-input
identity, not the target-profile contract or generated-artifact key.

Standard streams are explicit invocation inputs/observations, never inherited
implicitly from an interactive terminal. Each invocation declares stdin as
either `closed` (the adapter starts the child with immediate EOF) or a staged
byte fixture whose exact byte length and digest are recorded. The adapter
delivers fixture bytes unchanged, closes stdin after the final byte, and
reports a write/broken-pipe failure separately from the child's exit result;
empty input and absent input are therefore distinguishable. The stdin feeder,
stdout drainer, stderr drainer, and child wait/reap must make progress
concurrently. Never synchronously fill one pipe while waiting for the child to
read or drain another; bound adapter-side buffers and propagate cancellation
to every pump. If the child exits early, stop the feeder, finish draining both
outputs, reap the child, and retain the broken-pipe/early-exit facts without
letting a writer thread outlive the invocation. Record `stdin_delivery_state`
as `not_started` (no child process was spawned), `closed` (a started child was
given immediate EOF), `complete`, `truncated`, or `write_failed`; `complete`
means every fixture byte was accepted by the child pipe, not that the program
consumed it. A declared fixture is not a successful validation
input if delivery is truncated or fails; tests of deliberate early-close
behavior assert that transport outcome explicitly. Compiler and linker
profiles default to closed stdin. Runtime fixtures may provide staged stdin,
but may not read from a developer's terminal or an undeclared host stream. Captured
stdout and stderr remain separate byte streams; comparison is byte-exact by
default, with any profile-specific normalization explicitly named, versioned,
and tested. Do not silently transcode encodings, fold case, or normalize line
endings. The invocation identity records stdin mode/digest and the
output-comparison policy; these affect validation-attempt evidence, not source
semantics or the generated-artifact key. Retain bounded raw output; once EOF is
observed, also retain the full-stream digest and byte count even when only
bounded excerpts are stored. If EOF is not observed, mark the byte count and
digest as partial and never use them for an exact-output assertion.

For every invocation:

1. Resolve and record the selected profile/tool before compiling; query the
   tool version under the same supervision policy and reject an unsupported or
   mismatched version before accepting any output. Do not select a compiler by
   host-OS guess when multiple profiles are installed.
2. Create a unique isolated working directory and stage only declared inputs
   and dependencies there. Immutable source, interface, tool, and evidence
   inputs remain read-only and digest-verified. For generated-program
   filesystem tests, seed a separate per-run fixture tree from the frozen
   evidence snapshot; its manifest declares each relative path, initial
   content digest, and `read_only` or `read_write` permission. Writes, creates,
   deletes, and renames are confined to that disposable run tree and declared
   output roots; expected final path/content effects are part of the test
   oracle. Never expose the evidence snapshot itself as writable. Verify
   protected input digests again after exit, then compare the complete
   permitted output/effect set; reject undeclared effects. If the selected
   adapter cannot enforce this boundary, fail closed for filesystem conformance
   rather than treating a post-run hash check as containment. Pass paths as
   individual arguments, preserving spaces and Unicode; never
   rely on the caller's current directory, a shell-specific path separator, or
   environment-variable expansion. The compiler/tool receives an explicit
   `tool_environment` allowlist, with locale normalization where supported so
   diagnostic parsing is repeatable. Construct the child environment from an
   empty map plus the descriptor's explicit entries and only the minimal
   host-launch entries declared by the versioned adapter; never inherit the
   parent environment wholesale. Tool variable names must match
   `[A-Za-z_][A-Za-z0-9_]*` and values are strict UTF-8; reject NUL in names
   or values, `=` in names, malformed encoding, and names colliding under
   ASCII case-folding on every host. Keep
   declared spelling on POSIX, use strict UTF-8-to-UTF-16 conversion on
   Windows, and sort by canonical folded name when serializing. The adapter
   declares `tool_environment_limits` for entry count, key bytes, value bytes,
   aggregate UTF-8 payload bytes, and adapter-defined native serialized
   environment bytes. UTF-8 payload counts exclude separators and terminators;
   native serialized bytes include the host-required key/value delimiters and
   terminators under the named adapter version. Preflight every dimension
   before spawn and report all exceeded dimensions. Bind exact names and value
   digests (not secret values) plus adapter-required host entries into attempt
   evidence. Acceptance fixtures cannot depend on secret or ambient values.
   Compute each value digest as
   `SHA-256("CBLC-TOOL-ENV-VALUE-v1\0" || value_utf8_bytes)`; compute the map
   digest as `SHA-256("CBLC-TOOL-ENV-MAP-v1\0" || canonical_map_bytes)`.
   `canonical_map_bytes` use the `.cbi` canonical text rules and this exact
   field shape/order: top-level `schema`, `entries`; each entry has
   `namespace`, `name`, `utf8_length`, `value_sha256`. `schema` is
   `tool_environment_map_v1`; `namespace` is exactly `declared` or
   `adapter_host`. Encode lengths as canonical decimal strings and digests as
   lowercase full hexadecimal. Sort entries first by namespace (`declared`
   before `adapter_host`), then by ASCII-folded name bytes; preserve exact
   spelling in `name`. Case-fold duplicates are rejected across both
   namespaces, never overridden. Include adapter-required host-launch entries
   in the separately tagged namespace.
   An absent variable has no entry; an empty variable has a zero byte length
   and the digest of the empty byte sequence. Independent golden vectors must
   prove order independence, absent/empty distinction, exact value binding,
   and separation of user-declared versus adapter-required entries.
   A generated program receives a separate
   declared `program_environment` fixture; never inherit ambient developer
   variables or treat tool-only variables as application inputs.
   Independently enforce `tool_launch_payload_limit` for host APIs whose
   aggregate process-creation bound covers both argument and environment data.
   The adapter-versioned `tool_launch_payload_bytes_v1` calculation includes
   the executable/runtime launcher, tool arguments, `program_argv` when
   launching generated code, the serialized environment, and every
   host-required terminator/overhead byte declared by that adapter. Check it
   after individual logical bounds but before spawn; a host that cannot compute
   or enforce the declared bound is unsupported. This host-specific limit is
   attempt evidence, not a target-profile semantic bound. The adapter binds the
   effective native launch ceiling and its source/version for each attempt;
   it must account for current process/resource constraints that affect the
   host API and cannot infer capacity from the OS name alone. If a trustworthy
   ceiling is unavailable, fail closed as unsupported rather than risk a
   late, ambiguous spawn failure.
   `tool_environment`, `tool_argv_limits`, this joint launch limit, and its
   metric/version are invocation-evidence inputs: changing them reruns the
   affected tool evidence but does not change source semantics or generated
   artifact identity. If the host cannot carry every valid
   `program_argv` allowed by the selected profile, that host/profile pair is
   unsupported rather than silently narrowing the source contract.
3. Start stdout and stderr draining concurrently with the child process. Keep
   bounded byte counts and bounded retained tails/diagnostic excerpts. If a
   stream exceeds its configured cap, mark `output_limit_exceeded`, terminate
   the process tree, continue draining while discarding bytes beyond the cap,
   reap safely, and retain enough evidence to identify the tool and limit
   without storing unbounded output.
   Enforce the declared file/entry/path/byte limits on the writable subtree
   while the tool runs. Exceeding a stream or scratch-output limit terminates
   the complete child tree, drains/reaps safely, records bounded evidence, and
   publishes none of the partial output. The post-run artifact scan remains a
   separate integrity check and is not a substitute for a hard scratch quota.
4. Measure timeout and cleanup grace periods with a monotonic clock. Launch
   each tool inside an adapter-owned process-containment domain that tracks
   the root and every descendant; the domain must prevent or reliably detect
   descendants escaping supervision. A root-process exit is not invocation
   completion: wait for the declared descendant policy and pipe EOF. The
   default permits no surviving descendants. If a descendant remains after
   the bounded normal-exit grace period, terminate the full domain and classify
   `orphan_process_detected`, even if the root exited zero. On timeout,
   cancellation, or a hard resource limit, stop stdin feeding, request graceful
   domain termination according to adapter policy, then force-terminate the
   complete domain when the grace deadline expires. Drain/reap all children and
   finish or invalidate every stream pump before returning. A separate hard
   cleanup deadline bounds this process; if the adapter cannot confirm that
   all descendants are dead and handles/pumps are closed, return
   `process_cleanup_failed`, retain the staging root in quarantine, and block
   its reuse or garbage collection until recovery proves it safe. POSIX and
   Windows may implement containment differently behind this contract; if a
   host cannot provide the required containment, fail before spawn with
   `resource_limit_unsupported`. Never report a source diagnostic for timeout
   or cancellation, and never return while a compiler/linker/runtime child can
   still access staged files.
5. Validate expected artifacts independently: required output exists, is a
   regular file within the isolated root, is not a symlink/reparse-point escape,
   and is not accepted merely because the tool returned exit code zero. A
   nonzero exit with partial output never publishes an artifact; a zero exit
   with missing, unexpected, or malformed output is an external-tool or
   artifact-validation failure.
6. Record a normalized invocation identity (profile ID/version, tool family
   and reported version, ordered tool arguments, and—only for generated-program
   execution—the distinct ordered `program_argv` values or fixture digest,
   `tool_argv_limits`, `tool_launch_payload_limit`, metric/version, and
   effective native ceiling source/value,
   `tool_environment` canonical-map digest and
   policy/limits, adapter-required host-entry identities, and—only for generated-program
   execution—`program_argv_limits`, `program_environment`, and host-service
   fixture digest, input artifact
   digests, stdin mode and fixture digest, stdout/stderr comparison policy,
   process/filesystem/memory/CPU limits and metric/overshoot IDs, timeout/output
   limits, overall outcome, child process outcome/raw status, stdin delivery
   state, termination trigger, process-tree cleanup outcome, and separate
   stdout/stderr capture states)
   in validation evidence. Keep the resolved executable path, actual OS
   command representation, raw stdout/stderr, and temporary paths in the
   bounded diagnostic sidecar only; they are machine-local evidence, not
   semantic cache keys or source/module identity.

The adapter result is a closed tagged record, not a single exit integer. It
contains an overall outcome, a closed `preflight_failures` list (empty after a
successful preflight, otherwise all detected pre-spawn failures with type,
dimension, bound, and observed value), the child process outcome/status when
available,
the termination trigger (`none`, timeout, cancellation, limit, or orphan
containment), process-tree cleanup outcome (`complete`, `failed`, or
unsupported), stdin delivery state, and independent stdout/stderr capture
states so one failure cannot mask another. A resource-limit outcome also
records the dimension, metric ID, configured limit, measured/accounted usage,
and allowed enforcement overshoot; unsupported enforcement records the
missing capability before spawn. An `argument_limit_exceeded` result additionally
records `argument_domain` (`tool_argv` or `program_argv`), the dimension
(count, aggregate UTF-8 payload bytes, or native serialized length), configured
limit and observed value with units, all simultaneously exceeded dimensions,
and whether a child was spawned. If more than one applies, choose the primary
dimension deterministically: tool argument count before native serialized
length; program argument count, then valid-input aggregate UTF-8 bytes, then
native serialized length. Malformed input has no aggregate UTF-8 dimension.
A `tool_environment_limit_exceeded` result records `tool_environment` as its
domain, the complete set of exceeded dimensions (`entry_count`,
`key_utf8_bytes`, `value_utf8_bytes`, `aggregate_utf8_payload_bytes`, or
`native_serialized_environment_bytes`), configured bounds, observed values,
adapter version, and that no child was spawned. It is distinct from
`argument_limit_exceeded`, whose domain is only `tool_argv` or `program_argv`.
A `tool_launch_payload_limit_exceeded` result records metric ID/version,
configured and measured aggregate launch bytes, effective native ceiling
source/value, the argument/environment component sizes, and that no child was
spawned. It applies when each individual
argument/environment bound passes but their native combined payload exceeds
the adapter's declared limit.
A `tool_version_unparseable` result records the version-parser adapter ID/
version, successful version-query child status, complete stdout/stderr capture
states and digests, and the bounded raw diagnostic reference. It forbids later
compile/link/run children for that attempt. It is distinct from
`version_mismatch`, which requires a successfully parsed version outside the
compatibility constraint.
Overall outcomes include
`spawn_failed`, `tool_version_unparseable`, `version_mismatch`,
`exited_successfully`, `exited_with_failure`,
`abnormal_termination`, `tool_timeout`, `orphan_process_detected`,
`process_cleanup_failed`, `argument_limit_exceeded`,
`tool_environment_limit_exceeded`, `tool_launch_payload_limit_exceeded`,
`output_limit_exceeded`,
`stdin_transport_failed`, `stream_capture_failed`,
`tool_resource_limit_exceeded`, `resource_limit_unsupported`, `cancelled`, and
`artifact_contract_failed`. Preserve the numeric exit status when one exists,
but do not conflate a signal/abnormal termination with a compiler's documented
diagnostic exit; a successful child exit cannot override incomplete declared
stdin delivery or failed output capture. Diagnostic parsing is a separate
versioned adapter keyed by tool family/version; failure to parse text retains
the raw bounded message as an external-tool diagnostic and never fabricates a
CBL-C source span. Warning promotion is an explicit test/release policy and is
recorded with the invocation, not inferred from locale or shell state.

Each output stream has its own closed capture state: `not_started` (no child
spawned), `complete` (EOF observed and full byte count/digest available),
`limit_exceeded` (configured cap crossed, then drained to EOF while discarding
excess retained bytes), `read_failed` (the adapter could not continue reading
the pipe), or `incomplete` (EOF was not observed before the cleanup deadline).
`limit_exceeded` maps to `output_limit_exceeded`; `read_failed` maps to
`stream_capture_failed`; `incomplete` means EOF was still absent at the hard
cleanup deadline, prevents validation success, and requires
`process_cleanup_failed`. Never infer empty output from
`not_started`, `read_failed`, or `incomplete`.

The record retains every observed condition; `overall_outcome` is a single
deterministic primary classification and must not erase the orthogonal fields.
Select it by the following ordered rules:

1. A failure before any child is spawned uses its specific result
   (`spawn_failed`, `argument_limit_exceeded`,
   `tool_environment_limit_exceeded`, `tool_launch_payload_limit_exceeded`,
   or `resource_limit_unsupported`); child
   status is absent and both output streams are `not_started`. When multiple
   preflight failures are observed, retain each in `preflight_failures` and
   select the primary in this order: required-capability/containment failure,
   tool-argument limit, tool-environment limit, combined launch-payload limit,
   then spawn failure.
2. If the hard cleanup deadline expires before the process tree, stream pumps,
   and handles are confirmed closed, select `process_cleanup_failed`.
3. Otherwise select `orphan_process_detected` if a descendant survived the
   normal-exit grace period, even when forced cleanup subsequently succeeded.
4. Otherwise select the highest-priority observed terminal trigger in this
   fixed order:
   `cancelled`, `tool_timeout`, `tool_resource_limit_exceeded`,
   `output_limit_exceeded`. Preserve every observed trigger and resource
   dimension in the record; this fixed ordering, not event scheduling, chooses
   the primary result when multiple triggers occur.
5. Otherwise select `stdin_transport_failed` for truncated or failed delivery
   of declared fixture bytes, then `stream_capture_failed` for a `read_failed`
   stream state.
6. Otherwise, after a successful and fully captured version-query child,
   select `tool_version_unparseable` when its version adapter cannot produce a
   canonical version record; select `version_mismatch` only when a parsed
   version is outside the selected profile's compatibility contract. A failed
   query follows the preceding failure rules; preserve its child status,
   streams, and raw diagnostics.
7. Otherwise, for a child that exited successfully, select
   `artifact_contract_failed` if its declared output contract was violated
   (missing, malformed, escaped, or undeclared output). If no earlier rule
   applies, select `abnormal_termination`, then `exited_with_failure`, and
   finally `exited_successfully`. A nonzero or abnormal child result is not
   reclassified as an artifact failure merely because its output is partial
   or missing; preserve any artifact violation as a separate observed fact.

A `limit_exceeded` stream state selects `output_limit_exceeded` under rule 4
unless a higher rule takes precedence. `incomplete` requires rule 2. The exact
initiating condition is also stored as `termination_trigger`; simultaneous
resource dimensions and both stream states remain individually recorded. A
cleanup failure never overwrites the original timeout/cancellation/limit
trigger. This ordering is normative for cache evidence and diagnostics, not
merely presentation.

##### Filesystem scope for validation runs

Keep evidence inputs, runtime fixtures, and generated-program outputs as
separate namespaces. The immutable evidence snapshot is the source of fixture
bytes and expected effects; each execution receives a disposable copy/overlay
that cannot write back into that snapshot or another test/build. A fixture
manifest names exact relative paths, initial digests, and permitted access
(`read_only` or `read_write`); expected creation, final bytes, replacement,
rename, or deletion is declared separately. Paths outside the run root,
absolute/drive/UNC paths, parent traversal, undeclared symlink/reparse-point
traversal, and host aliases that escape the root are denied. Path checks use
the selected profile's filesystem equivalence rules and never rely only on
string-prefix comparison. Build inputs remain immutable even when a runtime
fixture is writable. Compare the complete post-run path/effect set against the
oracle, not only expected files; ignore timestamps/host metadata unless a
specific API contract makes them observable. The adapter must enforce isolation
during execution, not infer safety from a final digest scan. If a required host
cannot confine the generated process to the declared roots, report the
capability as unsupported and do not count filesystem behavior as portable
evidence.

The invocation harness must test argument fidelity and failure containment,
not only successful compilation. Add fixtures with checkout/temp paths
containing spaces, non-ASCII characters, shell metacharacters, and leading
hyphens; missing executables; version output that is malformed or hangs;
children that flood stdout and stderr simultaneously; a compiler that creates
partial output and exits nonzero; a compiler that returns zero without its
declared output; a process whose root exits while a descendant remains alive,
holds output pipes open, or attempts to mutate staged inputs; a descendant
that ignores graceful termination or attempts to escape its containment
domain; timeout and cancellation at each compile/link/run stage;
reparse-point/symlink escape attempts; and environment/locale differences. On
both Windows and POSIX,
assert exact logical argv, selected profile, result category, bounded retained
bytes, complete process-tree termination, cleanup of only the isolated
temporary root, and no publication of partial artifacts.

##### Artifact manifests, verification evidence, and publication

Keep three records distinct; combining them makes it too easy for an output
parser to certify its own mistaken lowering:

- The **expectation manifest** is derived only from canonical typed IR, CFG,
  lifecycle plans, logical and physical ABI descriptors, the target-profile
  manifest, and the frozen name table. It must not consume COBOL module IR,
  renderer output, or a parse of generated COBOL. The independently produced
  COBOL module IR is verified and compared against this manifest before
  rendering; the independently parsed rendered artifact is compared against
  the same manifest afterward. This prevents the lowering representation from
  certifying its own decisions. The manifest states what the artifact must
  mean: canonical module/symbol
  identities, profiles, calls and their resolved providers, layouts, numeric
  operation/type mappings and failure outcomes, control-flow/lifecycle edges,
  name-table references, and provenance.
- The **artifact manifest** is the immutable index for the produced bundle. It
  records only deterministic build identity and outputs: artifact schema,
  CBL-C compiler/backend and artifact-producing toolchain versions, canonical
  module IDs, source snapshot
  hashes, selected profile IDs/versions, interface and ABI fingerprints,
  name-table and expectation-manifest digests, generated COBOL artifact
  digests, source-map digests, and exact declared dependency edges. Paths are
  bundle-relative `/`-separated artifact locations, never source identity or
  absolute host paths. Every referenced file must be within the bundle root;
  reject traversal, undeclared symlink escape, missing files, duplicate IDs,
  and duplicate artifact paths. It also records a closed artifact class such
  as `release` or `test_instrumented`; the latter binds the instrumentation
  schema/version, test-runtime module digests, and instrumentation-site
  manifest digest. A release manifest cannot include test-only artifacts, and
  the link planner rejects a closure that mixes classes or instrumentation
  build identities.
- The **validation evidence record** states which claims were actually tested:
  structural validation, profile/dialect validation, compiler acceptance,
  linking, execution, and reference-model assertions. For each invocation it
  records stable compiler family/version/profile/options, result category,
  exit status, warning IDs, and artifact digests. Keep raw command paths,
  stdout/stderr, temporary paths, and host-only details in a separate bounded
  diagnostic sidecar; they are useful evidence but are excluded from the
  deterministic semantic-manifest hash. Bind the evidence to the exact
  artifact-manifest digest and test-run identity. Evidence records and their
  run-specific digests are not inputs to the deterministic artifact manifest
  or `generation_key`; repeated tests of identical artifacts may produce
  distinct evidence records without implying nondeterministic compiler output.
  The attempt-specific commit manifest/record binds the exact evidence set to
  the artifact-manifest digest. The current-generation reference, not the
  commit record alone, marks the artifact/evidence pair as current and
  successful. A JSON field claiming
  `compiled=true` or `passed=true` is not evidence by itself: local records are
  unverified unless produced directly by the active harness, and release
  claims require a trusted CI result/attestation bound to the manifest digest.
  If evidence is exported for independent verification, its attestation must
  identify the trusted workflow and the exact test/profile matrix; otherwise
  present it as an unverified local report, not a release gate.

All serializers reject duplicate fields/IDs, unknown required schema versions,
invalid digest encodings, inconsistent paths, undeclared dependencies, and
references to artifacts whose content digest does not match. Use SHA-256 over
the exact generated bytes with a declared digest domain and byte encoding;
generated line endings are fixed by the profile so the digest is reproducible.
Do not treat the current showcase manifest as the target schema: for example,
`samples/feature_showcase/cobol/cblc.manifest.json` uses `fnv1a64` artifact
hashes and embeds absolute checkout paths, so it is neither relocation-stable
nor strong enough to serve as the specified ABI/artifact fingerprint. The
current `transpiler_validate_generated_cobol()` in
`src/transpiler/transpiler_validation.cpp` only parses text into the COBOL AST;
it does not read this manifest, compare semantic expectations, or establish
compiler/link/runtime success. Keep that parser as one gate, not as the
manifest verifier.

Build a link manifest from the validated dependency graph, not by scanning a
directory or trusting only `CALL` text. For each call edge, preserve caller
module/symbol/call-site IDs, target provider module/symbol IDs, the frozen
COBOL entry spelling, ABI fingerprint/version, profile, and provider artifact
hash. The independently parsed COBOL call set must equal the expected call
set: unexpected calls and missing calls both fail. Add declared `COPY`, file,
runtime, and external-program dependencies as typed edges with distinct kinds.
The link closure includes exactly reachable providers and declared runtime
dependencies; deduplicate a provider only when canonical module ID, profile,
and artifact hash all agree. Duplicate program definitions, unresolved or
ambiguous targets, and mixed-profile artifacts fail before invoking the
COBOL linker.

Publish an output only after every artifact and required validation evidence is
complete. Write deterministic artifacts, validation records, and the complete
attempt-commit payload to isolated attempt staging; verify all hashes, manifest
references, link closure, and the staged commit payload before the coordinator
enters `prepared`. Under the publication protocol in Section 6.7, bind/reuse
the immutable artifact generation by `generation_key`, publish the verified
attempt-specific commit record immutably with no-replace semantics, then
atomically replace the current-generation reference as the sole success and
visibility linearization point. The commit record binds the `attempt_id`,
artifact generation key and artifact-manifest digest, validation-plan
ID/version/digest, full pass-registry digest, validation-input/evidence
digests and record IDs, and the exact expected artifact IDs. The commit record
is outside the deterministic generation and cannot make an attempt current by
itself; a published but unreferenced record/generation is an orphan, not a
successful build. Artifacts and published records are immutable. A reader
considers a current bundle successful only if the current reference, artifact
manifest, complete expected artifact set, commit record, and bound evidence
records all validate. A crash or injected failure before reference replacement
must leave the previous reference valid; an indeterminate replacement requires
the recovery path and cannot be guessed. If the selected output store cannot
provide the required commit/publication semantics, fail the build with a
storage diagnostic rather than emulate atomicity by overwriting files in
place. Cleanup of orphan staging bundles is a separate retention policy and
must never delete the last known-good committed bundle.

#### 8.7.2 Required output checks

- **Format and structure:** selected fixed/free format; line/column,
  continuation, comment, literal and encoding rules; no truncated line/name/
  literal; required divisions/sections in order; exactly one expected
  `PROGRAM-ID` per generated module.
- **Names and declarations:** names legal and within profile limits; unique
  under COBOL's case-insensitive name comparison; no reserved-word collision;
  all `PIC`, `USAGE`, sign, scale, group/elementary levels, `OCCURS`, and
  linkage layouts match the manifest and ABI; hidden result/context/
  construction/ownership fields are initialized when required.
- **Numeric operations and representation:** each typed IR operation maps to
  the profile's exact type/operation entry; operands, result type, range or
  precision, intermediate/guard strategy, conversion kind, rounding mode,
  and failure edge agree with the expectation manifest. Verify zero and
  signed-boundary guards precede arithmetic that could overflow; result
  storage is unchanged on failure; emitted `ON SIZE ERROR`, `ROUNDED`, and
  helper clauses match the declared profile evidence. Reject implicit
  truncation, host-width assumptions, unmanifested numeric helpers, or any
  picture/usage change that alters source-visible behavior. For floating
  operations also verify the declared evaluation precision and operation
  rounding boundary, direct correctly-rounded literal conversion, signed-zero
  and NaN comparison policy, subnormal handling, and absence of unrequested
  FMA/reassociation; a decimal `DISPLAY`/formatted result is not a substitute
  for representation-level validation.
- **Calls and ABI:** every `CALL` target declared or explicitly external;
  arity and by-reference argument order match the exported signature; result
  slot and exception-context ordering/version match the reconciled
  `docs/abi_spec.md` and ABI manifest; each
  `PROCEDURE DIVISION USING` agrees with `LINKAGE SECTION`; no stale helper or
  unresolved template target. For nested templates, each call must resolve to
  the exact concrete specialization in the IR manifest: an outer
  `vector<vector<string>>` lifecycle paragraph calls the
  `vector<string>` lifecycle paragraph, which calls the `string` lifecycle
  paragraph. Reject wrong-depth calls, skipped layers, accidental scalar
  operations, duplicate specialization routines, and any path that bypasses
  the child destructor or error-context propagation.
- **Exception-frame lowering:** the selected ABI manifest declares the frame
  model, depth/resource bound, frame and parent-frame fields, payload ownership
  fields, and transition protocol. Every generated frame initialization,
  child-frame selection, parent restoration, payload-complete publication,
  and clear/release operation must correspond to a logical CFG transition and
  preserve frame identity. Immediately after a throwing `CALL`, the emitted
  test/branch must inspect the designated frame before any result read, later
  argument, ordinary statement, or return; result storage is success-only.
  Handler dispatch occurs only after the payload-complete check and uses the
  manifested dynamic type/hierarchy order. Catch-by-value storage becomes live
  only after its lifecycle call succeeds. A rethrow retains the same frame and
  payload identity. Nested catches restore the exact parent frame, and an
  escaping child follows the frozen replacement transition without aliasing
  or overwriting the caught payload. `FATAL` must reach the profile-approved
  whole-run-unit termination path with no return edge, catch dispatch, or
  subsequent cleanup; no host/C++ helper may provide the behavior. The
  independent verifier checks these relationships in parsed COBOL and the
  expectation manifest, not by searching for paragraph-name markers.
- **Startup, uncaught reporting, and teardown:** independently reconstruct the
  startup and normal/exceptional shutdown CFG from emitted COBOL and compare it
  with the root-control, initializer, lifecycle, reporter, and termination
  expectations. Verify explicit root/control initialization before any
  potentially failing operation; heap readiness before other runtime services
  or managed globals; exact initializer completion/`READY` transitions; no
  entry into `main` on startup failure; and reverse destruction/finalization of
  only completed objects/services. On an uncaught exception, prove the private
  reporter reads only permitted live fields, makes no allocation or forbidden
  call, executes before payload destruction and service finalization, and
  cannot alter the original terminal category. Prove type-directed payload
  destruction and service finalization precede the heap leak check/release, and
  the profile's native process-termination operation follows teardown with no
  source-level return edge. Verify normal completion, startup uncaught,
  `main` uncaught, and fatal double-exception paths against their distinct
  portable terminal categories and pinned profile raw-status mappings. On
  ordinary entry completion, preserve its normalized result and perform
  teardown exactly once; on fatal double exception, do not emit cleanup edges
  after the fatal transition. A matching
  descriptor or trace cannot substitute for inspecting the actual COBOL
  statements and control-flow paths.
- **Activation storage:** mutable locals and lifetime/control flags are mapped
  to the selected per-invocation strategy; only immutable or explicitly shared
  state appears in `WORKING-STORAGE`. The recursive-call declaration/call form,
  activation capacity, and limit check agree with the target-profile manifest.
  For compiler-managed records, every successful push has exactly one pop on
  normal/exceptional exits, and failure before push leaves the caller frame
  unchanged. For native storage, fixtures prove simultaneously live recursive
  invocations retain distinct values and cleanup flags. The COBOL verifier
  rejects profile/strategy mismatches and shared mutable locals even if the
  generated program happens to pass a shallow non-recursive test.
- **Profile and allocation ABI:** every generated pointer, dynamic allocation,
  release, address-based declaration, numeric representation, and external
  runtime call is declared in the selected profile manifest. Allocation size
  and multiplication overflow are checked before allocation; allocation and
  matching release belong to the same declared family; unsupported facilities
  fail before rendering. No C++/host allocator or unmanifested vendor routine
  may provide generated-program behavior.
- **Control flow:** all branch targets/paragraphs exist; `IF`/`EVALUATE`/
  `PERFORM` scopes terminate correctly; no disallowed `GO TO`/`PERFORM THRU`;
  no reachable fall-through bypasses required return/stop/cleanup; labels are
  unique across linked modules; loop exits preserve source semantics.
- **RAII/exceptions:** construction-complete flags set only after success;
  cleanup destroys only completed objects, in reverse completion order, once;
  for vectors, `size` is the live range and `capacity` is only allocated
  storage; `reserve` must not call `T` constructors for spare slots; clear,
  shrink, pop, erase, and destruction dispatch exactly one destructor for each
  removed live element and none for unused/unconstructed slots. A staged
  allocation cleans only its completed prefix before rollback;
  every return/break/continue/throw/call-failure edge enters its cleanup path;
  catch payload binding and release order is correct; rethrow preserves
  payload; active exception cannot be overwritten; second throw goes directly
  to termination. For nested containers, verify a separate completion count
  or equivalent state at every layer; an outer element is marked complete
  only after its nested constructor returns successfully, and a nested element
  only after its own leaf construction completes.
- **Memory and strings:** every allocation has a matching compatible delete;
  no `malloc`/`free` mismatch; array cleanup touches only constructed
  elements; non-trivial elements are not default-constructed merely because
  vector capacity was reserved; required bounds/null checks exist; string buffer/length/capacity
  stay synchronized; literals are escaped so they cannot inject COBOL
  statements; non-trivial values are never byte-relocated.
- **Files:** each file verb refers to declared file/record; modes,
  organization/key clauses match profile; EOF and I/O errors are distinct;
  record size and error mapping match CBL-C semantics.
- **Dialect allowlist:** tokenize generated code and check verbs, clauses,
  intrinsics, directives, `USAGE`, pictures, and extensions against a
  versioned per-profile allowlist. Ignore tokens in comments/literals. Any
  deliberate extension requires a named capability, docs, and compiler-matrix
  test; never add a global unexplained vendor exception.
- **Traceability/determinism:** each emitted block maps to source or a compiler
  feature; no unresolved `T`, template marker, TODO sentinel, or placeholder;
  repeated builds with identical source/config are equivalent regardless of
  working directory, timestamps, or filesystem enumeration order.

##### Source-format normalization and generated-COBOL grammar

The independent reader has two deliberately separate stages: a profile-bound
physical-source normalizer and a tokenizer/parser for the generated subset.
Neither stage may assume that host text-file APIs, newline conventions, or
Unicode character counts equal COBOL source columns. The selected profile
declares source encoding, BOM policy, line-ending policy, tab policy, source
format, indicator/comment rules, text/reference areas, maximum source width,
continuation behavior, and whether in-source directives are allowed. Record
both byte offsets and logical token positions so diagnostics can name the
generated file/line/column and map back through the renderer source map.

The normalizer consumes the exact bytes supplied to the COBOL compiler and
performs these steps in order:

1. Validate the declared encoding/BOM and line endings; reject malformed
   encodings, disallowed mixed endings, or tabs when the profile has no
   explicit tab-expansion rule. Apply the profile's exact tab/column measure
   once; do not normalize Unicode or silently convert encodings after column
   checks.
2. Split into physical lines while retaining original byte ranges. For a
   fixed-format profile, parse the sequence area, indicator area, program-text
   area, and reference area using profile-defined 1-based columns; reject text
   that enters an ignored/reference area unless allowed by that profile. For a
   free-format profile, begin the program-text area where that profile says
   (GnuCOBOL documents column 1 through end-of-line, with an effective line
   limit, while fixed format uses sequence/indicator/program-text/reference
   areas). Do not apply fixed-format indicator or Area A/B rules to free-form
   text. The GnuCOBOL rules are a profile fact, not a universal COBOL parser
   default ([GnuCOBOL Manual, source format](https://gnucobol.sourceforge.io/doc/gnucobol.html)).
3. Interpret comments, debug lines, continuation indicators, and inline
   comment markers using that profile's lexical state. A comment marker inside
   a quoted literal is ordinary data; a quote continued across physical lines
   follows only the selected profile's exact continuation rule. GnuCOBOL, for
   example, documents fixed-format indicator values and dash continuation in
   column 7, while free format uses different rules
   ([GnuCOBOL Programmer's Guide, indicator area and comments](https://gnucobol.sourceforge.io/HTML/gnucobpg.html)).
4. Detect source-format/compiler directives before ordinary parsing. Either
   forbid them in generated output or require each directive to be represented
   by a profile-approved node whose value agrees with the manifest. Reject
   directives that switch format, dialect, decimal-point convention, encoding,
   or compiler behavior behind the profile's back.
5. Emit a logical character/token stream with origin ranges back to physical
   bytes. Tokens distinguish words (with raw and profile-canonical spelling),
   numeric literals, alphanumeric/figurative literals, punctuation/periods,
   operators, and allowed directive tokens. Literal tokens preserve their
   decoded value and exact source spelling; doubled delimiters, continued
   literals, national/hex/zero-delimited forms, and escape syntax are accepted
   only when the profile grammar declares them.

The grammar is a closed generated-language grammar, not a best-effort general
COBOL parser. Derive its nonterminal and statement inventory from the emitted
module-IR node variants and version it with the validator grammar. At minimum,
it recognizes the supported program/division/section ordering, nested data
entries and elementary clauses, linkage signatures, paragraph boundaries,
`CALL ... USING` slot modes, data references/literals, arithmetic statements,
conditions, explicit `IF`/`EVALUATE`/loop terminators, `PERFORM` targets,
explicit return/termination, generated file verbs, and only those allocation,
pointer, intrinsic, or exception-ABI constructs enabled by the profile. It
checks statement-specific operand categories and clause order rather than
merely balancing words such as `IF` and `END-IF`. A verb, clause, intrinsic,
level number, or literal form outside the emitted subset is a rejection unless
the profile manifest names and tests its capability. Recovery may collect
additional diagnostics, but recovered or error-containing trees are never
accepted for ABI, artifact, or compile/run evidence.

After parsing, normalize to the independent comparison graph: canonical
profile-folded names, exact data/layout clauses, ordered call slots, typed
literal values, explicit branch/call targets, and declared dependencies. The
normalizer must preserve enough distinctions to catch `BY VALUE` versus
`BY REFERENCE`, omitted result/context fields, changed sign/scale, wrong
program entry, and different exceptional/cleanup edges. It may ignore only
presentation facts proved non-semantic for that profile (permitted whitespace,
comments, and equivalent line wrapping). Run the same parser against hand-
written parser fixtures and mutation fixtures; do not generate its grammar
tests solely by passing module IR through the emitter, since that would repeat
the emitter's mistakes.

The current dialect note describes ANSI-85/fixed-format constraints, while
existing integration tests invoke GnuCOBOL with free-format options. Phase 0
must reconcile this into named, tested profiles rather than mixing assumptions.
The format is explicit compiler input; the profile used by the verifier and
compiler must always agree.

#### 8.7.3 Tests for the COBOL verifier itself

- **Accept fixtures:** minimal program; every emitted division/section;
  working-storage/linkage; numeric/string/class layout; each call shape;
  return slot; exception context; file declarations; cleanup paragraphs; each
  supported source format; nested vector specializations at depths 1, 2, 4, 5,
  8, and 16 with the complete manifest call graph.
- **Source-normalizer fixtures:** for each declared profile, cover empty and
  maximal physical lines, every fixed-format boundary column and indicator,
  sequence/reference areas, Area A/B restrictions where applicable, free-form
  line starts, permitted tabs/expansion, CRLF/LF policy, BOM/encoding errors,
  multibyte text around column limits, inline/full-line comments, debug lines,
  comment markers inside literals, doubled quote delimiters, fixed-format
  continued words/literals, free-format continuation rules, and source-format
  directives. Assert exact byte-to-token-to-diagnostic offsets and reject any
  compiler-visible byte that the normalizer silently dropped or reclassified.
- **Generated-grammar fixtures:** one independent hand-written fixture per
  statement/data-clause/node variant; nested `IF`/`EVALUATE`/`PERFORM` scopes;
  decimal-point/comma and literal forms allowed by profiles; period placement;
  paragraph boundaries; nested groups and `OCCURS`; call modes; and every
  supported allocation/file/exception form. Reject each unknown verb, clause,
  intrinsic, directive, malformed operand category, misplaced terminator, and
  grammar-version mismatch; recovered parser trees must not certify output.
- **Reject structure errors:** missing/duplicate/reordered divisions; missing
  `PROGRAM-ID`; duplicate or case-colliding names; malformed levels/pictures;
  missing linkage item; wrong call arity/order; missing paragraph/branch
  target; unclosed scope; malformed/continued literal; invalid columns/line
  format.
- **Reject ABI errors:** wrong picture width/sign/scale; wrong BY REFERENCE
  order; missing/extra result slot; missing/extra/mis-versioned exception
  context; undefined/stale runtime call; exported signature mismatches
  manifest or versioned implementation interface; registry entry points at a
  non-canonical/stale `.cblc` unit; nested lifecycle call wired to the wrong
  concrete type/depth; omitted intermediate specialization; duplicate/missing
  nested routine;
  leaked public status slot outside a catalogued compatibility adapter; legacy
  and native ABI versions accidentally combined.
- **Reject cleanup corruptions:** remove destructor from an exit edge; reverse
  cleanup order; set construction flag too early; destroy a partially
  constructed element; skip cleanup on return/continue/call throw; clear active
  exception before catch binding; let second throw replace first; mark a
  nested vector complete before one of its `string` elements is complete;
  omit a leaf destructor from a depth-5 or depth-16 cleanup chain; use
  `capacity` instead of live `size` for destructor/element loops; construct
  `T` in unused capacity; omit destruction after `pop_back`, `erase`, `clear`,
  or shrinking `resize`; substitute assignment/raw bytes for non-trivial
  copy/move construction; clamp an invalid insert or ignore an invalid erase.
  Each mutation must fail structural/manifest validation or an executable
  RAII/death test.
- **Reject unsafe/non-portable output:** forbidden verb/directive/vendor
  intrinsic/unsupported picture; excessive identifier/line; broken quote,
  period, or comment escaping; user text injected into paragraph/identifier;
  undeclared file/copy dependency. Put lookalikes only in comments/literals to
  prove the token-aware checker avoids false positives.
- **Manifest integrity and agreement:** valid canonical bundle with exact
  hashes; mutate one byte in every artifact; remove, duplicate, reorder, or
  forge a manifest field; insert an absolute host path, root escape, symlink
  escape, stale source hash, wrong name map, missing dependency, unknown profile,
  wrong ABI version, duplicate program/symbol ID, or unsupported digest/schema.
  The verifier rejects the altered bundle before linking and reports the
  artifact ID plus expected/observed digest. Reordered but semantically
  equivalent canonical inputs serialize identically; raw host-only evidence
  cannot change the semantic manifest digest.
- **Expectation/link-set agreement:** delete an expected COBOL `CALL`, inject
  an unexpected call, redirect to a same-basename provider, alter a
  `PROGRAM-ID`, or omit a `COPY`/runtime edge. The parsed artifact graph must
  match the typed-IR/link manifest exactly by canonical symbol/provider ID;
  neither a one-way “all expected items found” test nor directory enumeration
  is sufficient.
- **Evidence-level truthfulness:** present a manifest that claims compile,
  link, or execute success without a harness result; provide a record for the
  wrong artifact digest/profile/compiler version; forge or corrupt a release
  attestation; or mark a skipped optional-profile run as a release pass. The
  verifier reports only the highest level proven by a trusted harness/CI
  result and labels unsigned local evidence unverified.
- **Compile/run matrix:** compile each golden category with every required
  compiler/profile; retain command/version/diagnostics in temporary storage;
  link/run integration fixtures; assert exact transcript, normalized
  process-status category, any profile-promised raw status mapping, files,
  and termination. Fix warnings or allowlist by stable warning ID and narrow
  scope; do not suppress all warnings globally.
- **Mutation tests:** maintain a ledger mapping each verifier invariant to a
  stable rule ID, artifact/node family, deterministic mutator, expected
  rejection stage/code, and source/generated location. Split integrity
  mutants from semantic mutants: an integrity mutant changes bytes without
  updating the artifact digest and must fail integrity validation; a syntax,
  ABI, control-flow, lifecycle, or dependency mutant is re-sealed by the test
  fixture (recompute only integrity/container hashes) while the independent
  semantic expectation remains unchanged, so the intended parser/verifier
  rule—not a stale hash—must reject it before COBOL compiler invocation.
  Never forge compile/link/run evidence to exercise these checks. For every
  non-equivalent deterministic verifier mutant, assert the expected stable
  rule ID and COBOL line/column; a generic hash rejection does not count as
  killing a semantic mutant. Behavioral-only mutants assert the named runtime
  test and disagreement with an independent expected-result oracle. Keep
  explicit equivalent-mutant review records. Also mutate harmless
  whitespace/comments and assert acceptance only where the selected profile
  permits them.
- **Registry/generator boundary:** prove each native entry is sourced from
  its catalogued `.cblc` unit and generated interface version; reject COBOL
  template text or duplicate runtime semantics in the C++ registry/generator;
  verify compatibility adapters target the matching canonical entry.
- **Negative controls:** malformed COBOL is rejected without hang/crash; test
  cleanup removes temporary artifacts; no generated validator/test path writes
  `automated_tests.log`.

#### 8.7.4 Backend lowering and pass-boundary tests

These tests validate the compiler transformations before invoking a COBOL
compiler. They supplement, and never replace, the compile/run matrix above.
Each case should run against the typed IR, CFG, lifecycle plan, ABI manifest,
COBOL module IR, and rendered output as those representations become
available. Assign stable case IDs so failures remain comparable while the
backend is migrated from direct text emission. Phase 1 gates non-exceptional
dataflow and cleanup; Phase 4 activates throwing-call and constructor-failure
cases; Phase 6 activates the full borrow/iterator invalidation cases. A case
may be implemented earlier, but the roadmap must not claim the later gate
passed until its full required suite runs.

| Case | Input/action | Required evidence |
| --- | --- | --- |
| `BACKEND-DATAFLOW-MONOTONE-001` | For every registered object, borrow, exception, and resource-state transfer, exhaustively enumerate reduced finite state sets `A` and `B` with `A subset B` and compare `F(A)` with `F(B)`; exercise success/failure edges and branch refinements. Mutate one transfer or edge predicate to remove a still-possible fact or add a fact based on iteration order. For product domains too large for exhaustive enumeration, require a machine-checkable transfer rule classification plus generated reduced-domain instances. | Every transfer is monotone under subset inclusion and the solver starts from bottom, only accumulates facts, and reaches the least fixed point without widening. Production transfer metadata agrees with the independent rule oracle; a non-monotone mutation is rejected before CFG/lifetime results can be cached or lowered. The test records finite-height/work-budget bounds and proves limit exhaustion yields a resource diagnostic rather than partial facts. |
| `BACKEND-EFFECT-SCC-001` | Build direct, self-recursive, mutually recursive, and multi-SCC call graphs with effects in every summary dimension: typed throws, allocation, memory-region reads/writes, receiver mutation, ownership transfer, borrow creation/invalidation, external I/O, and termination. Include complete-empty, complete-nonempty, dimensionally unknown, absent, malformed, and stale imported effect records. Compare production summaries with an independent reference join solver; permute module/node/edge order, worker schedule, cache state, and profile; mutate a call edge, direct effect, throw set, completeness marker, schema version, and digest. Exercise the effect-work budget at limit-minus-one, exact, and plus-one. | Summaries equal the component-wise least fixed point of direct plus reachable callee effects; complete empty remains distinct from unknown, and unknown propagates only through the affected dimension while preventing unsound purity/no-throw/no-allocation/initializer-order claims. Canonical summaries and diagnostics are schedule/cache/profile independent. Mutations invalidate SCC summaries and transitive callers and are rejected before CFG/lifetime reuse. Budget exhaustion returns the stable resource diagnostic and publishes no partial summary. |
| `BACKEND-RESOLVE-001` | Unknown identifier, unresolved overload, missing template specialization, ambiguous member, and stale imported ABI. | Failure occurs before COBOL module creation, has the correct stable diagnostic/source span/instantiation trace, and creates no partial output artifact. |
| `BACKEND-NAME-DIGEST-COLLISION-001` | Use a deterministic test hash provider to force primary-prefix collisions, full primary-digest collisions, secondary-prefix collisions, and full secondary-digest collisions for distinct canonical keys. Include candidate collisions with fixed/reserved spellings and overlapping profile collision domains; vary key/request order, readable stems, ABI-frozen reservations, and the profile's initial digest length/name limit. Mutate collision-group membership, extension of only the last-discovered key instead of every colliding member, extension chunk size/direction, primary-versus-secondary transition, fixed-name handling, and publication after exhaustion. | All members of a private/private collision group extend in canonical-key-independent group steps until profile-canonicalized names are unique; a fixed reservation remains unchanged while only colliding private candidates extend. Primary and secondary digests use their distinct declared domains and exact hex-prefix rules. Distinct keys receive stable unique names when the secondary discriminator fits; full-digest collision or insufficient profile length produces the stable error naming every conflict before the name table is frozen/published. Results are unchanged by request order and never use traversal-order suffixes or partial publication. |
| `BACKEND-EFFECT-REGIONS-001` | Exercise effects on distinct globals/module services, local frames, receiver/formal borrows, fields, container elements, heap allocation sites, shared/escaped handles, and unknown imported calls. Test direct/transitive calls with formal-to-actual region substitution, aliased arguments, and initializer reads/writes; mutate one region mapping, escape fact, claimed-disjoint pair, `memory_region_schema_version`, `alias_policy_version`, or serialized region record. | Summaries preserve canonical region IDs and conservatively join every possible alias. Only verifier-proven disjoint fields/indices may be treated as disjoint; unknown or escaped state blocks unsafe reordering and produces the required global-dependency rejection. Caller summaries, initializer edges, and `.cbi` effect records agree. Stale/incompatible versions are rejected or widened exactly as declared and invalidate affected caches/link effects. A mutation cannot remove a possible read/write, alias, or initialization dependency and is caught before optimization or link ordering. |
| `BACKEND-RELEASE-MATRIX-001` | Generate CI jobs from a versioned matrix containing required Windows/POSIX common-profile rows, supported vendor/compiler rows, informational rows, and explicit unsupported pairs. Mutate/remove a required row, relabel it informational/unsupported, change its compiler/profile/options/test set, omit a generated CI job, claim a skipped job passed, reuse stale evidence, or forge the workflow/tool identity. Change only the matrix version and compare artifact versus evidence keys. | Every declared required row maps to exactly one trusted job and evidence record bound to the same source/artifact manifest, profile, compiler identity, workflow, and test-data/plan digests. Missing, skipped, stale, untrusted, or relabeled required rows fail release; informational and local `unsupported_here` results never substitute. Supported host rows preserve generated semantic artifacts and pass the required compile/link/run tests; matrix-only changes invalidate evidence/attestation but preserve generated artifact identity. |
| `BACKEND-LINK-ORDER-001` | Construct provider/runtime dependency DAGs with multiple valid orders, shared modules, profile-declared ordered libraries, and independent executable roots. Permute discovery and worker completion; mutate one ordered unit, violate a dependency constraint, duplicate an artifact, or substitute a catalog version. | Each root's explicit link-unit sequence is a deterministic topological order with canonical-ID tie breaks and preserves any declared tool/profile ordering constraints. The linker receives exactly that sequence; no host filesystem or argument sorting changes it. Invalid or stale order/catalog data fails before tool invocation, and equivalent graph input permutations produce identical per-root link plans. |
| `BACKEND-LINK-MANIFEST-VERIFY-001` | Independently validate hand-authored single-root and multi-root link manifests; mutate root/entry identity, module and callable records, edge kinds/coverage, provider artifact hash, ABI/profile tuple, frozen `PROGRAM-ID`, runtime catalog version, initializer sequence, heap-domain descriptor, root exception-control/reporter paragraph IDs, reporter ABI/schema/output-policy digests, and exception closure including externally mapped types/base/lifecycle providers and callback exception edges. Add missing, extra, duplicate, stale, and cross-root outputs; attempt to inject a file through wildcard/search-directory discovery. | The verifier recomputes each root's exact reachable closure and all dependency/runtime/initializer/heap/exception aggregation from typed calls and validated `.cbi` records, including external status/availability mappings, callback throw/translation contracts, and exception-aware external throw sets; it also verifies the root reporter/termination tuple against the selected profile and exact startup program artifact. It verifies exact call resolution and artifact identity and rejects every mutation before linker invocation. Canonical input reordering yields the same digest; root-specific exception, heap, reporter, and termination records remain separate while shared verified providers may be referenced by both. The adapter passes only the manifest's ordered exact artifact set; no unlisted file can enter the link. |
| `BACKEND-CROSS-MODULE-EXCEPTION-DOMAIN-001` | Compile provider and consumer in separate compiler invocations using serialized `.cbi` interfaces. Throw a provider-defined concrete exception, catch it in the consumer by exact type and each supported accessible base view, and include a same-spelling/different-module exception, a structurally identical exception from another module, and a concrete generic throwable. Mutate the canonical module/declaration/template-argument identity, runtime dispatch ID, collision discriminator, payload descriptor, base-subobject projection, lifecycle owner/visibility, interface version, and one provider/consumer copy independently; force runtime-ID collisions with the deterministic test hash provider. Also add a reachable external status adapter whose mapping introduces a throwable and an unreachable adapter that must not enter this executable root's closure. | Importing the same declaration preserves canonical source identity across modules; same spelling or payload layout never aliases distinct declarations. Provider, consumer, link manifest, and generated dispatcher agree on the complete canonical identities, versioned IDs, payload/lifecycle descriptors, and verified base projections. The caught base view addresses the specified subobject and invokes only the exported lifecycle entries. Every incompatible or unresolved record and unresolvable dispatch collision fails before rendering/link with both conflicting identities; no name-based or catch-all fallback occurs. The reachable external mapping is included with provenance in this root's exception closure, while the unreachable mapping is excluded. Separately compiled provider-consumer COBOL compiles and executes with the expected handler and exact construction/destruction trace on every claimed profile. |
| `BACKEND-COORDINATOR-FAILURE-ARBITRATION-001` | Run the same frozen ready-frontier DAG with worker counts 1, 2, and the configured maximum while deterministically permuting task completion order. Arrange independent tasks to fail with distinct typed causes, a dependency-closure failure, task-local failure, malformed output, and invocation-fatal failure; include descendants that must be `blocked`, independent tasks that must continue, and a cancellation arriving before dispatch, during a frontier, at its barrier, and after a completed failure. Inject quota exhaustion at each canonical reservation boundary and fail/cancel tasks before and after scratch-output verification/publication. | Admitted task sets, quota reservations, terminal outcome, primary diagnostic, complete failure-cause set, and ordered diagnostics are identical for equivalent inputs regardless of worker count or completion schedule. Every task receives exactly one declared outcome (`completed`, `failed`, `blocked`, `cancelled`, `not_admitted`, or `not_applicable`) with correct root-cause/provenance; blocked descendants are not fabricated as source failures, and independent admitted tasks finish. Cancellation and failure follow the state-machine linearization contract and never erase each other's evidence. No incomplete task output becomes visible or cached; limit exhaustion emits no partial semantic result. Host crash/exhaustion remains a distinct operational result. |
| `BACKEND-LINK-ROOT-001` | Build one project graph with two executable roots sharing a provider/runtime module, root-specific providers, and an unrelated module. Vary root selection/order, module discovery/completion order, and add or remove the unrelated module. Include zero-entry and multiple-entry roots, unresolved root calls, and conflicting runtime/heap descriptors. Compile, link, and execute each root independently, then run both executables concurrently. | Each selected root has exactly one declared entry point and a separately derived transitive link closure; shared modules may reuse verified artifacts, but unrelated modules and the other root's private dependencies are not linked implicitly. Each final executable receives exactly one compatible heap/runtime domain, with no handle or mutable service state shared across executable instances. Root identity and closure are bound into the link manifest/artifact key; order changes do not alter equivalent outputs, and unrelated modules cannot rename entries or perturb the other root's closure. Missing/multiple entries, unresolved calls, and descriptor conflicts fail before COBOL link with root/module/symbol context. |
| `BACKEND-CFG-PLAN-VERIFY-001` | Corrupt one control-region certificate field at a time: CFG/lifetime/profile digest, schema/algorithm version, region parent/member/boundary, edge disposition (omitted/duplicate/wrong kind), dominator/postdominator witness, dispatch ordinal/range, synthetic-node inverse projection, or resource-charge summary. Also claim a reachable block is unreachable and reuse a plan after changing only a relevant profile capability. | The independent verifier recomputes CFG facts and edge coverage, rejects every mutation before module IR, and never trusts the structurer's copied proof sets. Every reachable block and source edge has exactly one valid lowering/disposition; synthetic protocol nodes project back to the exact semantic CFG. Stale plans are invalidated by the declared key dependencies, and the module-IR builder accepts only a verified plan digest. |
| `BACKEND-CFG-POSTDOM-001` | Verify dominator/postdominator facts for branching graphs with distinct return/throw/termination exits, closed non-terminating SCCs with no finite path to the virtual exit, mixed SCCs with both finite exits and back-edges, and branches whose one arm diverges while another reaches a merge. Compare with an independent graph oracle; mutate virtual-exit wiring, finite-exit reachability, successor filtering, and a divergence-edge classification. Compile/run structured and dispatcher forms where supported. | Dominators and finite-exit postdominators match the equations and oracle exactly; exit kinds remain distinct in the CFG, and the analysis-only exit is never emitted. Non-finite-exit nodes have no fabricated merge facts; possible infinite execution is preserved, not converted into a fallthrough or forced exit. Divergence edges are accounted for once in the region certificate. Mutations fail before module IR, and supported lowering strategies have equivalent execution traces. |
| `BACKEND-IR-001` | Scalar assignment, load/store through a field/element place, method/operator call, construction, copy, move, destruction, allocation, checked access, throw, and file call. Include owned and borrowed results of the same static type, concrete operations, and type-parameter-dependent operations plus each operand-use mode. | Each action has a typed operation with canonical input/result IDs, structural types, `Place` projections, source origins, effects, operand modes, and either an exact concrete target or a complete canonical dependent-lookup record plus substitution environment. Owned value, borrow, handle, and place are distinguishable; may-fail results exist only on the success edge; copy/move/borrow use the exact selected operation/ownership mode when concrete; no semantic operand is opaque expression text or an allocated COBOL name. |
| `BACKEND-IR-ADAPTER-001` | Adapt legacy call/assignment text containing nested calls, commas inside nested arguments and literals, member/index chains, escaped quotes, and operands at the fixed-field limit minus one/at/plus one. | The compatibility adapter parses each operand exactly once into typed values/places, preserves argument boundaries and source spans, and either produces a complete sidecar or a stable diagnostic; it never silently truncates, splits on a comma inside nesting/literals, or leaves downstream consumers reparsing the original text. |
| `BACKEND-IR-VERIFY-001` | Corrupt one invariant at a time: duplicate/use-before-definition ID, non-dominating use, wrong type/category, invalid projection, malformed dependent lookup, unresolved concrete overload after specialization, missing effect, absent exceptional successor, malformed merge inputs, reordered side effects, and surviving opaque text after the adapter. | Before specialization, a dependent lookup is accepted only when its canonical candidate context, dependent receiver/argument types, and substitution operands are complete; malformed or ambiguous non-dependent calls fail. After specialization, every call/operator/lifecycle target is concrete and unresolved dependent lookup is rejected before CFG construction. All other verifier failures identify the pass, operation, and provenance; no failure falls back to text-based emission. |
| `BACKEND-TRANSFORM-001` | Run the identity path and each explicitly enabled IR/CFG transform against disjoint pure operations, aliased reads/writes, observable calls, allocation/throw failures, borrow invalidation, construction/destruction, and cleanup edges. Mutate the transform to reorder, erase, duplicate, retarget, or combine one effectful operation without updating its proof/provenance. | No transform runs unless opted in with declared invariants and proof obligations. Legal transforms preserve the observable operation/failure/ownership trace and complete provenance; invalid rewrites are rejected by the pass-specific proof or post-transform verifier. Changes to call targets, order, effects, or CFG force all affected effect, CFG, and lifetime analyses to rerun. An unavailable optimization leaves valid source IR unchanged; the renderer cannot repair or optimize it. |
| `BACKEND-PASS-REGISTRY-001` | Validate complete registries containing valid producer/consumer chains and fixed-point passes, including the concrete backend artifact DAG from typed IR/CFG through profile projection, request closure, names/ABI, pre-module-IR expectations (including executable-root startup/shutdown and uncaught-report expectations), self-verified module IR, rendering/source maps, independent output parsing, correlation, and compiler evidence. Then inject duplicate IDs/owners, missing producers, incompatible schemas, undeclared live/environment inputs, output aliasing, missing or extra DAG edges, expectation-to-module-IR or renderer-to-expectation cycles, parser consumption of emitter objects, correlation feedback into generation, mixed artifact/evidence outputs on one pass, invalid fixed-point declarations, lane conflicts, omitted failure mappings, and unsupported descriptor versions. Independently mutate reporter descriptors and semantic expectations to prove the expectation producer consumes verified CFG/lifecycle/profile inputs rather than module IR, parsed output, or the descriptor's self-asserted behavior. Change artifact-producing and validation-only descriptors independently while holding their upstream inputs fixed. | Invalid registry/DAG is rejected deterministically before any pass starts, with the descriptor and broken edge identified; no partial registry or pass output is published. The accepted graph has exactly one producer per artifact and the expectation/render/parser/correlation boundaries remain acyclic with their declared forbidden dependencies absent. Startup/shutdown expectations have one pre-render producer and their renderer/parser consumers cannot feed changes back into expected values. A validation-only descriptor change updates the full registry/evidence identity but preserves `artifact_pass_registry_digest`, `generation_key`, and generated artifacts; an artifact-producing descriptor change updates the artifact registry digest and invalidates only its transitive artifact consumers. Unrelated verified pass-cache entries remain reusable. Valid fixed-point work is confined to its declared pass and convergence budget; worker/schedule telemetry cannot alter semantic artifacts. |
| `BACKEND-PASS-INVALIDATION-001` | For each row of the pass invalidation matrix, change exactly one source/interface/rule, generic body/target, effect/order/CFG/lifecycle, logical ABI, profile/layout, heap-handle/domain/pool descriptor, control-region strategy/schema, name-policy, uncaught-report semantic/output policy, reporter paragraph descriptor/schema, or renderer input. Repeat from clean and warm-cache builds; inject stale, missing, corrupt, and mismatched pass artifacts; cancel/fail a pass before publication; vary worker completion order. | Exactly the dependent analyses/artifacts are invalidated; unrelated and downstream-only-independent results are reused only when complete input digests, pass/schema versions, and verifiers match. A reporter paragraph/output-policy change rebuilds the root expectation, name/module-IR/rendered COBOL, artifact manifest, and downstream validation evidence while preserving target-neutral typed IR and unrelated CFG/lifecycle analyses; if the change alters termination or exception-lifetime semantics, all dependent effect/CFG/ABI analyses also invalidate. A validation-only parser/correlation change reruns evidence without changing reporter COBOL or artifact identity. Profile-only changes preserve target-neutral semantic/specialization IDs; renderer-only changes preserve semantic/CFG/lifetime/ABI inputs while regenerating text/maps and compiler evidence. A semantics-preserving CFG-to-COBOL strategy change rebuilds private region/module/rendering artifacts but not the verified CFG, lifetime proof, or ABI fingerprint; edge/order/effect changes invalidate CFG/lifetime too. Heap-domain changes rebuild the merged service/link artifacts but preserve semantic specialization IDs unless object semantics changed. Clean and incremental final artifacts, diagnostics, and observed behavior agree. Failed/cancelled passes publish neither outputs nor cache entries, and dependents never consume partial results. |
| `BACKEND-CACHE-STORE-001` | Exercise absent, valid, evicted, truncated, wrong-schema/key/profile, digest-corrupt, path-escaping, and verifier-rejected entries; race identical and different outputs under the same key; kill writers at each write/flush/bind boundary; evict while readers hold leases; inject cache read/write permission and capacity failures. | Only fully verified immutable entries are consumed. Partial writes are never visible; identical writers converge; same-key divergent output fails nondeterministically; corrupt entries are isolated and recomputed from frozen source. Cache failure/eviction changes no source diagnostics, semantic output, or behavior, and cannot block cold recomputation; cache telemetry stays outside semantic manifests. Live readers are protected from eviction, and output/publication-store failures remain fatal and distinct from cache I/O outcomes. |
| `BACKEND-CACHE-DIAGNOSTIC-001` | Build a successful pass with warnings/notes, then compare cold and warm runs under the same diagnostic catalog, cache eviction/recompute, a message-template-only catalog revision, and relocation of the live checkout while retaining the same frozen source snapshot. Corrupt or remove one diagnostic record, change its provenance/source hash, duplicate an instance key, and attempt to cache an incomplete failed-pass output. | Under the same catalog, cold and warm runs produce byte-identical normalized diagnostic records and rendered messages. A template-only catalog revision may change presentation text but preserves stable code/instance keys, typed parameters, severity, order, and source mapping without rerunning semantic analysis. Infrastructure cache telemetry remains separate. Missing, malformed, duplicate, or stale-provenance diagnostics reject the whole entry and trigger recomputation; failed/partial pass results cannot become cached success or an empty artifact. |
| `BACKEND-PARALLEL-ISOLATION-001` | Run two or more compiler invocations/modules in one process with deliberately interleaved parsing, template substitution, standard-library usage collection, reverse translation, diagnostics, cancellation, and failure cleanup. Exercise each pass at one worker and at its maximum declared concurrency across independent tasks; compare worker counts 1, 2, and N and run under a data-race detector where supported. | `process_serial`/`invocation_serial` passes never overlap beyond their declared lane; proven `parallel_safe` tasks do overlap in a controlled fixture without sharing mutable state. Each task's outputs, source diagnostics, resource charges, and caches match isolated serial baselines regardless of interleaving; failure/cancellation in one invocation cannot leak parser/template/diagnostic state into another. Missing descriptors default to serialization, and telemetry/worker count never changes semantic manifests. |
| `BACKEND-SPECIALIZE-ORDER-001` | Instantiate generic methods and containers over `int`, `string`, a user RAII class, `vector<string>`, and `vector<vector<string>>`; include dependent overloads, conversions, and construct/copy/move/destroy operations. Attempt CFG construction with one unresolved dependent operation and with a post-specialization callee changed. | The typed source IR records complete dependent operations. Structural specialization resolves each to one concrete callee/effect/target-neutral layout before CFG creation; the CFG references exactly those IDs. CFG construction is rejected if specialization is incomplete, and downstream passes reject any substituted callee/type that differs from the frozen specialization graph. Nested call edges identify every intermediate specialization in order. |
| `BACKEND-SPECIALIZATION-CYCLE-001` | Compile a concrete generic function that recursively calls the same specialization; mutually recursive concrete functions; a generic expansion chain such as `F<T>` requesting `F<vector<T>>`; an indirect ownership/lifecycle chain with a valid runtime base case; and illegal by-value recursive type/layout dependencies. | Same-key recursion reuses one specialization node and forms an explicit call edge/SCC; mutual recursion is represented without duplicate bodies. Expanding type arguments fail only when the configured depth/work bound proves the chain is excessive, with the full instantiation trace. Valid indirect lifecycle recursion remains an explicit runtime SCC and destroys a finite counted chain exactly once. Illegal by-value layout/lifecycle cycles fail independently. No partial specialization graph or CFG is published, and valid recursive call/effect summaries converge deterministically. |
| `BACKEND-SPECIALIZATION-DETERMINISM-001` | Discover the same specialization graph in serial and parallel modes, with reversed module/file order, different worker completion schedules, cold/warm caches, and multiple target profiles; include duplicate requests and recursive same-key edges. | Canonical specialization keys, final semantic IDs, graph edges, effect summaries, and diagnostics are byte-stable across schedules and profiles. Profile-specific ABI/layout/name projections may differ but cannot alter semantic specialization identity. Equivalent requests intern once; collision handling compares full keys; resource-limit failures select the same canonical module/key independent of discovery order. Cache records reproduce the same validated graph and resource charges as fresh specialization. |
| `BACKEND-EVAL-ORDER-001` | Chained receiver/index/argument expressions with observable side effects, including `outer.at(i).at(j).append(value)`, plus a branch expression whose result merges from two paths. | Each expression evaluates exactly once in the specified source order; index checks and borrows precede later receiver/argument calls; each `.at()` returns a borrow and no intermediate copy occurs; merge inputs are explicit and typed; failure on an earlier check prevents all later side effects and leaves no usable result. |
| `BACKEND-LOGICAL-ORDER-001` | For each adopted `&&`/`||` contract, exercise all left truth values with a right operand that increments a counter, mutates an object, allocates, performs checked access, or throws; include nested mixed operators and injected eager-evaluation mutation in generated COBOL. | Under the recommended left-to-right short-circuit contract, each left operand runs once; `&&` skips its RHS for false and `||` skips its RHS for true; skipped effects/errors do not occur. Evaluated RHS operations appear on the correct CFG branch with typed merges and exception edges. The independent artifact/CFG validator and execution trace reject eager evaluation, duplicated evaluation, wrong precedence, or target-profile-dependent behavior. |
| `BACKEND-CFG-001` | Branches, nested loops, return from nested scope, `break`, `continue`, try/catch, rethrow, and may-fail calls/access/allocation/construction; mutate one terminator, declared outcome/successor, edge kind/target, predecessor list, merge input, or failure-only result use per test. | Every block has exactly one explicit terminator and no implicit fallthrough; predecessor/successor edges are reciprocal and complete; every may-fail operation has all and only its declared outcomes; invoke results exist only on normal edges; merge inputs agree in type/category/ownership for every predecessor; unreachable blocks are identified; malformed or missing edges fail before lifetime analysis. |
| `BACKEND-CFG-LOWERING-001` | Lower verified reducible and irreducible CFG fixtures using every profile-supported strategy, including nested loops, multiple latches/exits, multi-entry cyclic SCCs, branch arms with/without a common postdominator, break/continue, typed catch dispatch, invoke success/failure, cleanup blocks, return, and fatal termination. Repeat with permuted block/edge storage order and worker schedules. Mutate dominator/postdominator facts, loop membership/latch, region certificate, predicate evaluation count, block ordinal/width, dispatch arm/target, cleanup route, result read on failure, or paragraph fall-through; execute both structured and dispatcher forms where available. | Region classification and strategy choice are deterministic and match an independent graph oracle; every edge is assigned exactly one region/terminator role. Reconstructed output edges preserve the source CFG exactly after removal of declared lowering-protocol nodes. Predicates and calls execute once in source order; every success/failure edge and cleanup runs exactly once; no implicit fall-through or invalid state can reach arbitrary code. Both supported strategies produce the same reference trace/object ledger. Unsupported CFG/profile pairs fail before rendering with `unsupported_control_flow`, not a silent fallback. |
| `BACKEND-CFG-BUDGET-001` | Exercise each CFG/control-region work dimension at limit-minus-one, exact limit, and limit-plus-one using long chains, high-fan-out diamonds, nested/multi-latch loops, irreducible SCCs, exceptional/cleanup-heavy graphs, and graphs with many candidate postdominators. Repeat with block/edge storage permutations, worker counts, and warm/cold caches. Mutate one charge increment, work-unit schema version, counter overflow, configured target nesting limit, and dispatcher ordinal width. | Identical canonical CFGs produce identical per-dimension charges and strategy plans independent of traversal/storage order. Limit-plus-one fails before publishing a partial plan with `resource_limit_exceeded` naming dimension, limit, observed count, pass, and stable origin; it is not mislabeled `unsupported_control_flow`. Graphs within compiler budgets but unsupported by all declared profile strategies fail specifically with `unsupported_control_flow`. Structured lowering is chosen deterministically when valid and profile-feasible; dispatcher fallback occurs only for a recorded structured-representation/profile limitation, never because a budget was exceeded or a host-dependent heuristic preferred it. |
| `BACKEND-ENTRY-EXIT-001` | Run `void main()` with explicit return and fallthrough; `int main()` with zero, nonzero, and fallthrough; local/imported `void`/value-returning callees called through multiple caller levels; uncaught startup and `main` exceptions; and fatal double-exception termination. Mutate lowering to use whole-run-unit termination for a callee, return to a caller after entry completion, publish entry status in an ABI result slot, collapse ordinary uncaught and fatal outcomes, select the wrong profile raw status, or treat nonzero `main` result as success. Compile/link/run under every supported profile; include invalid entry return type/value-form rejection. | A callee returns to its exact call continuation once, its result is success-only, and caller side effects after the call execute once. Entry completion ends the run unit and never resumes a caller; `void` and zero completion normalize to success while nonzero completion normalizes to failure. Startup uncaught, `main` uncaught, and fatal double-exception outcomes match their separately pinned portable categories and profile-specific raw status mappings; raw host exit codes remain evidence, not cross-profile semantics. Invalid signatures fail before rendering; entry status is not an ABI result slot, and normal entry completion, ordinary return, uncaught exception, and fatal double-exception termination remain distinct outcomes. |
| `BACKEND-ACTIVATION-ISOLATION-001` | Execute self-recursive, mutually recursive, cross-module recursive, and each declared callback/re-entry form at depths 1, 2, the configured limit, and limit+1. Keep distinct values alive in scalar/class/`string` locals, receiver and argument temporaries, partial-construction flags, catch parameters, return slots, and exception frames at every depth; throw/catch/rethrow at inner and outer depths. Mutate the profile activation strategy, frame layout/capacity, local-to-storage mapping, push/pop ordering, limit check, or emit one live local/flag/frame slot in `WORKING-STORAGE`; re-seal semantic mutants. | Each live invocation observes its own storage and frame identity; return values, reverse cleanup, nested handler restoration, and exact destructor counts match an independent activation ledger. Limit+1 is rejected before callee entry with the frozen typed outcome and caller state intact. Native and compiler-managed COBOL strategies produce equivalent source-visible results within the same declared bound. Every storage/transition mutation is caught by the independent manifest/COBOL verifier or execution trace; profiles lacking proven activation isolation reject recursive/re-entry use before rendering. |
| `BACKEND-DATAFLOW-001` | Objects are constructed, moved, assigned, or destroyed on only one branch before a join; include loop back-edges, fresh/stale borrows around invalidation, repeated invalidation, exceptional edges, partially constructed aggregates, and dynamic containers whose live count differs from capacity. Generate bounded small CFGs spanning straight-line, diamond, nested-branch, reducible-loop, multi-latch, and irreducible-SCC shapes, with finite object/borrow states and success/failure transfer functions; compare production worklist results against a deliberately simple test-only reference solver that applies transfer functions to a fixed point without using production scheduling, bitset, or join code. Mutate CFG edges, a transfer outcome, cleanup order, and one incoming/outgoing state in the emitted analysis manifest. | Per-object state sets join by union and match each reachable predecessor; the reference solver and production solver agree on reachability and every block/edge fact for all generated fixtures. Definitely-live and maybe-live facts are correct. Invalid use is rejected; path-dependent destruction is guarded or split; completion flags are set only after success and cleared once. Borrow facts converge in a finite lattice; fresh borrows after mutation are valid while prior borrows stay invalid, with no unbounded epoch counter or stale-borrow revival. Container cleanup ranges use live count, never capacity. Every mutation is rejected by the verifier or detected by comparison to the reference result. Reversing block discovery order, predecessor order, worker schedule, and cold/warm cache state preserves the fixed point and diagnostic order; exceeding the charged transfer budget yields the same structured resource-limit diagnostic and publishes no analysis artifact. |
| `BACKEND-LOOP-LIFETIME-001` | Construct a counted local in nested loop bodies, then exercise normal iteration, `continue`, `break`, return, and throw. | Loop dataflow reaches a deterministic fixed point; each iteration destroys only its completed locals before re-entry; `continue` and `break` preserve correct scope cleanup and do not duplicate or skip destruction. |
| `BACKEND-CLEANUP-001` | Counted objects constructed at different points in nested scopes, then exit through normal completion, return, break, continue, constructor failure, call failure, throw, or rethrow. | Edge-specific live-object sets are correct; cleanup is reverse completion order; only fully constructed objects are destroyed; each object is destroyed at most once; active exception identity survives cleanup. |
| `BACKEND-CTOR-FAIL-001` | Throw during base, member, array-element, and nested-container construction at every construction position. | Parent object becomes live only after full constructor success; failure cleans the completed subobject prefix in reverse order and never invokes the incomplete parent's destructor; outer scopes clean only previously completed objects. |
| `BACKEND-RETURN-ORDER-001` | Return an owning class/container from a prvalue, a named local, an explicit move, a conditional/merged expression, and a nested call while other locals and temporaries are live. Cover copy-only, move-only, and copy-plus-move types; inject failure at each selected result construction/move and any declared ABI-marshalling construction. Inspect typed IR, semantic CFG, lifecycle call graph, and physical ABI lowering. Mutate the selected operation, source moved-from state, result destination, elision decision, construction-completion flag, cleanup order, or emitted result-slot mapping. | The semantic rule selects one exact copy/move/direct-construction path before profile lowering; no backend/profile-specific elision or extra copy occurs. The caller-provided abstract result is fully constructed before local cleanup, maps to exactly one ABI result slot, and becomes readable/owned only on success. Source moved-from state and destructor obligations match the selected operation. Locals and temporaries then die in reverse completion order; failure cleans only completed result subobjects and live locals, leaves no published result, and propagates through the correct exception edge. Post-ABI lowering preserves the call graph, order, completion state, and ownership transfer; every mutation is rejected by independent CFG/ABI/COBOL verification or contradicted by the execution ledger. |
| `BACKEND-EXCEPTION-EDGE-001` | Throwing direct/imported/method/constructor calls inside nested `try` scopes with intervening managed locals and typed catches. | Call failure targets the innermost eligible handler after required cleanup; payload remains active through cleanup and catch binding; unmatched propagation reaches the caller; a second exception terminates without replacing the first. |
| `BACKEND-EXCEPTION-CONTEXT-001` | Execute each legal context/frame transition (`EMPTY`→`RAISING`→`PROPAGATING`, handler binding/entry/completion, cleanup, rethrow, propagation, and `FATAL`) in local and cross-module calls. Inside an active catch, exercise a nested throw caught by an inner `try`, then a child exception escaping the handler as a replacement. Make the replacement expression itself throw (caught inside the nested scope and escaping it) separately from failure while constructing/copying/moving the replacement payload. Mutate transition order, active/type/payload-complete fields, context/frame version, parent-frame identity, owner identity, clear/detach/reparent count, and cleanup-before-parent-release order; re-seal semantic mutants. | The logical CFG, ABI manifest, generated COBOL, and independent runtime trace agree on every transition. Expression-evaluation exceptions use the still-`EMPTY` child frame and ordinary nested-handler dispatch; replacement-payload construction failure in `RAISING` is fatal and exposes neither partial child nor parent as a replacement. A completed child is published before handler cleanup; only after no-throw cleanup is the caught parent released once and the child reparented for propagation. Rethrow preserves identity/type without a child payload. Nested caught exceptions clear only their child and resume the parent. Any forbidden transition or failure during `RAISING`, `PROPAGATING`, or `CLEANING` reaches terminal `FATAL`, preserves the original identity, and cannot dispatch another catch or return. Unsupported forms fail semantic analysis. Every mutation is independently detected before execution or contradicted by the trace. |
| `BACKEND-EXCEPTION-FRAME-BOUND-001` | Compute per-activation frame bounds for nested handlers across branches/joins, loops, nested `try` scopes, caught child exceptions, replacement throws, rethrow, cleanup edges, and calls whose own exception is caught in the caller. Reorder CFG discovery; mutate one push/pop, parent rebind, exceptional edge, loop back-edge state, merge stack, or reported bound; forge the layout's slot count and profile maximum. Test exact-limit and one-over functions and re-seal both CFG and layout artifacts. | The independent verifier recomputes the maximum live frame-owner stack from the exception CFG. Calls count against their own activation only; all merges and back-edges have balanced owner/state stacks. The exact-limit function is accepted with one primary plus required child slots; one-over is rejected before COBOL rendering. No valid runtime path allocates frames or exceeds the proven layout, and a forged bound/layout fails even when hashes are recomputed. |
| `BACKEND-CATCH-BIND-FAIL-001` | Catch a managed class payload by value and inject a throwing copy-constructor/lifecycle failure at every base, field, nested element, allocation, and payload-copy step; include nested and cross-module catches with observable handler and post-call statements. Also mutate lowering to mark the catch local live before its constructor succeeds, enter the handler early, or route the secondary failure to an outer/sibling catch. | Catch-by-value construction occurs while the original payload remains context-owned and before handler-body entry. A failure cleans only the completed catch-parameter subobjects, then takes the fatal double-exception path: no handler body, outer/sibling handler, caller continuation, or unrelated cleanup runs; the original payload is not overwritten or destroyed twice. On success, the catch local is destroyed before the original payload is released. The runtime trace proves exact construction/destruction counts and whole-run-unit termination. |
| `BACKEND-FATAL-TERMINATION-001` | Trigger a second exception during propagation/cleanup in `main`, a local subprogram, and an imported provider; place a catch and observable statement in each caller after the failing call. Run every exception-enabled COBOL profile. Mutate lowering to `GOBACK`, normal-status termination, returning to the caller, invoking a host `abort`, or executing a handler for the second exception. | The generated COBOL uses only the profile-declared native fatal mechanism and terminates the whole run unit with the exact profile-normalized fatal result; no caller catch, following statement, or remaining cleanup runs, and the first exception is not replaced. Raw host exit/signal data is retained separately. Any profile unable to prove process-level non-success is rejected for this conformance gate; no C++/OS helper supplies program semantics. |
| `BACKEND-EXCEPTION-COBOL-LOWERING-001` | Lower nested typed/catch-all handlers, reference and value catches, rethrow, nested throws in a catch, cross-module propagation, and fatal double-exception paths through every supported exception profile. Independently mutate the rendered COBOL/serialized module IR to reorder or omit the context `CALL` slot, move the post-call exception check after result/side-effect use, change a frame or parent-frame reference, publish an incomplete payload, enter a handler before type/payload validation, mark a catch value live before construction succeeds, clear the wrong frame, alter a cleanup continuation, add a `FATAL` return/handler edge, or bypass native termination. Re-seal mutated artifacts so semantic checks—not stale hashes alone—must reject them. | The independent COBOL parser reconstructs the exact frame fields, calls, state checks, type dispatch, cleanup edges, and fatal terminal path and compares them with the logical CFG/ABI expectation manifest. Every semantic mutation is rejected before external COBOL compilation with the relevant stable verifier rule and source/generated location. Unmutated fixtures compile and execute with an independent event ledger proving immediate exception checks, parent restoration, payload identity, exact cleanup, and whole-run-unit fatal termination. Each profile's claimed native termination mapping is independently observed; unsupported profiles fail closed. |
| `BACKEND-BORROW-001` | Hold borrows/references/iterators across owner destruction, vector reallocation, erase, clear, move, and nested `.at()` chains. | Invalid use is rejected statically where provable or raises the specified `iterator_error`/borrow diagnostic at runtime; valid borrows preserve object identity and do not materialize copies. |
| `BACKEND-LIFECYCLE-001` | Construct/copy/move/assign/destroy scalars, class members, arrays, `string`, `vector<string>`, and nested vectors. | Each lifecycle op names the exact concrete canonical type operation selected during specialization and is attached to the corresponding concrete CFG operation/edge. Allocation does not imply construction; capacity does not imply liveness; non-trivial values never lower to raw byte assignment. |
| `BACKEND-ABI-001` | Direct, imported, native-library, static/instance method, constructor/destructor, return-value, and throwing calls at each supported ABI version. | Caller and callee agree on symbol, signature, receiver presence/type/mode, explicit argument order, result slot, exception context/payload, ownership, and version. Mismatch is rejected before rendering; compatibility status parameters appear only for catalogued adapters. |
| `BACKEND-ABI-ARGUMENT-FAIL-001` | Call local and imported functions/methods with a side-effecting receiver and mixed scalar, borrowed, and by-value RAII arguments. Inject evaluation/construction failure at the receiver and each explicit argument position; also exercise success followed by normal return, callee throw, and rethrow. Count evaluation, copy/move construction, ownership transfer, and destruction; mutate the call edge to enter the callee before all arguments are ready or to clean a transferred parameter in both caller and callee. | Receiver and arguments evaluate once in the specified order; failure at position N prevents later evaluation and callee entry, destroys only completed caller-owned temporaries in reverse order, and preserves borrowed originals. On success, each by-value parameter transfers ownership exactly once at the declared callee-entry boundary; the callee alone destroys it on every exit, while the caller cleans only retained temporaries. Physical linkage slots and exception context match the canonical descriptor, and each mutation is caught by CFG/ABI verification or the executable lifecycle trace. |
| `BACKEND-RECEIVER-ABI-001` | Lower static methods, const and mutable instance methods, constructors, destructors, assignment operators, base-subobject calls, and imported class methods; inject constructor failure before and after completion. | The descriptor has no receiver for static methods and exactly one typed receiver for each instance/lifecycle entry. Ordinary methods borrow the existing receiver with the declared access mode; constructors receive uninitialized destination storage and publish liveness only on success; destructors receive a live borrow and never own the enclosing storage. Local and external lowering preserve receiver identity without an implicit copy, omitted slot, or wrong base projection; constructor failure never invokes destruction on an incomplete object. |
| `BACKEND-ABI-DESCRIPTOR-001` | Emit composed descriptors for scalar, class, pointer/borrow, template-specialized, imported, and exception-throwing signatures; mutate absent/unknown/present states, receiver presence/type/mode/projection/lifecycle state, logical argument/result/context/payload contracts, physical layouts, profile, entry spelling, ownership, `logical_call_contract_version`, `interface_version`, `abi_version`, and descriptor schema version independently. | Pass 6 records logical/source contract versions and receiver facts but no physical `abi_version`; Pass 7 reads and validates the export-pinned version against the exact profile projection before projecting physical slots. Pass 8 freezes ABI-visible names and seals the composed descriptor containing that version. Provider interface, consumer call record, generated `LINKAGE SECTION`/`PROCEDURE DIVISION USING`, and manifests agree field-for-field. The receiver slot, when present, precedes explicit logical arguments; a result slot follows iff non-void and an exception context follows iff may-throw. Unknown imported effect is distinct and conservative. Required-field omissions, duplicate fields, stale profile/schema, and unknown required enum values fail closed before symbol registration. Each version change invalidates the appropriate source, logical-contract, physical-ABI, or descriptor-schema artifacts; source locations/private names do not. |
| `BACKEND-ABI-PROFILE-PROJECTION-001` | Lower one canonical logical call descriptor through every supported COBOL profile, including instance/lifecycle receivers, different pointer/layout capabilities, exact per-profile ABI projections, a missing export version, contradictory provider/profile declarations, and one intentionally unsupported version/profile pair. | Receiver presence/type/mode/projection, logical argument order/modes, result/context presence, ownership, and exception semantics remain identical. Pass 7 uses the export-pinned physical `abi_version` and requires an exact projection in the selected profile; imported consumers preserve the provider's version. Missing, contradictory, or unsupported declarations fail with stable diagnostics before layout fallback or module rendering; no default/highest-version negotiation occurs. Pass 8 freezes ABI names and seals the profile-bound ABI fingerprint. Profile changes do not perturb the source-interface hash or semantic specialization key; no guessed or host-dependent layout is accepted. |
| `BACKEND-NUMERIC-SEMANTICS-001` | For every adopted integer and floating type, execute every operator and conversion supported for that type (including integer remainder/shifts where adopted), comparisons, and mixed-type operations at minimum/maximum, one-past, zero, negative, and representable interior values. Include integer `minimum / -1`, divide-by-zero, invalid integer shift counts, left-shift overflow, negative quotient/remainder, narrowing boundaries, float precision boundaries, rounding ties, signed zero, subnormals, infinities, NaNs, and every declared exceptional comparison/domain case. Inject output mutations removing a guard, changing a rounding mode, truncating an intermediate, or bypassing a typed failure edge. | A high-precision/reference oracle independent of generated COBOL confirms exact results and exception types on every required compiler/profile. Checked failure leaves the target unchanged and follows the typed CFG edge with cleanup once; quotient/remainder/shift/conversion and floating exceptional behavior match the frozen source contract. Every operation is traced from typed IR through profile mapping, manifest, COBOL structure, compiler execution, and result/error assertion. Mutated output is rejected structurally or fails the executable reference assertion; no host integer width, implicit COBOL rounding, or vendor default changes semantics. |
| `BACKEND-FLOAT-LOWERING-001` | For each adopted floating operation/type/profile tuple, compare literal parsing and constant folding with runtime execution using an independent exact-rational/high-precision oracle. Cover both zero signs, normal/subnormal boundaries, adjacent representable values, rounding ties, integer precision boundaries, finite extremes, infinities, NaN classes/payload policy, all comparisons, float-width and integer conversions, and each supported math intrinsic's domain/accuracy boundaries. Mutate emitted/profile lowering to add an intermediate conversion, move a rounding point, change a comparison, contract multiply-add to FMA, canonicalize a promised-preserved NaN, or alter a typed exceptional edge. | Results match the frozen source descriptor at each observable operation boundary: exact representation where promised, otherwise the documented accuracy bound; compile-time and runtime values/outcomes agree. Unsupported profile tuples fail before rendering. Every mutation is detected by typed-IR/manifest verification or an executable oracle; decimal text formatting alone cannot pass a representation assertion. |
| `BACKEND-NUMERIC-GUARD-001` | For each signed width, test addition/subtraction at every guard threshold and adjacent value; multiplication in all four sign quadrants, including exact `min`/`max` products and adjacent overflowing products; negation of `min`; and division/remainder zero and `min/-1` cases. Instrument or mutate the lowered guard to evaluate after arithmetic, reverse a comparison, overflow a guard expression, or compute into the final destination before validation. | Every guard predicate agrees with an independent mathematical oracle at the threshold and both adjacent values. Guard calculations themselves are representable; dangerous arithmetic is not evaluated on a failing path; final storage is committed only on success. Any removed, reordered, inverted, narrowed, or overflowing guard is rejected by the IR/COBOL verifier or an execution/death assertion. |
| `BACKEND-NUMERIC-CONST-001` | Evaluate constant expressions and equivalent runtime expressions at each integer minimum/maximum, one-past range, overflow, conversion, shift, and division boundary; include host-C++ overflow traps and floating expressions at selected precision/rounding ties and exceptional values. | Compile-time and runtime evaluation produce the same value or the same specified diagnostic/exception category. Constant folding uses checked mathematical integer evaluation and the adopted floating contract; it never inherits host compiler overflow, precision, or rounding behavior. A failed constant expression is rejected before output publication with source provenance. |
| `BACKEND-NUMERIC-ABI-001` | Pass each public numeric type through direct, imported, native-library, method, constructor, and return-value ABI slots; include signed minima/maxima, floating edge values, arrays/fields where supported, producer/consumer builds in separate invocations, and caller/callee profile/type/precision/picture mismatches. | Canonical numeric IDs/ranges/precision and source conversion semantics agree between modules. The profile descriptor and emitted `PIC`/`USAGE`/sign/scale/representation, argument/result slots, and marshalling match field-for-field. Incompatible type-policy, profile, ABI-version, or physical-layout projections fail before link/render; no silent widening, narrowing, decimal/binary substitution, or host-layout assumption occurs. |
| `BACKEND-ABI-FINGERPRINT-001` | Independently encode canonical descriptor fixtures, check fixed SHA-256 vectors, then reorder source discovery/map keys and vary paths, timestamps, host endianness, diagnostic text, and local paragraph/temp names; separately mutate each ABI-significant field, numeric source-type/physical mapping field, and each domain-separated hash prefix. Independently change a profile's arithmetic guard/helper implementation while preserving public layout. | An independent encoder produces the specified full digest vectors. ABI-significant changes alter the ABI fingerprint; source-contract changes alter the source-interface hash. A profile arithmetic-lowering change invalidates its lowering/artifact evidence but leaves ABI fingerprint unchanged unless an exported entry/layout/boundary mapping changes. Irrelevant metadata leaves all semantic hashes unchanged. Private body-only edits affect the implementation hash; public template-body edits affect the template-body hash and its source-interface reference, not the ABI fingerprint by themselves. Cache truncation never substitutes for full-digest verification. Equal digests still trigger field-for-field descriptor validation; truncated/schema/version/profile mismatches never pass compatibility checks. |
| `BACKEND-TIR-VERIFY-001` | Verify each initial typed-IR operation family, then independently corrupt value/place IDs, canonical types, projection targets, access modes, ownership categories, callee/dependent-lookup state, provenance roles, effect facts, failure outcomes, result initialization state, and region-local evaluation-order edges. Include truncated parser text and unknown imported effects. | The verifier accepts only structurally complete IR; every injected corruption fails at the earliest owning pass with a stable internal-invariant diagnostic and source/related origins. Unknown effects remain conservative. No malformed IR reaches specialization or CFG construction, and no emitter-side reparsing or fallback can make it pass. |
| `BACKEND-ARG-MARSHAL-001` | Side-effecting receivers and arguments, temporary expressions, by-value classes/containers, moves, borrows, and injected failure while staging each receiver/argument position. | The receiver and each argument evaluate once in specified source order; a receiver is borrowed, never copied implicitly; staging copy/move/allocation operations and failure edges exist in typed IR/CFG before ownership analysis. By-value arguments create exactly one owned parameter object transferred to the callee; borrows alias without copies; pre-call failure skips the call and cleans only completed caller temporaries in reverse order. Physical ABI lowering introduces no second semantic copy or ownership transfer. |
| `BACKEND-ABI-CFG-PRESERVATION-001` | Compare the verified semantic CFG/lifetime plan with the graph after logical ABI and target-profile physical ABI lowering for non-throwing, throwing, imported-unknown-effect, value-return, payload-copy, argument-staging-failure, and double-exception calls. | After removing ABI-only protocol nodes, semantic successor edges, active exception identity, and per-edge live-object state are unchanged. Every ABI split maps back to one source edge; cleanup/handler/propagation executes exactly once; result completion is success-only; no ABI/rendering step adds an unmodeled throwing or allocating operation. |
| `BACKEND-RESULT-THROW-001` | Return scalars and owning classes/containers through result slots; inject failure during construction/move and throw after staging. | Result slot is marked complete only after successful construction; success transfers ownership once; failure leaves no published result and caller does not read/destroy an incomplete value. |
| `BACKEND-ABI-COMPAT-001` | Call a named old ABI through its versioned adapter and call the native entry directly. | Adapter maps its declared legacy order/status/error to the canonical native contract; it invokes exactly one canonical implementation; mixed-version or hybrid native/status layouts are rejected. |
| `BACKEND-HEAP-ABI-001` | Exercise the logical heap contract on the common typed-pool profile and each native-allocation profile: zero/negative/overflowing extents; limit-minus-one/limit/limit-plus-one; fragmented and exhausted pools; allocation failure at the Nth request; matching and mismatched type/count release; stale/double handles; contiguous `new[]`; and nested `vector<vector<string>>` reserve/rollback. Inject constructor failure at every element position and release failure at adapter boundaries. | `new T[0]` returns an empty non-dereferenceable token and constructs/releases no element; a negative runtime count yields `invalid_argument`; multiplication overflow or count above `max_size` yields `length_error`; exhaustion yields `bad_alloc`. Allocation reserves but never constructs; only successful construction advances live state; every object is destroyed once before release; every failure leaves old storage/content valid and reclaims only completed replacement objects. Null deletion is a no-op; stale/wrong-type/double release follows the non-throwing fatal policy. Common output uses only its declared standard COBOL pool constructs; native output uses only the manifest's declared facility. Cross-profile artifacts/handles cannot mix, and no C++/POSIX/Windows allocator supplies application semantics. |
| `BACKEND-TEST-INSTRUMENTATION-001` | Compile the same fixture without hooks and in marked test-instrumentation mode; inject each site/boundary at occurrence 1..N and N+1, including bad allocation, constructor/copy/move/comparator/hash/I/O failure, partial-I/O counts where permitted, destructor failure, and unmatched/duplicate IDs. Include two identical operations at distinct source sites and repeated instantiations of one generic body at distinct concrete specialization keys. Trace compiler-generated root operations for bootstrap, module readiness, uncaught reporting, payload release, service finalization, heap teardown, and process termination; mutate their synthetic identity inputs, provenance role, or `probe_mode`, and request fault injection at an observation-only site. Rebuild the identical source snapshot with reversed module discovery and varied worker counts; mutate the site manifest to point at a wrong operation, specialization, artifact digest, boundary, or undeclared probe; attempt an unsupported failure injection, cross-namespace cache reuse, and mixed instrumented/release link inputs. Also make a renderer-only formatting change and rebuild the instrumented artifact. | Only instrumented generated COBOL contains test control fields/probe calls. Removing all tagged test nodes from no-fault instrumented IR/CFG recovers the exact original semantic operations, edges, lifecycle states, and ABI projection, and execution matches the uninstrumented fixture. Distinct source operations, specializations, and canonical root operations produce distinct appropriate `fault_site_id`s; IDs are stable across renderer-only changes and independent of discovery/worker order, generated COBOL names, and module-IR node allocation. Observation-only sites allow trace emission but no fault action; unsupported actions and invalid synthetic IDs/provenance fail before execution. The independent verifier confirms one-to-one boundary-to-probe coverage and rejects every mutated site manifest before execution. Each configured fault probe fires exactly once at the declared effect boundary; injection cannot add a failure effect absent from the operation contract; partial transfers match exact counts and post-failure state; every unmatched plan fails the test; trace object tokens/event order match generated lifecycle behavior. A renderer-only change preserves semantic `fault_site_id`s and normalized trace events while regenerating probe-node mappings, COBOL ranges, instrumentation manifest, and test artifact digests. Instrumented keys/cache/publication remain isolated; mixed-mode links and any release reference to test-only artifacts fail closed. Release artifacts contain no hook symbols or test runtime dependency. C++ test hooks are confined to compiler-infrastructure tests and never simulate generated-program behavior. |
| `BACKEND-TRACE-VALIDATOR-001` | Validate good normal/exception traces and an expected fatal double-exception trace; truncate after header/event/footer, mutate schema/run/artifact/manifest/plan digests, omit or forge terminal records, reorder/duplicate sequence numbers, duplicate object transitions, use unknown site/boundary/event-kind IDs, omit/duplicate required event fields, add unknown fields, cross-wire root/service/exception IDs, and pair traces with inconsistent process exit/timeout/kill outcomes. For heap-backed uncaught exceptions, remove, duplicate, or reorder report-begin/report-outcome, payload-destroy, service-finalization, heap-check/release, and termination events; include both report terminal events, no outcome, a result/event mismatch, or output-failure event under a profile that cannot observe failure. Inject payload bytes, wrong service order/ordinal, or a terminal category inconsistent with the profile. Mutate canonical numeric spelling, hexadecimal case/length, escaping, field lengths/delimiters, and overflows; compare equivalent fixtures across profiles. | Only complete digest-bound traces with canonical encoding, the exact event-kind-specific field set, valid lifecycle transitions, independently expected shutdown order, and supervisor-consistent terminal outcomes pass. Each uncaught report has exactly one begin and one legal terminal outcome; completion/failure outcomes are mutually exclusive, profile-observable, and cannot be interleaved with teardown. Root/service/exception IDs and plan ordinals resolve to the bound artifacts. The validator proves reporting precedes payload destruction and service teardown, payload destruction precedes heap check/release, and termination follows cleanup; reporter events contain no payload bytes. Expected fatal termination is accepted only with the declared fatal event and independently observed process termination; timeout, forced kill, unexplained missing footer, malformed/truncated/noncanonical trace, or self-reported success cannot pass. Cross-profile decoded event semantics and terminal categories agree despite distinct artifact/profile headers; expected ledgers come from independent fixture models. Concurrent profiles validate declared happens-before relations rather than imposing undefined total order. |
| `BACKEND-TRACE-ISOLATION-001` | Fail trace reservation, hit the exact capacity and one event beyond it, inject trace append/flush failures immediately before a configured application fault and before normal/fatal termination, and vary trace byte limits across the same bounded fixture. Exercise dedicated-file and independently captured-stream transports; attempt to alias trace with application stdout, stderr, uncaught-report output, or a declared application file; pre-create a colliding endpoint, redirect the endpoint through a link, spoof an endpoint through source-controlled environment input, falsely declare a multiplexed channel as dedicated, or use a profile with no isolatable transport. Mutate the descriptor digest, transport ID/version, endpoint-kind identity, and ephemeral path/handle independently, then reuse stale evidence. | Trace bytes remain on the declared test-only channel and never appear in, consume, reorder, or alter application stdout/stderr, uncaught-report bytes, or application files; mixed-channel splitting/prefix heuristics are rejected. Endpoint collision, redirection, source/environment override, and descriptor/actual-endpoint disagreement fail closed before accepting execution evidence. A profile without a provably separate transport fails the trace-evidence gate as unsupported; no runner-selected fallback is accepted. Descriptor digest, transport ID/version, and endpoint-kind identity are bound into validation evidence: changing them invalidates/recomputes trace evidence but preserves source, semantic analysis, generated COBOL, and artifact key. Ephemeral path/handle changes affect only attempt-local supervision evidence; stale evidence or any path/handle placed in semantic/generated-artifact identity is rejected. Trace overflow/sink failure is reported only as invalid test evidence; application operations, injected failure selection, cleanup, and profile fatal outcome are unchanged. No event is silently dropped while the run can pass; a required trace failure yields `trace_overflow`/`trace_sink_failure`, and a failed fatal-path flush does not turn process termination into a return or a different status. Instrumentation performs no application-heap allocation or user lifecycle/API calls. |
| `BACKEND-HEAP-MODULE-001` | Allocate an object in module A, pass its handle through imported function/method calls in B and C, mutate and destroy it in the owning type's lifecycle path, then test foreign executable/profile/domain handles, duplicate/conflicting pool descriptors, missing type accessors, constructor re-entry, repeated heap initialization, and provider storage persistence across calls. | Every module operation resolves the same opaque handle through one linked domain and concrete type/layout ID; no consumer guesses a module-local address or chooses a release family. The link manifest contains the merged descriptor and exact service symbols; conflicts fail before rendering. Service state persists for the required lifetime or an explicit context is threaded through all calls. Constructor re-entry cannot allocate a reserved slot; initialization is once/idempotent or diagnosed; live objects are cleaned before shutdown, and debug leak reporting identifies outstanding canonical type/handle origins. Cross-domain/profile handles fail closed. |
| `BACKEND-HEAP-MERGE-001` | Link the same concrete specialization referenced from multiple modules; add distinct allocation sites for the same type; repeat an allocation-site reference through imports; add static reservations and dynamic requests; vary maximum contiguous extent, alignment, profile capability, and per-executable heap budget; exercise recursive/reentrant calls up to runtime exhaustion. Permute module/import/link order and worker completion. Mutate pool keys, provenance IDs, one static-slot count to overflow, layout/lifecycle fields under an equal digest, and aggregation-rule version. | Equal normalized pool keys yield one pool while retaining all distinct provenance; repeated references to one allocation site do not double count, and distinct static reservations sum exactly with overflow diagnosed before rendering. Maximum extent/alignment/capabilities merge by their declared rules. Every module uses the single executable-wide dynamic-capacity policy; no consumer gets a private pool or silently enlarges the budget. Requests exceeding profile or executable limits fail before rendering when statically knowable, and runtime exhaustion yields `bad_alloc` without corrupting live objects. Descriptor, capacity, service names, and artifact digests are identical under input-order permutations and differ when a true merged field changes. Field-level conflicts fail despite digest equality; recursive/reentrant demand is bounded by the shared runtime pool and never relies on a guessed local call count. |
| `BACKEND-HEAP-HANDLE-001` | Reuse a typed-pool slot after release; replay the old handle before/after reallocation; release null, stale, duplicate, wrong-type, wrong-extent, and foreign-domain handles; supply a token from a simultaneously live domain with the same descriptor; test a foreign-entry boundary that is and is not proven same-domain. Use a reduced-width generation profile to reach exhaustion and attempt wrap/reuse. Exercise array element projections, `delete` versus `delete[]`, and an active exception during a release violation. | Every reused slot gets a distinct generation before its new handle is published; stale tokens never resolve to a new object. Generation exhaustion retires the slot without wrap and later allocation reports `bad_alloc` when the remaining pool is exhausted. Domain validation distinguishes runtime instances, not just matching profile/link IDs; an unprovable foreign-handle ingress is rejected before rendering or by the declared boundary check. Null deletion is a no-op; wrong type/extent, stale, and duplicate release follow the non-throwing fatal policy without replacing the active exception. Element projection cannot release a whole array or escape the allocation's bounds; scalar/array release pairing is exact. |
| `BACKEND-HEAP-STATE-001` | For scalar `new`, each `new[]` constructor position, and container reserve/growth/insert/erase/clear, observe and fault-inject each block and per-element transition. Attempt resolve/publish/adopt/release from every invalid state; force failure before reservation, during construction, during nested-subobject cleanup, immediately before publication, and during staged reallocation. Mutate live counts, completion prefixes/bitmaps, owner IDs, and transition order in typed CFG and generated module IR. | No reservation token is source-usable. Only successful constructors mark elements live; scalar/array handles publish only after all required construction succeeds. Failed construction destroys exactly the completed subobject/element set and publishes no handle. Container storage has one owner, exact live-range agreement on every CFG edge, and inaccessible unconstructed capacity. Reallocation preserves old owner/data until staged construction succeeds, then commits once and destroys/releases old storage once. Invalid transitions are rejected before state mutation; resolution during construction/destruction and release with live elements cannot proceed. An injected failure leaves no leaked reservation, duplicate owner, live-state mismatch, or replacement of an active exception. |
| `BACKEND-GLOBAL-INIT-001` | Link modules with constant and dynamic globals, provider/consumer initializer reads, same-module declaration-order dependencies, transitive initializer function calls, multiple valid topological orders, module/global dependency cycles, unknown imported effects, reentrant initialization, and repeated exported-entry calls. Inject heap-service and global-constructor failure at every initialization position; exercise normal `void`/`int main` completion, uncaught `main` and startup exceptions with heap-backed payloads, reporter-output failure, an explicitly throwing payload destructor, an injected payload-lifecycle invariant failure, and double-exception cleanup failure. Mutate graph edges/order, `READY` transitions, completion flags, init/fini call count, exception-report timing, payload-release timing, and link-manifest order. | Heap/runtime services initialize before managed globals; `main` and observable exported entries run only after the frozen dependency order reaches `READY`. Canonical tie-breaking makes valid order deterministic; every dependency cycle or unresolved initializer effect is rejected before rendering. Initialization runs once, and failure never enters `main`; only completed objects/services unwind in exact reverse order while preserving the first exception. An explicitly throwing destructor is rejected by semantic analysis; a violated no-throw lifecycle invariant during unwind reaches terminal fatal handling and cannot replace the primary exception. On uncaught failure, reporting occurs while the exception payload, heap, and required reporter services are live; the no-throw/non-allocating report precedes type-directed payload destruction, service finalization, heap leak-check, and heap release. Native reporting failure cannot replace the primary exception or termination mapping. Normal exits destroy globals and finalize services before heap leak-check/release; the entry result is preserved, and fatal second-exception termination skips remaining cleanup. No generated path relies on COBOL `VALUE` or `WORKING-STORAGE` startup behavior. |
| `BACKEND-UNCAUGHT-REPORT-001` | Compile and execute normal completion, locally caught exceptions, exceptions rethrown then caught, uncaught startup/`main` exceptions, and fatal double exceptions on every claimed COBOL profile, with scalar and heap-owning payloads, imported throwable types, and nested generic payloads. Mutate the reporter to use C++/OS calls, invoke `what()` or a user callback, allocate, mutate/retain the frame, read payload after destruction, report after service finalization/heap release, report more than once during propagation, invoke during `FATAL`, or change the terminal category. Exercise every profile output disposition, including an observable output failure where supported; independently mutate/reseal reporter ABI/schema versions, profile output-adapter policy version, reporter descriptor/digest, and expectation manifest, including its expected path order and allowed-read/call set. | The independent expectation builder re-derives root reporting/teardown expectations from verified semantic CFG/lifecycle/type/profile inputs; a re-sealed forged manifest cannot authorize altered output. Normal completion and caught exceptions produce no uncaught-report events; a root-escaping ordinary exception reports exactly once; a fatal double exception bypasses the reporter and reaches termination without cleanup. The generated reporter is the declared private COBOL paragraph and its complete call graph is statically verified no-throw, non-allocating, and free of user calls. It emits only the baseline canonical type/source-origin fields unless a separately verified payload renderer is declared; it borrows but never mutates or retains the live frame. The output verifier proves report-before-payload-destroy-before-service-finalization-before-heap-release ordering from parsed COBOL, and runtime traces confirm it. Output failure is secondary and preserves the original exception and termination mapping. Every illegal call, access, order, or descriptor mutation is rejected before execution; valid fixtures compile/link/run with the selected profile's native output and termination behavior. |
| `BACKEND-ALLOC-PROFILE-001` | Lower `new`, `new[]`, `delete`, `delete[]`, vector growth, and string reserve under every named profile. | Profile manifest names the exact allocation/release operation and failure mapping; size overflow precedes allocation; failed growth preserves old storage; no hidden platform API or C++ allocator call appears; a profile missing required semantics emits the specified capability diagnostic. |
| `BACKEND-LAYOUT-PORTABILITY-001` | Compile/link matching caller and callee modules across each supported compiler/profile; compare manifest field layouts and execute integer/floating argument, conversion, result, and exception round trips at boundary values. | Numeric field widths, precision, sign/scale, representation, group order, pointer/handle form, entry names, and ABI version agree within a profile; cross-profile compatibility is not assumed; no host `sizeof`/padding guess enters the layout. |
| `BACKEND-TPL-DISPATCH-001` | Deep nested receiver/operator/member chains and lifecycle calls through depths 1, 2, 4, 5, 8, and 16. | Specialization is complete before CFG construction; every CFG call/lifecycle edge selects the exact concrete specialization one level below from the canonical specialization graph; no type-erased fallback, omitted layer, duplicate specialization, or accidental temporary/copy is present. |
| `BACKEND-PROFILE-001` | Lower each supported operation to each named target profile; include operations intentionally unavailable on one profile. | Supported operations use only allowlisted COBOL constructs; unavailable semantics produce a source-mapped diagnostic before rendering; OS identity alone never selects a different operation. |
| `BACKEND-PORTABILITY-PROFILE-001` | Run one frozen source fixture suite through the common typed-pool profile and every named native-allocation profile. Cover scalar/class/string allocation, `new[]`/`delete[]`, exhaustion, constructor rollback, nested `vector<vector<string>>`, exception cleanup, and cross-module handle use. Independently mutate common-profile output to introduce `USAGE POINTER`, `BASED`, `SET ADDRESS OF`, `ALLOCATE`, or `FREE`; mutate profile allowlists, source format, profile ID/version/digest, compiler identity, host-OS labels, uncaught-report ABI/schema and output-adapter policy versions, and reporter output statement/destination/stream/encoding/newline/field form/output-failure observability. Also test a feature explicitly unavailable to a profile. | Each promised profile independently passes generated-COBOL parsing, compile, link, and execution for every claimed capability. The common profile contains no construct outside its tested common-subset allowlist; native-only facilities appear only in profiles declaring and proving them. The independent parser catches extension leakage even when text appears in a comment/literal or formatting changes. Reporter operations and observable output behavior match the pinned profile contract; unmanifested output forms, streams, encodings, or status assumptions are rejected. Unsupported semantics fail with a stable source-mapped diagnostic before rendering. Profile/capability changes require consistent version/digest updates and invalidate the correct lowered artifacts/evidence; OS identity and compiler brand alone cannot select or upgrade semantics. A GnuCOBOL-only success does not establish ANSI-85 compatibility, and common-profile evidence is never inferred from native-profile runs. |
| `BACKEND-NAMES-001` | Long identifiers sharing the same truncated stem; case-only collisions across scopes/modules; reserved words and invalid leading characters; overloaded/external symbols; generated temporaries; imported symbols; nested template specializations; reverse module/worker/specialization discovery; profile limits shorter than the normal digest; and ABI-frozen names colliding with private candidates. | The exact same canonical key always maps to the same legal name within a profile; distinct keys never alias inside a collision domain. Fixed ABI names are byte-for-byte preserved or fail explicitly. Private names remain within each profile's grammar and limit, and are deterministic across input order, process address, host OS, working directory, worker count, and cold/warm cache. Impossible profile/name-space combinations fail before rendering with both conflicting canonical IDs; no silent truncation, order-dependent suffix, or unresolved reference occurs. |
| `BACKEND-NAME-MAP-001` | Serialize and independently read the `CBLC-COBOL-NAME-TABLE` artifact across program/data/linkage/paragraph names, generated activation fields, control regions, cleanup and exception names, template instances, imported calls, and exported ABI-frozen entries. Permute request/reference order and cold/warm cache state; remove/add one expected source or compiler-generated request or reference; mutate key hex, entry/schema/profile/allocator versions, stem-policy parameters, collision-domain set, reference set, provenance role, ABI-frozen spelling, Unicode/source spelling, array order, whitespace/escape, and artifact content digest; re-seal every semantic mutant. Put a module-IR node ID into the pre-module name table, omit a required generated request, append a late name after freeze, or omit a later module-IR declaration/call reference to a frozen entry. Also compare the provider `.cbi` exported spelling and final link-manifest import. | The table uses the Section 6.7 canonical JSON codec and is byte-identical for identical inputs. An independent verifier decodes/re-encodes every canonical key, reruns allocation from the independently derived closure-complete profile-lowered request set, and checks exact entry/reference coverage, including stable pre-module request IDs and typed-IR/CFG/ABI or compiler-generated provenance roles; it also checks profile legality, canonicalized namespace uniqueness, and collision results. Module-IR node IDs are never allocation inputs. The set is derived independently from verified IR/CFG and generated planning records, not copied from the table. After module-IR construction, a separate verifier proves every declaration, operand, call, and branch-target name reference resolves exactly once through that frozen table. The provider `.cbi`, name-table entry, ABI descriptor, and link manifest preserve the same external spelling; private names remain module-local. Missing/extra/duplicate entries, late requests, stale or unknown schema/policy versions, altered spellings, malformed encodings, digest-valid semantic mutants, and cache/profile mismatches fail before module-IR publication or linking. |
| `BACKEND-NAME-ALLOCATION-001` | Unit-test canonical-key serialization and non-colliding base allocation with hand-authored vectors covering every required/optional tag, absent versus empty values, ordered type lists, fixed-width integer encodings, and profile stem-policy/initial-digest parameters. Assert exact key bytes, primary hash input concatenation, lowercase hex, kind token, candidate separators, and right-truncation of the readable stem. Cover ordinary/reserved/invalid-leading stems, maximum and discriminator-too-short identifier lengths, identical duplicate requests, and conflicting duplicate-key requests. Reject digest parameters outside 8–64 hex digits or not divisible by four; reject duplicate/out-of-order/unknown tags, empty required IDs, truncated or overflowing lengths, trailing bytes, and unsupported schemas. Permute tagged-field input order only where canonicalization must normalize it. Mutate tag assignments, field encoding, integer endianness, schema/domain prefixes, digest input concatenation, base candidate formula, stem normalization, or duplicate handling. | Independent golden vectors match the exact serialized key bytes, primary digest inputs, base spellings, and diagnostics; equivalent canonical inputs serialize identically independent of host endianness/layout, while meaningful identity changes alter key bytes. Identical duplicate requests coalesce to one allocation with all references preserved; conflicting duplicate keys fail deterministically before emission. Invalid encodings fail before hashing. Collision extension and forced hash-collision behavior are tested separately by `BACKEND-NAME-DIGEST-COLLISION-001`; whole-program declarations/references and serialized-map verification remain the responsibility of `BACKEND-NAMES-001` and `BACKEND-NAME-MAP-001`. |
| `BACKEND-RENDER-001` | Render every module-IR node family at profile line-length boundaries: exact fit, one byte/column over, legal statement/clause/operand breaks, oversized identifiers (which must fail name allocation before render), indivisible literals/clauses, fixed/free source formats, permitted continuation forms, newline/BOM/final-newline policies, and encoding failures. Render identical inputs from cold/warm caches, different worker orders, working directories, locales, and host OS settings. Independently rescan bytes and source-map ranges, then parse the artifact and reconstruct normalized module structure. Mutate one whitespace/token, line ending, continuation mark, byte offset, mapping range, and line-limit calculation at a time; re-seal artifacts. | Exact profile-pinned output bytes and source maps are reproducible independent of host/runtime state. Every line obeys the profile's byte/column limit; only grammar-approved breakpoints are used and no identifier/literal/clause is split or truncated. An oversized generated identifier fails in name allocation; an indivisible lowered construct that cannot fit fails before publication with a source-mapped diagnostic. Independent byte scanning confirms encoding, newline, line/column and half-open range data; parsing confirms the normalized graph. Structure, ABI, branch/call graph and provenance remain equivalent; formatting changes cannot alter semantic choices, and mutated source maps/artifacts are rejected even when re-sealed. |
| `BACKEND-MODULE-IR-001` | Construct module IR for all supported emitted node families, then independently mutate each self-verifier stage: input/profile/ABI/name/control-region/activation-layout identity; schema/version, node tags/IDs/field encodings and order; parent/child/name/declaration/paragraph/operand/call/ABI/CFG references; profile capability and PIC/USAGE/layout; control-flow/lifecycle/activation state; ABI/dependency agreement; and canonical serialize/deserialize. Include digest-valid stale and semantic mutants, cycles legal only for CFG edges, duplicate and ambiguous references, and malformed nested containment. Assert invalid IR cannot reach renderer, cache, or publication and diagnostics identify the earliest failing stage, rule, node, and source/related origin with stable ordering. Render/reparse valid fixtures with line wrapping, continued literals, nested groups, paragraph calls, and profile-specific syntax; compare normalized reconstructed COBOL graph field-by-field. | Each malformed self-verification input is rejected at the earliest applicable stage before rendering; failed stages publish no partial IR, indexes, cache entry, or success-shaped artifact. Unknown required schema/node variants fail closed; versioned cache results with stale/missing digests are misses. Valid IR has unique fully resolved references and agrees with the frozen ABI/profile descriptors, verified CFG/lifecycle facts, and activation layout; it does not consume the semantic expectation manifest. Serialization round trips preserve the canonical graph. Rendering/reparse preserves normalized declarations, calls, control flow, literal values, lifecycle/error edges, and provenance; downstream semantic-expectation equivalence is covered independently by `BACKEND-EXPECTATION-INDEPENDENCE-001`. Whitespace-only changes affect output hashes/ranges but not node identity or semantics. No raw COBOL text or emitter-side fallback can repair a corrupted IR. |
| `BACKEND-MODULE-IR-BUDGET-001` | For every declared module-IR, verifier, COBOL-byte/line/token/literal/depth/parse-work, expectation/correlation, and diagnostic budget, exercise limit-minus-one, exact-limit, and limit-plus-one. Use wide/high-fanout graphs, deeply nested expressions/groups/regions, large literals/comments, large provenance sets, malformed syntax with recovery pressure, ambiguous correlation candidates, near-overflow count/byte products, and many distinct diagnostics. Permute node/reference order, worker scheduling, and cache state; mutate one charge increment, counter width, work-unit schema, quota, and cancellation point. | Charges and the first over-limit origin are deterministic across enumeration, host, worker count, and cache state. Exact limits pass; plus-one fails before unbounded growth/publication with the named dimension and stable diagnostic. No partial IR, parsed graph, correlation report, successful cache entry, or falsely complete diagnostic set is published. Resource exhaustion is distinct from syntax/profile/semantic failure. A policy-only quota change preserves generated COBOL, source map, ABI/name artifacts, and generation key while invalidating affected verification evidence; stale charge summaries are rejected. |
| `BACKEND-EXPECTATION-INDEPENDENCE-001` | For hand-authored fixtures covering calls, ABI slots/layout, branches, allocation failure, cleanup, imports, and template lifecycle, independently derive the semantic expectation graph and generate COBOL. Assert semantic records contain stable expectation IDs and semantic source origins but no module-IR node IDs or rendered ranges. Fault-inject one defect at a time into profile lowering, module-IR construction, expectation construction, rendering, output parsing, and the downstream expectation-to-module/output correlation join; include a defect shared by any proposed helper and mutate emitted bytes after manifest creation. Test missing, ambiguous, one-to-many, and many-to-one correspondence according to the versioned join policy. With verified module IR and frozen names held fixed, independently mutate/re-seal the expectation manifest and observe renderer inputs and exact output bytes. | The expectation builder consumes only verified semantic/CFG/ABI/profile/name artifacts and not module IR or rendered output; each expectation is independently reproducible from those inputs. The output reader consumes rendered bytes and not emitter objects. Only the separate validation-correlation report records observed module/output node IDs and generated ranges; it cannot feed back into expected semantic values. The renderer API/input closure excludes both expectation and correlation artifacts: changing either alone leaves rendered bytes and source map unchanged, while an incorrect expectation or join is rejected by independent validation. Independent golden values detect incorrect shared decisions; every injected single-layer defect is localized and rejected before the next trust boundary. A matching manifest and parser never count as behavioral proof without compile/link/run evidence. |
| `BACKEND-CORRELATION-INVALIDATION-001` | Build and validate an unchanged module, then change only the output-parser grammar/tokenizer version, correlation-report schema, join/cardinality policy, or generated-coordinate mapping policy. Replay old reports/evidence, reparse the same source bytes, and inject both a still-valid join and a newly ambiguous/missing join. Separately change only the external COBOL compiler identity under the same semantic profile. | Parser/correlation-only changes regenerate the report and affected validation evidence, change the full registry/evidence-plan identity, and reject stale evidence while preserving `artifact_pass_registry_digest`, generated source bytes, source map, module IR, ABI/name artifacts, `generation_key`, and artifact-manifest digest. A mismatch discovered under the new policy fails the gate without rewriting generated output or expectations. Compiler-identity-only changes invalidate compile/link/run attempt evidence but preserve source/semantic artifacts. No evidence is relabeled across parser, join, coordinate, or tool versions. |
| `BACKEND-COBOL-EXPECTATION-RETURN-001` | Derive return expectations from independently verified typed IR, lifecycle/CFG proofs, logical and physical ABI descriptors, profile layout, and frozen names for scalar return, direct class construction, named-local copy/move, explicit move, conditional merge, nested-call result, and ABI staging. Mutate only generated module IR or COBOL to select a different lifecycle call, add/remove an elision, set result-complete before construction, publish before cleanup, destroy the source before transfer, duplicate/omit a destructor, read a result on a failure edge, or map to the wrong result field. Separately alter and re-seal the expectation record. | The manifest states the canonical selected operation, source/result object identities, moved-from transition, construction completion, exact cleanup order, success-only publication/read, and physical result-slot mapping. Independent parsing of emitted COBOL must reconstruct those facts and match the manifest; a self-consistent forged manifest or digest is rejected against semantic inputs. Every COBOL-only mutation is rejected before external compilation, and unchanged fixtures compile/link/run with lifecycle traces matching the object ledger across required profiles. |
| `BACKEND-COBOL-SOURCE-LEX-001` | Feed exact byte fixtures for each profile's fixed/free format: boundary columns, indicators, line endings, tabs, BOM/encoding, multibyte literals, comments/debug lines, doubled delimiters, continued words/literals, and in-source directives. Place lookalike tokens in literals/comments and mutate one dropped/reclassified byte or source-map offset at a time. | Token stream and byte/line/column ranges match the profile specification exactly; comments/literals cannot inject syntax or trigger false positives. Unsupported encoding/continuation/directive/source format fails closed with accurate generated location and source origin; no OS newline, Unicode, or tab behavior is silently assumed. |
| `BACKEND-COBOL-SUBSET-PARSE-001` | Parse independent hand-written fixtures for every emitted module-IR statement/data-clause variant, nested scope form, ABI call mode, literal, file/exception/allocation operation, and profile extension. Mutate terminators, periods, clause order, operand category, nesting, and grammar/profile schema; compare accepted parsed graphs to expectation manifests and module IR. | Valid fixtures reconstruct the exact normalized declarations, layout, call slots, branches, and dependencies. Unknown/unmodeled syntax and grammar-version mismatch are rejected, not skipped or recovered into accepted output. Parse/serialize/reparse is stable for semantic structure; permitted formatting-only changes preserve the graph, while each semantic mutation is caught before compile/link. |
| `BACKEND-PROVENANCE-001` | Direct call, imported call, synthetic cleanup edge, and template specialization, each traced from source span through typed IR/CFG/module node to rendered COBOL range. | Every range has the correct primary origin and related definition/use/scope/ABI origins; generated-only nodes are labeled as compiler generated; no stage invents a nearby source location when provenance is missing. |
| `BACKEND-PROVENANCE-COORDINATES-001` | Map source and generated byte offsets to 1-based lines/columns for ASCII, multibyte BMP and supplementary UTF-8 scalars, combining sequences, tabs, LF/CRLF, malformed UTF-8, LSP UTF-16 positions, and compiler adapters reporting byte/code-unit/character columns. | Half-open byte ranges round-trip against the exact hashed snapshot; CBL-C scalar columns and protocol/tool conversions match independent fixtures; malformed input and unknown/out-of-range tool coordinates fail closed without inventing a source origin. |
| `BACKEND-SOURCE-MAP-001` | Map both directions for one-to-one, one-to-many, and many-to-one spans; add overlapping ranges, line wrapping, generated paragraphs, stale source hashes, and checkout relocation. | Range lookup uses specified deterministic precedence; mappings preserve canonical module ID/source hash and origin roles; renderer formatting changes positions but not origin identity; stale maps are rejected. |
| `BACKEND-DIAGNOSTIC-ORDER-001` | Produce multiple independent and duplicate errors across modules and parallelizable passes; vary worker completion, verifier discovery, cache hit/miss, and repeated pass scheduling. Include ties on module, source byte offset, phase, and diagnostic ID, with different related origins/specialization contexts. | Diagnostics have stable instance keys; exact duplicates are merged, distinct provenance/context is preserved, and total ordering follows the specified key independent of discovery/scheduling. Serialized records/messages contain no pointer/address, temporary absolute path, nondeterministic hash-table ordering, or terminal presentation state. |
| `BACKEND-FAILURE-ARBITRATION-001` | Build a DAG with independent modules returning distinct source, ABI, resource, and tool failures, plus task-, dependency-closure-, and invocation-scoped failures, direct/transitive consumers, an independent branch, and a legitimately not-applicable pass. Hold/release workers to permute completion order; run with worker counts 1, 2, and N; inject cancellation at fixed barriers and before/during/after commit linearization. | Every worker count admits the same frozen frontier and yields the same per-task outcome (`completed`, `failed`, `blocked`, `cancelled`, `not_admitted`, or `not_applicable`), sorted diagnostics, failure-cause set, missing-input/root-task records, blocked-descendant roots, and primary diagnostic. Task-scoped failure affects only its output consumers through prerequisite propagation; dependency-closure scope blocks its declared transitive consumer domain; neither creates fabricated diagnostics for blocked work. Independent admitted work is collected. Invocation-fatal causes stop only subsequent frontiers. At each fixed cancellation point, the terminal state and completed frontiers are deterministic; moving that point may stop later frontiers, but never erases observed causes or lets completion order select the result. No partial artifact is published. |
| `BACKEND-TOOL-DIAGNOSTIC-001` | For every pinned compiler-family/version adapter, feed diagnostics for fixed/free-format generated files with absolute/relative paths, same-basename artifacts, wrapped source lines, exact token boundaries, multiple locations, multiline notes/caret blocks, warnings/errors, and diagnostics without positions. Exercise the adapter-declared byte/code-unit/character column convention, continuation-line coordinates, CRLF output, out-of-range line/column, stale artifact hash, wrong generated file, unknown path, malformed encoding, and unrecognized format. Place diagnostic records below, at, and across the stdout/stderr retention caps; split one diagnostic across chunks/cap boundaries; then trigger a read failure or missing EOF. Mutate adapter version/rule order, path normalization, line/column conversion, artifact selection, map precedence, severity/category decoding, and capture completeness; retain bounded raw bytes plus complete digest/count only when EOF is observed. | A diagnostic is mapped only when the exact tool/profile/artifact identity and adapter parse yield a valid generated byte offset in the hashed COBOL artifact. Mapped results preserve raw coordinates and text while adding the exact generated range and deterministic primary/related CBL-C origins. Ambiguous, stale, unknown, out-of-range, unparsed, or capture-incomplete locations remain explicit external-tool diagnostics with bounded raw data and no guessed source span; they cannot become source diagnostics by nearest-line fallback. Truncated excerpts never claim a complete diagnostic set, and output-limit/read/incomplete-stream outcomes cannot pass compile evidence even when a retained diagnostic maps successfully. Adapter mutations are detected by fixtures and source-map verification, and all compiler records retain the exact command, version, captured stdout/stderr state, observed byte counts/digests, and exit status. |
| `BACKEND-DIAGNOSTIC-SNAPSHOT-001` | Emit the same diagnostic code/subject/span from two different source snapshots, then relocate an identical frozen snapshot; repeat for generated-only/external-tool diagnostics using distinct artifact-input digests. Vary specialization context and related origins, reorder parallel discovery, change only message templates/localization, and attempt to reuse a cached diagnostic from the other snapshot. | Instance keys differ when the exact source/artifact snapshot or specialization/related-origin identity differs, but remain stable under checkout relocation, worker order, and message presentation changes for identical frozen inputs. A stale cache entry is rejected before diagnostics merge; exact duplicate instances within one snapshot merge, while distinct contexts remain separate and sort deterministically. |
| `BACKEND-MANIFEST-VERIFY-001` | Mutate each emitted COBOL/interface/name-map/source-map/expectation artifact independently; corrupt paths, IDs, digest/schema/profile fields, artifact class, instrumentation metadata/site manifest, dependency and call edges, and compile/run evidence; reorder equivalent manifest inputs; try to link test-only artifacts into a release closure. | The independent reader checks canonical serialization, all referenced content hashes, root containment, dependency/call-set equality, ABI/profile compatibility, instrumentation-site correspondence, artifact-class isolation, and evidence-to-artifact identity before link. Every mutation fails closed with artifact/field context; semantically equivalent orderings produce identical manifests; release links cannot consume test artifacts; validation status never exceeds the highest trusted evidence actually present. |
| `BACKEND-EVIDENCE-STATUS-001` | Build the required tuple set from the test catalog for multiple profiles and evidence levels. Remove/duplicate a case record; misstate a filtered run as complete; promote source/structure success to compile/run; bind a passing attempt to the wrong artifact, plan, fixture, tool, or profile; forge `unsupported`/`not_applicable` without capability or predicate evidence; omit a required case; and retry a failed case successfully while preserving its attempt history. | The independent aggregator derives state only from the catalog, selected profile matrix, and immutable bound attempts. Each required `(case_id, evidence_level, profile_id)` is accounted for exactly once in the current run state; missing, duplicate, stale, or mismatched evidence cannot pass. Lower-level evidence never promotes; `unsupported` counts only under an approved narrow exclusion, `not_applicable` requires its declared predicate proof, and filtered runs remain partial. A valid retry may satisfy the current tuple only through a new passing attempt with matching identities; the prior failure remains immutable history. The aggregate reports required/selected counts, satisfying attempt IDs, approved exclusions, and every unresolved tuple; a forged summary boolean or pass count is rejected. |
| `BACKEND-OUTPUT-MUTATION-001` | For every generated-COBOL verifier rule, mutate one syntax/ABI/numeric/control-flow/dependency/lifecycle invariant after producing a valid fixture; run both unre-sealed integrity mutants and re-sealed semantic mutants with the independent expectation untouched. Add permitted whitespace/comment transformations and behavioral-only mutations that require compile/run. | Integrity mutants fail the digest rule; re-sealed semantic mutants fail the specific expected parser/manifest rule before compiler invocation, with stable rule ID and generated location. Equivalent formatting is accepted only when profile-permitted. Every non-equivalent deterministic mutant is detected by its intended layer; hash failure alone cannot count as semantic mutation coverage, and runtime mutations require disagreement with an independent oracle. |
| `BACKEND-EVIDENCE-001` | Claim compile/link/run success without executing the harness; reuse a result from another artifact/profile/compiler version; tamper with CI attestation, workflow identity, test matrix, or bound manifest digest; label a skipped profile as passed. | Self-asserted fields never raise validation status. Local evidence is explicitly unverified unless observed by the active harness; release status requires a trusted CI attestation bound to the exact manifest digest, compiler/profile matrix, and test results. Any altered/mismatched/untrusted record is rejected or downgraded to unverified, never counted as a pass. |
| `BACKEND-ARTIFACT-ATOMIC-001` | Start with a valid committed build, then inject failure after each source/module artifact write, manifest serialization, hash verification, structural validation, compiler/link/run step, artifact-generation binding, attempt commit-record publication, and current-reference replacement. Run the coordinator crash/cancellation and filesystem/lease races specified by `MOD-COMMIT-CANCEL-001` and `MOD-PUBLISH-ATOMIC-001` against the same artifact/evidence schema. | No failed stage publishes a success-shaped bundle or overwrites/invalidates the last known-good bundle. Readers ignore uncommitted staging/orphans; a committed manifest references a complete verified artifact set only, and a successful current reference binds that artifact manifest to the exact validated evidence. Outcomes after uncertain replacement are established only by reference revalidation or startup recovery; no backend artifact check substitutes for the coordinator protocol. Failures retain structured diagnostics and bounded raw tool evidence in isolated storage; orphan cleanup cannot delete committed output or data held by a live reader. |
| `BACKEND-RESOURCE-001` | Exercise each configured source, graph, IR/CFG, specialization-key/substitution, recursive call-SCC effect-propagation, diagnostic, and generated-byte budget at limit minus one, the exact limit, and limit plus one; repeat with reordered modules, worker counts, and cold/warm caches. | Boundary behavior is exact; failure is a stable `resource_limit_exceeded` diagnostic with dimension/limit/observed work; specialization closure and SCC iteration charges are deterministic; module and phase selection is deterministic; no counter wraps, work is silently truncated, or partial artifact is published. |
| `BACKEND-CANCEL-001` | Request cancellation during parsing, specialization, dataflow, rendering, and validation, including inside long worklists. | Cancellation reaches a documented safe point within its bound, has a distinct non-success result, leaks no temporary resources, preserves prior artifacts, and publishes no success-shaped partial module. |
| `BACKEND-TOOL-INVOKE-001` | Invoke fake compiler/linker/runtime tools through the Windows and POSIX adapters using paths containing spaces, Unicode, shell metacharacters, and leading hyphens; test exact `tool_argv` boundaries and count/native-serialization limits at minus one, exact, and plus one; explicit profile/version selection; `tool_environment` allowlists/locales; missing executables; malformed version responses; zero-exit missing output; nonzero partial output; and symlink/reparse-point output escape. For generated-program runs, verify `program_argv` and `program_environment` are passed only through their distinct runtime fields and are not folded into compiler/tool configuration. | The fake tool receives exactly `tool_argv` without shell reinterpretation; tool-vector excess fails before spawn with `argument_domain=tool_argv`, the correct limit dimension/units, and no child status. Actual generated COBOL observes exactly the declared program arguments/environment, with no leakage between the tool and application environments. The same logical process inputs hold on both hosts. Only a successful, completely captured version query can yield `version_mismatch` for malformed or unsupported version output; query timeout, nonzero exit, cleanup, or capture faults retain their corresponding higher-priority process outcome. Reject a mismatch before compile/artifact acceptance, and consider only declared regular outputs rooted in the isolated directory. Tool/runtime input changes affect only their corresponding validation evidence, never source/profile selection or generated-artifact identity; partial/escaped outputs are never published. |
| `BACKEND-PROGRAM-ARGV-001` | Run generated programs with zero, one, many, empty, whitespace-only, leading/trailing whitespace, quote, backslash, shell-metacharacter, and non-ASCII UTF-8 arguments. Distinguish tool argv from program argv. Test Windows UTF-16 conversion including unpaired surrogates, POSIX invalid UTF-8 bytes, executable-name exclusion, and count/aggregate-UTF-8-byte limits at minus one, exact, and plus one. Independently change the target profile's declared program-argument count/byte limits. Inject a profile mapping that replaces malformed input, changes error ordering, or conflates launcher and source-visible limits and require rejection. Round-trip fixtures through actual compiled COBOL entry code on every required profile. Inject allocation/construction failure at each materialization position and mutate quoting/escaping, argument order, one byte, profile mapping, and evidence fixture independently. | `process::arguments()` returns exactly the ordered logical vector excluding the executable name; empty arguments and boundaries are preserved, and arguments are never shell-expanded or split. Application-vector excess is rejected before spawn with `argument_domain=program_argv` and exact limit dimension/value; each adapter proves its launcher can carry the entire declared profile limit. Within launcher limits, malformed native encoding reaches the decoder and raises `invalid_encoding`, never replacement text; argument count still applies, and encoding validation precedes aggregate UTF-8 byte measurement. A malformed fixture that exceeds the native launcher bound is rejected before spawn with the corresponding `argument_limit_exceeded` domain/dimension, not mislabeled `invalid_encoding`. A profile that cannot preserve the declared valid range reports unsupported rather than claiming portability. On allocation failure at position N, the completed prefix is destroyed exactly once in reverse order, staged storage is released, and no partial vector escapes. The expectation manifest and COBOL call graph show the selected native acquisition mapping and concrete `string` lifecycle dispatch. Changing only runtime argument values changes validation evidence but preserves generated COBOL and its artifact key. Changing declared profile limits bumps the profile contract and invalidates affected profile-lowered artifacts and conformance evidence without changing the profile-neutral source-interface hash; a semantic decoder/limit-order mutation fails the conformance gate rather than passing as a new profile mapping. |
| `BACKEND-HOST-SERVICE-001` | Execute generated COBOL with absent versus empty environment variables, valid/invalid UTF-8 values, name-case collisions, ambient-secret decoys, fixed wall-clock instants across time zones, monotonic schedules with equal/backward samples, and fixed random seeds across every profile. Inject environment lookup, clock-read, and random-generation failures; force allocation failure while materializing returned values and failure after a random request. Mutate only each host-service fixture, profile mapping, and expected output. | Tool environment and program environment remain distinct; only declared program variables are visible, absence differs from empty, and encoding/case behavior matches the frozen portable contract. Wall-clock values are timezone/locale independent; monotonic readings never decrease or fail with the specified typed error. Seeded random sequences are identical across required profiles, and failed generation does not advance state. Test values enter only through declared typed host-effect boundaries; each error follows the correct CFG/catch/cleanup edge, and generated COBOL contains no C++ service shim. Changing environment/clock/seed fixtures reruns execution evidence but does not alter generated COBOL or its artifact key. |
| `BACKEND-TOOL-FS-ISOLATION-001` | Run generated file-I/O programs against immutable evidence inputs, read-only fixtures, read-write copy-on-write fixtures, and declared output roots. Exercise create/truncate/append/update/delete/rename; parallel runs using the same fixture snapshot; relative traversal, absolute, drive/UNC, separator/case alias, symlink/reparse-point and hard-link escape attempts; undeclared file creation; and post-run restoration tricks. Mutate rights, fixture digests, expected effects, and host sandbox capabilities. | Evidence snapshots and source inputs remain byte-identical and cannot be written by the child; each execution has an independent disposable fixture view. Only manifest-authorized paths/mutations succeed; attempted escapes and undeclared effects are denied or invalidate the run. The complete resulting file set and bytes match the independent oracle, with only declared host-observable metadata considered. Parallel executions cannot see or alter one another's effects. If confinement cannot be enforced, the profile reports unsupported and no filesystem-portability evidence is accepted; a final digest scan alone never passes. |
| `BACKEND-STDIO-001` | Run fixtures with stdin closed, explicitly empty, and non-empty (including NUL and non-ASCII bytes); read incrementally and only after a delay; exit before consuming all input; flood stdout and stderr concurrently; emit CR/LF variants and invalid UTF-8; and inject partial stdin-write, broken-pipe, capture, and cap-boundary failures on Windows and POSIX. Include input/output volumes larger than each OS pipe capacity, with the child writing both output streams before reading stdin, reading stdin before writing, and exiting while the feeder is blocked. Include child-success plus broken-pipe and child-success plus stream-capture-failure combinations. Also inject simultaneous timeout/cancellation, output-cap, stdin-write, capture, cleanup, and artifact-contract faults to exercise the normative overall-outcome precedence while varying worker/interleaving schedules. Mutate expected bytes, stream assignment, fixture digest, and comparison-policy version independently. | No child inherits a terminal or undeclared stream. `stdin_delivery_state` is exactly one of `not_started` (no child spawned), `closed` (started with immediate EOF), `complete` (all fixture bytes accepted by the pipe), `truncated` (early child exit left declared bytes undelivered), or `write_failed`; fixture bytes arrive unchanged and EOF occurs after the declared bytes when delivery completes. Stdout and stderr each report exactly one of `not_started`, `complete`, `limit_exceeded`, `read_failed`, or `incomplete`, with full byte count/digest only when the full stream was drained. Cap crossing maps to `output_limit_exceeded`, read failure to `stream_capture_failed`, and missing EOF at the hard cleanup deadline to `process_cleanup_failed`. The closed result preserves all component states, child outcome/raw status, termination trigger, and cleanup outcome; primary `overall_outcome` follows the specified deterministic precedence, so child success cannot mask transport, capture, artifact, or cleanup failure. Feeder, both drainers, and process reap complete without deadlock or leaked workers, including early child exit. Stdout/stderr remain separate, byte counts and full-stream digests match independent expectations, and an output-cap failure cannot pass exact-output validation even if its drained digest matches. Only explicitly declared normalization is applied. Stdin/expectation changes rerun affected validation attempts and change evidence identity, but preserve generated COBOL and its artifact key; stream or capture failures never publish a successful validation result. |
| `BACKEND-TOOLCHAIN-INVALIDATION-001` | Build with a pinned supported compiler/toolchain, then independently change only its executable/package digest, its version within the same declared profile constraint, the tool-compatibility policy, a dialect/capability mapping, the CBL-C compiler backend version, and the validation-plan/test-data digest. Include a forged version response that disagrees with the verified executable/package identity. | A same-profile external compiler identity change preserves source/semantic, specialization/CFG/lifecycle, generated-COBOL outputs, `generation_key`, profile ID/version, ABI fingerprint, and artifact-manifest digest, but invalidates temporary compile/link outputs and reruns acceptance/link/run evidence with the new evidence-input identity. Changing only the tool-compatibility policy reruns tool selection/acceptance checks without changing profile ID/version, ABI fingerprint, or generated COBOL. A capability or dialect contract change bumps the profile contract version and invalidates all affected profile-lowered/module/rendered artifacts and downstream evidence while preserving only proven target-neutral analyses. A CBL-C compiler-backend change invalidates its declared lowering/rendering dependents. A validation-plan/test-data-only change preserves generated COBOL and reruns affected gates with new attempt evidence. Version/digest disagreement, unsupported versions, or undeclared tool substitution fails before accepting any output; no stale evidence is upgraded or reused. |
| `BACKEND-TOOL-SUPERVISION-001` | Use fake tools that hang before/after spawning descendants, flood stdout and stderr concurrently, ignore graceful termination, exit by signal/abnormal status, and are cancelled during compile/link/run. Include a root that exits zero while a descendant stays alive, holds stdout/stderr open, or mutates staged input; attempt descendant detachment/containment escape. Exercise just-below/at/above normal-exit grace, termination grace, hard cleanup deadline, timeout, and output caps, plus failures during child cleanup. | Root exit never completes the invocation while a descendant or pipe pump remains. A surviving child is classified `orphan_process_detected` and the whole domain is terminated; timeout/cancellation escalates from graceful to forced termination at the declared bound. The adapter separately records termination trigger and cleanup outcome; it returns `process_cleanup_failed` and quarantines staging if death/closure cannot be confirmed. Otherwise all descendants are reaped, streams reach EOF or are explicitly invalidated, no child can mutate inputs/artifacts after return, bounded evidence is retained, tool failures remain distinct from CBL-C diagnostics, and no partial artifact is published. Unsupported containment fails before spawn. |
| `BACKEND-TOOL-LIMIT-001` | Use fake tools that create file/directory-entry counts, per-file logical bytes, aggregate logical/allocated bytes, and relative-path depth/length at limit minus one, exactly at the limit, and one over; test process-tree live counts at limit minus one, exactly at the limit, and one over (including the root process, excluding threads). Test peak committed memory at byte boundaries with overlapping and staggered descendant peaks (distinguishing peak-of-simultaneous-sums from sum-of-process-peaks), and aggregate CPU exhaustion with children that exit before the root. Include concurrent descendants, sparse/oversized files, scratch-volume exhaustion, and an adapter lacking required hard quotas. Repeat with a changed invocation envelope under the same target profile. | Exact-boundary outputs are accepted only if they also satisfy the artifact manifest. Any count/byte/path/process/memory/CPU overrun yields `tool_resource_limit_exceeded` with the resource dimension, metric ID, configured limit, measured/accounted usage, and allowed enforcement overshoot; it terminates and reaps the complete child tree, leaves bounded evidence, and publishes no partial output. CPU accounting includes reaped children and all threads; memory accounting uses the maximum simultaneous sum over the live containment domain. A simulated scratch-volume-full condition is contained to the invocation's isolated storage. Missing enforcement/accounting capability yields `resource_limit_unsupported` before spawn; generated-program execution cannot run with unbounded memory or wall time, and resource-sensitive fixtures cannot run without their declared CPU budget. Changing only invocation limits changes the evidence-input identity and reruns affected gates, but leaves the target-profile contract, generated-artifact key, and COBOL digest unchanged. A post-run scan alone cannot pass a hard-quota case, and no limit failure is reported as a CBL-C source diagnostic. |
| `BACKEND-DETERMINISM-001` | Translate identical source/profile/config from different working directories and repeated clean processes. | Normalized IR, names, manifests, and COBOL are identical; only explicitly normalized absolute source paths may differ in source maps. |
| `BACKEND-MIGRATION-001` | Run the same supported corpus through the legacy emitter and each staged typed-IR/CFG/module-IR path during migration. | Compare normalized structures and execute both outputs; investigate every semantic difference. Retire the old path only after the corpus, fault injection, and profile gates pass; byte-for-byte equivalence is not required where formatting changes. |

The CFG/lifecycle tests use an independent construction/destruction ledger and
fault-injection trace. They must not derive the expected live set from emitted
cleanup statements. For path-sensitive cases, record expected block/edge IDs,
object state at block entry/exit, cleanup operations, exception state, and
source span in test fixtures. This provides a backend regression oracle before
the generated COBOL verifier is complete.

| `BACKEND-ROOT-BOOTSTRAP-001` | On every claimed profile, inspect the startup entry graph and inject failure at heap bootstrap, each post-heap runtime-service setup, and every dynamic initializer. Use a heap-backed startup-exception payload; fail reporter output and mutate lowering to report after payload destruction, service finalization, or heap release. Mutate root-record field order/initial values, add an allocation or user call before root setup, initialize another runtime service before heap readiness, replace explicit initialization with `VALUE`/implicit `WORKING-STORAGE`, make a finalizer throw, and make cleanup throw while preserving the startup exception. | The first generated startup actions explicitly establish a statically allocated, non-allocating root exception/control record and fatal marker before any potentially failing operation; heap bootstrap precedes every other runtime-service and object initializer. A runtime service ordered before heap readiness is rejected by the independent startup-graph verifier. Heap/bootstrap errors are representable and reported without entering `main`; heap-bootstrap failure has no initialized service/object cleanup, while later failures unwind globals, report while payload and required reporter services are live, destroy payload, finalize only completed services, then check/release the heap before the profile termination mapping. Reporter-output failure cannot replace the primary exception or alter termination. Finalizers are verified no-throw; a second exception triggers the specified fatal path and never replaces the primary exception. No claimed profile relies on implicit COBOL startup state. |
| `BACKEND-ACTIVATION-ROOT-001` | Before `main`, inspect and mutate initialization of the root activation-depth counter, declared limit, and strategy ID; start recursive entry at depths 0/1 and at the exact configured maximum. Inject failure while provisioning a compiler-managed activation arena after heap bootstrap; mutate counter increment/decrement on normal return, throw, rethrow, and fatal paths; make the entry activation leak into global shutdown. | Control fields are explicitly initialized before any generated call without allocation or user code. Arena provisioning occurs only after heap readiness and failure prevents entry to `main` while preserving the typed startup error. Each successful call increments and decrements exactly once across all nonfatal exits; limit+1 is detected before callee entry and leaves caller state unchanged. Normal entry completion reaches depth zero before global teardown. Fatal termination does not attempt to resume/pop dead activations or replace the primary exception. |
| `BACKEND-ACTIVATION-LIMIT-001` | At limit and limit+1, call recursive and imported callees with side-effecting receivers and scalar/borrowed/by-value RAII arguments; inject temporary-construction failures and activation-arena exhaustion. Test profile/config limits at zero, exact capacity, capacity+1, and `size_type` boundary; mutate `limit`, `requested_depth`, or `limit+1` computation. Mutate the lowering to check depth before evaluating arguments, after entering the callee, after publishing the result, or without adding a may-fail effect/CFG edge; also remove that effect only where the proof claims the bound unreachable. | Receiver and explicit arguments evaluate once in source order before activation acquisition. Limit failure prevents callee entry and result publication, destroys only completed caller-owned staged values in reverse order, preserves borrows and caller state, and propagates catchable `stack_overflow{limit, requested_depth}` with exact values and no heap allocation. Invalid profile/config limits fail before compilation or execution. The effect/ABI summary includes the failure unless a verifier-checked proof excludes it; effect removal without that proof is rejected. Successful acquisition/push and each exit/pop occur exactly once. |
| `BACKEND-ACTIVATION-STORAGE-BOUND-001` | Derive physical per-function activation bytes from profile-projected locals, temporaries, flags, result slots, and exception frames with varied `PIC`/`USAGE`, `OCCURS`, and alignment. Test `activation_limit * max_activation_record_bytes` exactly at and one byte/unit above `max_live_activation_storage_bytes`, integer overflow in the product, per-function record-size exact/over limits, and native versus compiler-managed activation strategies. Mutate one field size, frame slot count, alignment rule, limit, or arithmetic operation; re-seal the layout and profile manifest. Inject compiler-managed arena provisioning failure before `main`. | An independent layout verifier recomputes all target bytes without host `sizeof`; exact-bound configurations are accepted and over-bound/overflow configurations fail before COBOL rendering. The arena reserves the proven bound before `main`, and provisioning failure follows the startup error path without entering user code. A native strategy is accepted only with compile/run evidence sustaining the same byte bound at the configured depth; count-only evidence is insufficient. Every layout/product mutant is caught even when digests are recomputed. |
| `BACKEND-ACTIVATION-LAYOUT-EMISSION-001` | For both declared activation strategies, generate recursive and re-entrant fixtures with disjoint local, temporary, result, cleanup-flag, and exception-frame fields across two concrete functions and repeated activations of one function. Independently mutate the emitted COBOL to move a field into `WORKING-STORAGE`, alter `PIC`/`USAGE` or `OCCURS`, change an offset/slot stride, alias two logical fields, omit a reserved frame slot, map a logical field to the wrong data item, or retain a prior slot's flags/payload on reuse; re-seal generated artifacts. | The COBOL verifier reparses the actual declarations and references and matches each logical field, physical representation, storage class, slot/frame range, and access projection to the separately built activation-layout manifest. Mutable invocation state cannot leak into shared storage; every physical mapping is total, unique, and profile-supported. Recursive live invocations have distinct storage, reused slots are reinitialized before publication, and the independent execution trace confirms correct activation parentage and cleanup. Any unsupported addressing/layout form or semantic mutation fails before the artifact is accepted; a correct manifest alone cannot certify altered COBOL. |
| `BACKEND-ACTIVATION-EXCEPTION-001` | Inject activation-limit failure from an `EMPTY` call-site frame, while a catch handler is active with an inner `try`, during exception-payload `RAISING`, during `PROPAGATING`, and during cleanup. Compile a function whose required simultaneous frame depth is exactly the profile maximum and one above it; mutate fixed payload fields/publication order, frame-depth analysis, reservation mapping, state-to-edge mapping, child-frame selection, or fatal continuation. Verify no allocator/user constructor is called to produce `stack_overflow`; corrupt a reserved slot/publication as an internal invariant fault. | Exact-maximum frame demand compiles with reserved slots; one-over demand is rejected before COBOL rendering. A valid program cannot exhaust its statically reserved child-frame capacity. An empty frame propagates the declared typed limit error; an active handler receives it in its pre-reserved child frame and may catch it while retaining the parent payload. A corrupt/missing slot takes terminal fatal handling without overwriting the parent. The fixed payload is published exactly once with the configured limit and attempted depth; it performs no allocation or user call. Failure in `RAISING`, `PROPAGATING`, or `CLEANING` enters terminal `FATAL`; the original exception is not overwritten, no sibling/outer catch intercepts the second failure, and no prohibited cleanup or caller continuation executes. Every path matches the exception-state CFG and independent runtime trace. |
| `BACKEND-ACTIVATION-CONCURRENCY-001` | Apply the Phase 0 threading decision. If 1.0 is single-threaded, compile negative fixtures for source thread/task APIs and declared concurrent re-entry. If threading is supported, run concurrent recursive entries with independent locals, activation counters, exception contexts, and injected failures; mutate one shared process-global counter/frame chain, race push/pop, or cross-wire a task's parent frame. | A single-threaded build rejects unsupported concurrency before rendering and advertises no thread-safe standard-library guarantee. A threaded profile gives each task isolated activation/exception control state and meets its synchronization contract; concurrent traces show no cross-task frame/payload access, lost updates, double cleanup, or false limit errors. The same profile/feature declaration governs compiler diagnostics, ABI descriptors, and standard-library thread guarantees. |
| `BACKEND-TRACE-ACTIVATION-001` | Validate recursive/re-entrant traces with nested activation, object, and exception-frame tokens; mutate duplicate/live-token reuse, wrong parent activation, cross-context token use, missing/duplicate enter/exit, an exit before cleanup, a late/missing activation-limit event, and an unmatched active token on normal return. Include fatal termination with intentionally live activation tokens and concurrent traces only when the profile declares threading. | The independent trace validator checks token uniqueness/liveness, exact parentage, call-boundary order, cleanup-before-exit, depth-limit-before-child-entry, and exception-frame-to-activation ownership against the fixture oracle. Normal execution closes every activation exactly once; fatal traces are accepted only when the supervisor confirms the declared fatal termination and the terminal event permits remaining frames to stay live. Concurrent ordering is checked per execution context plus declared happens-before edges, never by inventing a total order. Malformed or self-consistent forged traces do not pass merely because their hashes are valid. |

| `BACKEND-EXCEPTION-TYPE-IDENTITY-001` | Compile separate thrower/catcher modules containing the same imported exception declaration, same-spelled but distinct declarations, concrete generic exception specializations, and derived/base catches. Permute module/link order; mutate canonical module/symbol/type keys, ABI/schema versions, runtime IDs, payload descriptors, lifecycle owners, base access/path/projection, or hierarchy edges. Force dispatch-ID hash collisions with a deterministic test provider and include stale/incomplete/duplicate provider records. | The same canonical declaration has the same source identity across modules; distinct declarations never match by spelling. Provider, consumer, merged link table, and generated dispatcher agree on runtime ID, payload descriptor, hierarchy, and lifecycle ownership. Valid derived-to-base catches preserve the dynamic payload identity and destroy it exactly once; inaccessible/ambiguous/cyclic/missing metadata and unresolved collisions fail before rendering/link. Canonical link-order permutations produce the same table/digest, and re-sealed semantic mutants are rejected by full-record comparison rather than only stale-hash detection. |
| `BACKEND-LINK-EXCEPTION-TABLE-001` | Link two executable roots sharing some exception providers but with distinct root-specific throwable types, imported catchers, and unreachable exception modules. Vary and permute dependency discovery; mutate the exception closure by dropping/adding a throwable or required base type, admitting an unreachable module type, changing a provider artifact/ABI tuple, editing the merged handler table, or retaining a stale table digest. | Each root's exception dispatch domain is exactly the reachable throw/catch closure plus required ancestry, independent of filesystem/module order. Shared types may appear in both root manifests, but root-private and unreachable types cannot contaminate the other table. Provider artifact, type record, ABI/payload descriptor, generated dispatcher, and link-manifest digest agree; every incomplete, extra, stale, or cross-root type fails before linker invocation. |
| `BACKEND-LINK-EXTERNAL-EXCEPTION-CLOSURE-001` | Build two executable roots with shared and root-private external COBOL dependencies. Map recognized, unknown, and call-availability failures to distinct throwable types; include an exception-aware external provider and callbacks with derived types and required base/payload/lifecycle owners. Permute dependency discovery and mutate each root's closure by dropping/adding an adapter- or callback-mapped type, base, payload/lifecycle provider, or interface digest; assign a type to the wrong root; use an unresolved mapping or conflicting type provider; then re-seal the link manifest. | Each root's exception dispatch domain is the exact reachable closure of CBL-C call throw sets, catches, external status/availability mappings, callback throw/translation/consumption contracts, exception-aware external throw sets, and required ancestry/payload/lifecycle providers. Root-private external exceptions cannot contaminate another root; shared required types remain consistent. The external dependency descriptor, type owner, ABI/payload record, generated dispatcher, and root link-manifest digest agree. Missing, extra, stale, ambiguous, unresolved, or cross-root records fail before rendering/link even though the throwing program has no generated CBL-C provider artifact. |
| `BACKEND-EXTERNAL-EXCEPTION-ADAPTER-001` | Exercise each Phase 0 external COBOL adapter mode: verified non-throwing calls, status-to-exception mappings for every declared success/failure value plus unknown status, and compatible/incompatible exception-aware ABIs. Separately trigger `CALL ... ON EXCEPTION`, an application status failure after an external side effect, partially written `out`/`inout` fields, payload construction/copy failure, callback re-entry, and an exception raised on a foreign boundary. Mutate adapter mode/version, COBOL program spelling, call count, status/output read order, output-validity declaration, effect summary, context slot, exception type/payload ABI, result-publication guard, and cleanup edge; re-seal artifacts so validation is semantic. | Returned application status is never confused with native call-resolution failure. The external call executes at most once; success-only results are unavailable on mapped failure; output validity follows the exact declared per-status contract and is never guessed; external side effects are not represented as rolled back. Mapped exceptions use the same linked type IDs and lifecycle dispatch as internal throws, and caller-owned staged objects clean exactly once. Incompatible/missing descriptors, undeclared foreign exceptions, and forged success paths fail before compile/link or at the specified native boundary. Failed payload construction enters terminal fatal handling and cannot replace the active exception. Re-entry uses the declared invocation/exception context or is rejected. The COBOL structural verifier rejects every semantic mutation, while unmutated fixtures compile and execute on every claimed profile. |
| `BACKEND-EXTERNAL-EXCEPTION-CONTEXT-001` | For exception-aware calls, execute normal return with an `EMPTY` child frame and escaping return with one complete `PROPAGATING` payload. Mutate the returned state to `RAISING`, `HANDLER-BINDING`, `HANDLER-ACTIVE`, `CLEANING`, and `FATAL`; alter parent/activation identity, type-set membership, payload-complete marker, descriptor/lifecycle owner, context ABI version, and exception frame; retain the context after return; and move validation after result reads or staged-argument cleanup. Re-seal artifacts and mutate generated COBOL independently. | Only the two declared boundary outcomes are accepted, and validation precedes all result use, handler matching, and argument cleanup. Parent and activation identity remain unchanged; escaping payload metadata matches the linked type/ABI and is complete. Any corrupt, stale, incomplete, undeclared, or retained context takes the profile's terminal fatal path without guessed cleanup or replacement exception. The structural verifier rejects altered checks/order, and execution traces prove no invalid payload reaches a catch or success result. |
| `BACKEND-EXTERNAL-CALLBACK-EXCEPTION-001` | Run synchronous external-to-CBL-C callbacks with zero/one/repeated invocations, all declared orderings around `out`/`inout` writes, sequential and nested re-entry up to the active limit, normal return, internally caught callback exceptions, escaping exceptions translated to callback status/payload, propagation back through the outer external status adapter, and callback failure consumed/continued/stopped under each declared policy. Mutate callback entry/signature/context, argument aliasing, cardinality/order, reuse of the parent's frame, activation push/pop, call-scoped retention, translated payload data, foreign exception escape, and handling of a required failure status. | Each callback has a distinct reverse call edge, activation, and child exception frame; parent state is preserved and exact cleanup occurs. Effects conservatively include every allowed invocation and aliased mutation; outputs are considered valid only under all declared callback/mutation orders. No exception unwinds through foreign COBOL. Translation preserves canonical type and payload identity on a declared propagation path, while declared consumption/continuation is observable and cannot be confused with propagation. Retained/asynchronous callbacks fail semantic validation unless a separately versioned lifetime ABI is selected. Generated wrappers and external contracts agree; structural mutants fail before COBOL compile and execution traces prove the declared callback outcome. |
| `BACKEND-EXTERNAL-CONTRACT-TRUST-001` | Exercise a compiler-verified generated provider with a complete effect closure, a trusted external assertion with pinned authority/version/digest, an untrusted or missing assertion, a profile-guarantee contract, and an external provider whose behavior contradicts its claimed non-throwing contract. Mutate provenance and attestation metadata, substitute a digest-valid but unauthorized contract, and present successful adapter structural checks without external behavior evidence. | Only a generated provider whose implementation and transitive effects pass verification is labeled compiler-verified. An external assertion remains an explicit policy-bound assumption even when its bytes/hash are valid; missing, stale, or unauthorized trust metadata is rejected or reported untrusted. Profile guarantees are accepted only within their verified capability contract. A contradicted external claim is not presented as proven by the compiler, and releases record the trust dependency separately from adapter correctness; generated-code structure and successful tests of unrelated fixtures cannot authenticate the foreign implementation. |
| `BACKEND-COBOL-EXPECTATION-EXTERNAL-001` | Independently derive expectations from imported `.cbi` adapter records plus typed calls/CFG, then compare against module IR and reparsed COBOL for all three adapter modes. Mutate only generated IR/COBOL to omit/change the external target, reorder arguments/context slots, remove or add `ON EXCEPTION`, compare the wrong status value, reverse success/failure branches, read outputs before checking status, publish a result on failure, omit an exception edge, or bypass cleanup. Also mutate only the expectation manifest and re-seal it. | The expectation builder never reads module IR, renderer output, emitter helper text, or parsed COBOL; the output cannot certify its own behavior. A separate verification step regenerates the full expected external-call records from the validated `.cbi` descriptor and typed-call/CFG inputs, then compares semantic fields as well as input identities/digests; re-sealing a forged manifest cannot make wrong expectations authoritative. The independent output verifier rejects every altered call contract/branch/result/cleanup relationship. Unmutated outputs agree structurally and then compile/execute with status, side-effect, exception, and cleanup assertions on each claimed profile. |
| `BACKEND-EXTERNAL-ADAPTER-INVALIDATION-001` | Build separate consumer/provider artifacts, then change one external descriptor field at a time: status mapping, call-availability policy, per-status output validity, effects, exception set, ownership, callback signature/mode/lifetime/cardinality/order/argument aliasing/re-entry/failure continuation, external program spelling, physical linkage slot, exception ABI, contract authority/ID/version/digest/trust policy, attestation evidence, and private provider effect implementation. Run cold/warm caches and link in both provider orders; replay the previous consumer artifact against the changed `.cbi` and re-seal a stale artifact manifest. | Every source-contract, outcome, exception, callback-call-graph, or ownership change invalidates the affected call/callback semantics and all dependent effect/activation/CFG/lifetime/render/link evidence; physical slot/spelling/ABI changes additionally update the physical fingerprint and ABI artifacts. A private implementation-effect change updates only its declared effect/link dependents when the public descriptor and behavior contract are unchanged. An authority or policy revocation blocks use and invalidates trust/release evidence; refreshed attestation for the same already accepted contract semantics reruns trust validation without perturbing generated COBOL, while changed contract content invalidates the corresponding source/effect/ABI domain. Unrelated specializations/callers remain reusable only with matching complete dependency keys. No old consumer or cached generated call is accepted against a changed descriptor, and cache key classification matches the field's declared source, ABI, effect, or evidence domain. |

| Test ID | Fixture/action | Required result |
| --- | --- | --- |
| `BACKEND-TOOL-ENVIRONMENT-001` | Launch fake tools with absent, empty, and populated environment entries; attempt undeclared parent-variable leakage, case-fold collisions, invalid/non-ASCII names, embedded NUL/`=`, malformed UTF-8, Windows UTF-16 conversion failure, mutable values after snapshot, and overrides of locale, `PATH`, or compiler option variables. Exercise per-entry/count/aggregate-UTF-8/native-serialized limits at limit-minus-one, exact, and plus-one, including simultaneous overages. Vary environment ordering and host adapter versions. | The child receives exactly the declared environment plus only adapter-declared minimal host-launch entries; absent differs from empty, and no undeclared parent variables or secrets leak. Names are ASCII-valid, case-fold unique, deterministically serialized, and values are strictly encoded with no replacement. All exceeded dimensions are recorded; preflight returns `tool_environment_limit_exceeded` with no child status, using the specified deterministic primary-failure order. Changing values or limits updates invocation evidence only, not generated program artifacts; platform conversions preserve the same logical map or the host/profile pair is unsupported. Evidence stores value digests rather than secret values. |
| `BACKEND-TOOL-ENVIRONMENT-DIGEST-001` | Compare independent golden vectors for empty, absent, empty-valued, multi-entry, case/permutation, Unicode-value, and adapter-host-entry maps. Reorder entries; vary one name byte, value byte, value length, schema/domain prefix, field order, exact spelling, namespace, and namespaced host-entry value digest; attempt cross-namespace duplicate case-folded keys and forged/resealed map digests. | Independent implementations produce identical value and map SHA-256 digests from the specified domain-separated canonical encoding and exact record shape, regardless of input order or host endianness. Absent and empty values differ; any semantic key/value/host-entry change alters the digest; duplicate folded keys and noncanonical encodings fail before spawn. Re-sealing a forged digest cannot authorize a map that differs field-for-field from the staged environment. |
| `BACKEND-PROFILE-INVOCATION-001` | Compile the same frozen source under a fixed/free-format target profile using matching options, then contradict that profile with command-line flags, response-file entries, environment variables, or compiler configuration defaults. Exercise nested response files, cycles, undeclared/dynamic includes, malformed encodings, and mutation between snapshot and spawn. Upgrade the compiler within the allowed compatibility range where defaults change; vary only executable selection/argument spelling while preserving the verified capability mapping; independently mutate target-profile capabilities/version and invocation descriptor/options. Replay old compile evidence after each change. | The invocation descriptor's effective options—including exact response-file bytes, closure, grammar, and digest—are explicit and evidence-bound, and must implement, not redefine, the target profile's source format, dialect, and semantic capabilities. A conflict, undeclared option source, mutable/dynamic response file, or drifted default fails before accepting compiler output; profile semantics cannot be inferred from compiler defaults. Tool-selection or invocation-only changes rerun affected validation evidence while preserving generated artifacts when the profile contract is unchanged. A true semantic-profile change bumps its contract version and invalidates affected profile-lowered artifacts and downstream evidence. No prior compile result is reused across changed effective options, response-file bytes, profile capability, or tool identity. |
| `BACKEND-TOOL-LAUNCH-LIMIT-001` | On Windows and POSIX adapters, vary executable/runtime-launcher bytes, tool arguments, generated-program arguments, and environment serialization so each individual limit is below/equal/above its bound and the combined native launch payload is below/equal/above the joint bound. Include cases where every individual bound passes but only the combined bound is exceeded; vary the actual native launch ceiling/resource constraints; mutate the metric formula/version and attempt to change the environment after preflight. | The adapter uses the declared versioned metric for the actual host call representation, includes all required components/terminators/overhead, binds the effective ceiling/source for the attempt, and checks the joint limit before spawning. A joint-only excess returns `tool_launch_payload_limit_exceeded` with component breakdown, measured/bound bytes, and no child status; it is not mislabeled as an argv or environment-only excess. An unmeasurable or unenforceable host limit fails as `resource_limit_unsupported`. Changing only host launch limits/metric reruns invocation evidence and preserves generated artifact identity; any value mutation after preflight is detected and no tool is launched. |
| `BACKEND-TOOL-RESULT-001` | For each pinned compiler adapter, test zero/nonzero/abnormal process termination with complete, absent, partial, stale, and unexpected output artifacts. Pair each with parsed warning/error/note records, unrecognized diagnostic text, empty stdout/stderr, contradictory text versus exit status, and each versioned warning/unclassified-diagnostic policy. Exercise simultaneous timeout plus diagnostic output, output-cap plus rejected warning, cleanup failure after child exit zero, and nonzero exit plus artifact mismatch. Mutate only the diagnostic parser, warning disposition, raw-status mapping, or output-manifest expectation; include compiler success followed by output capture/cleanup failure. | Compile acceptance is derived from the supervised process result, the tool-adapter's per-phase process-result mapping, required output-manifest validation, and the declared warning policy—not substring heuristics over human-readable text. A non-success primary tool outcome, missing or partial required output, capture/cleanup failure, recognized error, or rejected warning cannot pass even if plausible output exists. A zero exit with complete outputs is accepted only when outputs satisfy the independent artifact/COBOL validators and all diagnostics meet policy; unclassified diagnostics follow the explicit policy and never silently disappear. Overall-outcome precedence remains the runner's normative ordering while every simultaneous condition, raw status, trigger, diagnostic, and artifact fact is retained separately. Raw stdout/stderr and parsed/unparsed diagnostics are retained separately. Parser/policy/status/output-contract changes invalidate affected validation evidence, not generated COBOL identity; stale or contradictory evidence is rejected. |
| `BACKEND-TOOLCHAIN-BINDING-001` | Resolve a compiler through PATH, an explicit path, a wrapper, and a package-managed toolchain. Replace or retarget the executable after identity inspection but before spawn; mutate it while the invocation is running; exercise symlink/launcher indirection, compiler-to-linker child invocation, response files, ambient option/config variables, environment-controlled include/plugin paths, and runtime-library replacement after link. Mutate an executable, downstream tool, or runtime/library package while retaining the same self-reported version; supply a version string that claims an allowed release for a disallowed binary; vary harmless host-specific executable paths while keeping the verified tool closure identical. | The attempt record identifies the immutable verified toolchain/runtime closure that actually ran, including compiler/linker children and all behavior-affecting options/configuration inputs. Hashing a mutable path and later launching it is insufficient; the adapter either prevents substitution or detects it before accepting results. Reported version alone never authenticates identity. Undeclared wrappers, downstream binaries, ambient options, plugins, runtime libraries, or identity races fail closed and produce no accepted compile/link/run artifact or conformance evidence. Path changes alone do not change semantic artifacts or tool compatibility identity when the verified closure is the same; changing any executable, package, argument policy, or behavior-affecting configuration invalidates the affected attempt evidence. |

| Test ID | Fixture/action | Required result |
| --- | --- | --- |
| `BACKEND-RELEASE-INVOCATION-001` | Build one required CI matrix row, then independently change its effective compiler arguments, response-file bytes, tool-environment map/limits, launch-payload metric/limit, warning/diagnostic policy, tool identity, and semantic target profile. Replay old compile/link/run attempts and signed evidence against each row; remove or relabel a row while retaining its passing evidence. | Every CI attempt and attestation binds the exact semantic profile and invocation-descriptor/effective-input digests. Any tool-invocation-only change rejects stale evidence and reruns affected compiler/linker/runtime validation while preserving generated artifacts if semantic profile inputs are unchanged. A target-profile semantic change invalidates the affected profile-lowered artifacts as well as evidence. Missing, relabeled, or mismatched matrix rows cannot be counted as required passes; tool identity or reported version alone cannot substitute for the invocation binding. |

### 8.8 Target matrix, integration, fuzzing, and release gates

Represent the release matrix as a versioned, reviewable artifact rather than
an informal list of CI jobs. Each row describes one claimed or explicitly
unsupported host/toolchain/profile combination; do not imply a full Cartesian
product where a compiler or dialect is not supported on a host, and do not
infer support from a job that happened to run. The matrix artifact contains:

| Field | Contract |
| --- | --- |
| `matrix_id` / `matrix_version` | Stable identity and schema version; changing row membership, requiredness, test coverage, or claimed support creates a new validation-plan identity. |
| `row_id` and requirement state | Canonical unique row ID and `required`, `informational`, or `unsupported` state. Only `required` rows establish release support; an unsupported combination is documented and cannot be represented as passed. |
| Host/build environment | OS family/version, runner image or immutable environment identity, filesystem capability profile, architecture, and any host-specific adapter version. These select test tooling only, never CBL-C semantics. |
| Semantic target profile | Target-profile ID/version and capability-contract digest, including source format/dialect and promised COBOL behavior. Compiler defaults or brand do not define this contract. |
| Tool invocation descriptor | Exact descriptor schema/version/digest; tool-compatibility policy; verified compiler/linker/runtime identities; effective ordered tool arguments and immutable response-file closure; tool-environment map/policy/limits; joint launch-payload metric/limit; warning/unclassified-diagnostic policy; process-result mapping; and adapter version. A compiler brand without an exact profile and invocation descriptor is not a row. |
| Required evidence set | Stable test-suite/test-case IDs, fixture/reference-data manifest digests, required compile/link/run stages, resource limits, and expected artifacts/outcomes. Each promised capability is mapped to at least one required executable test, not only a parser or marker assertion. |
| Evidence binding | Source/artifact-manifest digest, row and matrix IDs, workflow identity/ref, resolved tool identities, exact invocation-descriptor digest and effective-input digests (including response files and environment-value digests), test-plan/data digests, per-stage outcomes, and signed/trusted attestation identity. Missing or untrusted or mismatched invocation evidence remains unverified. |

For every profile claimed portable across the project-supported hosts, include
required Windows and POSIX rows for generated-source identity and actual
compile/link/run behavior. A named COBOL compiler need not exist on every host,
but its unsupported host pairs must be explicit; that narrower matrix does not
make the compiler's profile a common cross-host baseline. The common typed-pool
profile must have at least one required Windows row and one required POSIX row.
Build/test tooling may use different host adapters, but the semantic
profile/capability contract and source fixtures remain the same. For identical
artifact inputs, compare generated COBOL, ABI descriptors, link plans, and
expectation manifests across those rows; only explicitly host-neutralized
presentation/source-map fields may differ. Runtime results are compared using
the portable outcome contract, with raw native process/compiler status retained
as evidence rather than treated as cross-platform semantics.

Generate the CI job matrix from this artifact so CI cannot quietly omit a
required row while the documentation still claims it. Required rows may not be
skipped, xfailed, or treated as optional when a tool is unavailable; they fail
the release gate. Local runs may report `not_run`/`unsupported_here`, but these
statuses never count as release evidence. Informational rows are visible and
cannot replace required rows. A matrix-only change invalidates validation
evidence and trusted CI attestations but does not invalidate generated COBOL or
semantic/profile artifacts unless the target profile itself changed.

- Run the same source corpus on Windows, POSIX, and each supported COBOL
  compiler/profile. Compare semantics and documented differences, not vendor
  diagnostic wording.
- Integration programs: single-file CLI; multi-module file processor;
  `vector<string>`, `vector<vector<string>>`, four/five-level and depth-16
  nested vectors; mixed nested sequence/associative/ownership types; exception
  through imported modules and every nested layer; allocation failure caught
  at outer/inner/leaf allocations; file error caught; clean build from
  embedded library sources with source directory absent at runtime.
- Fuzz lexer/parser, malformed literals, template graphs/substitutions, UTF-8
  strings/parsers, iterator ranges, and randomized container operations.
  Instrument ownership counters and host runtime memory tools where available.
  Every crash, mismatch, timeout, leak, or unexpected exception becomes a
  minimized deterministic regression. Include nested template/type graphs and
  call/member/operator chains through depth 16; bound generated input size so
  fuzzing tests compiler robustness without hiding the deterministic depth
  conformance suite.
- Property/reference models: vector/list/deque against a sequence model;
  maps/sets against ordered key models; unordered containers against key/value
  models independent of bucket order; strings against byte/UTF-8 references;
  lifecycle counts against constructed-object ledgers. Add recursive nested
  sequence models at depths 1, 2, 4, 5, 8, and 16, comparing generated
  operations, deep copy independence, move-from validity, access-chain
  mutation, and recursive destruction against a simple tree-of-vectors model.
- Golden COBOL and ABI tests for every exception/cleanup edge and library
  signature; compile generated artifacts twice and assert determinism.
- Test-runner meta-tests prove fault injection fires at each allocation/call
  site, death tests detect abnormal termination, skip/xfail is visible,
  artifacts stay outside tracked paths, and no test writes
  `automated_tests.log`.
- Release gate: zero unexplained failures/skips; every catalog item passes on
  required profiles or has an approved capability exclusion; every `required`
  release-matrix row has passing trusted evidence bound to the release
  manifest, with no required row skipped or downgraded; no open severity-1
  correctness/exception/ownership/portability issue; all regressions pass from
  a clean checkout.

Avoid using the C++ standard library as an unquestioned oracle for semantics
CBL-C deliberately changes. Use simple mathematical/reference models for
container behavior and explicit CBL-C assertions for every intentional
difference.

## 9. Compatibility and release governance

Use a semantic version for the language standard and separately version the
generated COBOL ABI. During 0.x, breaking changes are permitted but must update
the specification, generated ABI version, migration note, samples, and tests
in one change. Within 1.x, source and behavioral compatibility are expected
for documented stable features; ABI compatibility is maintained only for the
declared ABI major version.

Every public library addition must include:

1. canonical `.cblc` implementation and public declaration;
2. specified signatures, ownership, errors, complexity and invalidation;
3. implementation and generated-COBOL tests;
4. cross-module and cross-target evidence;
5. examples and a feature-status update; and
6. a compatibility statement.

Do not mark a feature complete because one sample compiles. “Complete” means
semantics, lifecycle, generated COBOL, ABI, tests, documentation, and supported
target behavior all agree.

## 10. Main risks and mitigations

| Risk | Consequence | Mitigation |
| --- | --- | --- |
| Containers arrive before non-trivial lifecycle support | leaks, double destruction, string corruption, unsafe relocation | Make generic construct/copy/move/destroy dispatch a hard prerequisite. |
| Template errors surface during emission | confusing failures and nondeterministic COBOL | Canonical structural types and instantiate-time semantic checking. |
| C++ COBOL templates return as a shortcut | two sources of truth and no native CBL-C library | `.cblc`-only public implementations plus embed consistency checks. |
| Exception cleanup is piecemeal | skipped/double cleanup on return/loop exits | Central cleanup-aware control-flow lowering and edge-complete tests. |
| `shared_ptr` is exposed without weak ownership or thread policy | cycles leak or users assume false thread safety | Implement shared/weak as a pair and declare threading contract. |
| COBOL vendors differ in intrinsic, file, numeric, or memory behavior | platform-dependent results | Portable common subset, capability profiles, target CI, adapter boundary. |
| C/C++ status and UB conventions leak into APIs | ambiguous errors and fragile code | Defined CBL-C semantics, typed errors/results, no meaningless status slots. |
| The roadmap promises every C++ library feature | 1.0 never ships | Freeze the explicit 1.0 catalog; defer advanced facilities by named policy. |

## 11. Recommended immediate next steps

1. Approve the 1.0 feature scope and choose whether threading is in or out of
   1.0; preserve all listed families either way, with thread support gated by
   the selected profile.
2. Re-run clean-checkout initialization/build/test and replace no history:
   append a dated status to the 2025 build report, reconcile the obsolete
   `libft` submodule claim, and establish current test evidence.
3. Reconcile the language/library/ABI/dialect documents; publish the support
   matrix and explicit fixed/free COBOL profiles; add required CI workflows
   for Windows, POSIX, and selected COBOL compilers.
4. Build the end-to-end test harness, IR expectation manifest, fault-injection
   seams, counted RAII probes, generated-output mutation tests, and the
   per-entry ABI/status classification; migrate clamping/no-op vector tests
   to 1.0 error and post-failure assertions before counting translation-only
   tests as feature evidence.
5. Implement structural type identity and explicit lifecycle dispatch; settle
   integer/overflow, null/index, ownership/move, `new`/`delete`, object-live
   versus storage-capacity state, partial construction, and ABI rules.
6. Prove the normal-path `vector<string>` lifecycle by compiling and executing
   generated COBOL, including copy/move/self-assignment, nested scopes,
   growth/reallocation, insertion, erase, clear, shrink, and exact destruction
   of live elements only. Do not mark it generally supported yet.
7. Prove `vector<vector<string>>` and operator/member chaining at every depth
   1–16; treat depths 2/4/8 as checkpoints and 16 as the release gate.
8. Complete payload-aware exceptions for every 1.0 throwable layout, then add
   allocation/constructor/reallocation/cleanup fault injection, cross-module
   ABI propagation, and immediate abort on a second exception; rerun the
   nested suite at depths 4, 5, 8, and 16.
9. Deliver the dedicated string subsystem: private ownership, UTF-8 validation,
   strict parse/range errors, strong mutation guarantees, and bounded interop;
   rerun all vector and nested-vector evidence.
10. Complete iterators/borrows and algorithms, then add `array`, list/deque
    families, ordered maps/sets, unordered maps/sets, and adaptors in Phases
    7–8.
11. Add `unique_ptr`, `shared_ptr`, `weak_ptr`, and broader utility ownership
    cases in Phase 9; follow with portable I/O/filesystem, date/time, random,
    math, environment/process, packaging, and release-matrix work.

## 12. Research references

The references below are primary WG21 working-draft text. The draft is a
moving document and is consulted as a design catalogue, not as CBL-C's
normative specification. Pin a published C++ edition or draft revision in
future compatibility studies if an exact clause snapshot is needed.

- [Containers overview](https://eel.is/c++draft/containers)
- [Container requirements](https://eel.is/c++draft/container.requirements)
- [Allocator-aware containers](https://eel.is/c++draft/container.alloc.reqmts)
- [Sequence containers](https://eel.is/c++draft/sequence.reqmts)
- [Ordered associative map](https://eel.is/c++draft/map.overview)
- [Unordered associative containers](https://eel.is/c++draft/unord.req.general)
- [Iterator library](https://eel.is/c++draft/iterators.general)
- [Ranges requirements](https://eel.is/c++draft/range.req)
- [Unique ownership](https://eel.is/c++draft/unique.ptr)
- [Shared ownership](https://eel.is/c++draft/util.smartptr.shared)
- [Weak ownership](https://eel.is/c++draft/util.smartptr.weak)
- [Exception unwinding](https://eel.is/c++draft/except.ctor)
- [Exception termination](https://eel.is/c++draft/except.terminate)
- [WG21 / ISO C++ committee working documents](https://www.open-std.org/jtc1/sc22/wg21/)
