# CBL-C Language Standard and Compiler Behavior

**Status:** authoritative repository specification
**Version:** 0.6 (bounded template subset and versioned fixed-array function ABIs)
**Last reviewed:** 2026-08-07

This document is the normative reference for CBL-C as implemented by this
repository. It specifies source syntax, static semantics, runtime behavior,
diagnostics, module behavior, and the behavior of the CBL-C-to-COBOL compiler
pipeline. CBL-C-to-C translation is intentionally removed from the supported
product surface.

The word **must** describes a required rule. **May** describes an allowed
implementation choice that does not change observable CBL-C behavior. A
feature marked **implemented** is part of the current supported language. A
feature marked **partial** is accepted only for the cases described here. A
feature marked **unsupported** is not part of this version, even if a backend
or the parser happens to recognize a fragment of it.

This file is the language contract. The following documents are subordinate
references:

- [`runtime_api_reference.md`](runtime_api_reference.md) gives the runtime and
  standard-library ABI details.
- [`abi_spec.md`](abi_spec.md) gives generated-program calling conventions and
  storage layout.
- [`cblc_sample_inventory.md`](cblc_sample_inventory.md) provides executable
  examples and feature coverage.
- [`../compiler_feature_tracker.md`](../compiler_feature_tracker.md) records
  implementation progress and known work items; it does not override this
  standard.
- [`getting_started.md`](getting_started.md), [`onboarding_checklist.md`](onboarding_checklist.md),
  and [`contributing.md`](contributing.md) document usage and contribution
  workflow around the language.
- [`cli_usage_examples.md`](cli_usage_examples.md) documents the command-line
  interface, while [`ci_pipeline.md`](ci_pipeline.md) documents verification.
- [`cobol_dialect_requirements.md`](cobol_dialect_requirements.md) documents
  the target COBOL profile.

When implementation and documentation disagree, the discrepancy is a compiler
bug or a documentation bug. It must be resolved explicitly; readers must not
infer a third behavior from whichever backend happens to accept the input.

This document contains both normative rules and implementation-status notes.
Normative rules use **must**, **must not**, and **may**. Status notes are marked
**current**, **partial**, **planned**, or **gap** and describe what the present
repository can prove. A planned rule is not an implemented language feature.

## 0. Reading and maintenance model

The standard is maintained as a layered contract:

| Layer | Answers | Change impact |
| --- | --- | --- |
| lexical/syntax | What source text is accepted? | Parser, formatter, lexer, syntax tests |
| static semantics | What does accepted source mean? | Symbol tables, semantic checks, diagnostics |
| execution semantics | What happens at runtime? | Runtime helpers, error/status behavior |
| target contract | How is meaning represented in C/COBOL? | Emitters, ABI, generated-source tests |
| artifact contract | What files are produced and consumed? | CLI, packaging, manifests, build tests |
| tooling contract | How do formatter, LSP, reverse translation, and maps behave? | Tooling and round-trip tests |

Every feature must identify which layers it changes. A feature is not complete
when only the parser accepts it.

For each feature, maintain a small record with this shape:

```text
Feature ID: CBLC-<area>-<name>
Status: current | partial | planned | deprecated | removed
Source forms: exact examples and rejected forms
Static meaning: types, scope, mutability, evaluation, lifetime
Runtime meaning: normal result, failure, status, and side effects
Targets: COBOL lowering, C lowering, unavailable-target policy
Artifacts: generated helpers, dependencies, names, and ABI
Compatibility: source, semantic, ABI, artifact, and diagnostic impact
Evidence: positive, negative, backend, runtime, round-trip, and docs tests
```

This record is the expansion unit. New features should add a record and
subsections rather than rewriting the meaning of unrelated features.

## 1. Scope and conformance

CBL-C is a small, statically checked, C-like source language designed to lower
deterministically to COBOL. It is not general-purpose C++ or portable C and
does not promise source compatibility with C, C++, or any COBOL edition.

A conforming implementation of this repository's language must:

1. reject source that violates the lexical, syntactic, or semantic rules here;
2. report source locations for diagnostics where a location is available;
3. preserve the specified evaluation and lowering behavior;
4. produce the specified observable behavior in the supported CBL-C-to-COBOL
   path; and
5. keep the parser, semantic pass, generators, formatter, samples, and tests
   synchronized when adding a language feature.

The current compiler accepts a practical implementation subset. It does not
claim to implement every construct suggested by C-like syntax.

## 2. Compiler pipeline

For a CBL-C source unit, the compiler performs these conceptual stages:

1. **Input and module discovery:** reads source files, resolves `import`
   directives, and registers translation-unit metadata.
2. **Lexing and parsing:** recognizes declarations, functions, class/struct
   members, statements, and expressions.
3. **Symbol registration:** records types, data items, functions, methods,
   constructors, imports, and exported signatures.
4. **Semantic analysis:** checks names, scopes, types, visibility, mutability,
   argument counts, return rules, pointer compatibility, string capacity,
   initialization, and control-flow constraints.
5. **Lowering:** converts source constructs into the shared internal model and
   target-specific operations.
6. **Emission:** writes COBOL or C source, source maps, diagnostics, and any
   requested intermediate or visualization output.

Parsing success does not imply compilation success. A source file can be
syntactically valid and still be rejected by semantic analysis.

The compiler is deterministic: the same source, compiler configuration, and
input module graph must produce the same semantic result and equivalent output.

## 3. Lexical rules

### 3.1 Identifiers

Keywords are matched case-insensitively by the lexer/parser. User-defined
identifiers are matched by their source spelling in the CBL-C symbol tables;
implementations must not silently merge `Count` and `count`. Qualified
standard-library names use the registry's exact spelling. The original spelling
is retained for source maps and diagnostics even when a target name is
normalized. An identifier must not collide with a reserved keyword in the
position where the keyword is recognized. Generated target names may be
normalized and disambiguated; this does not change source lookup.

Identifiers are subject to the repository's configured maximum lengths. A name
that cannot be represented in the compiler's fixed metadata buffers is an error,
not a silently truncated name.

### 3.2 Literals

The implemented literal families are:

- decimal integer literals;
- character literals, including supported escape forms;
- string literals, including supported escape forms;
- `true` and `false`; and
- floating-point literals where the selected type and backend support them.

Negative numbers are unary negation applied to a positive literal; `-42` is not
a separate lexical integer category. Literal conversion must preserve the
diagnostic and range rules of the destination type.

### 3.3 Comments and whitespace

Whitespace separates tokens and has no semantic meaning except where it prevents
two tokens from merging. The lexer accepts the comment forms implemented by the
frontend. Comments are retained in the structures needed by formatter,
round-trip, and source-map behavior where supported.

### 3.4 Syntax notation and reserved surface

The syntax in this document is normative by example and by the explicit source
forms in each feature record. When a compact grammar is needed, use this EBNF
notation:

```text
production ::= alternative | alternative
[item]     optional item
{item}     zero or more item repetitions
'text'     literal token
NAME       named token or production
```

New productions must be added near the feature that owns them and must include
at least one accepted example and one rejected boundary case. The lexer token
registry and parser entry points are implementation details; they do not make
an otherwise undocumented token part of CBL-C.

## 4. Translation units and modules

A translation unit may contain imports, copy directives, declarations, and
function or method definitions. A multi-file build is a module graph, not a
textual concatenation.

```cblc
import "metrics_worker.cblc";
copy "shared.cpy";
```

`import` makes public declarations from another CBL-C translation unit
available for signature checking and calls. The imported file must be supplied
to the compiler invocation or be resolvable through the configured input path.
Imports must not introduce an unresolved cycle or ambiguous public symbol.

`copy` records a copybook dependency for the supported reverse/round-trip
workflow. It is not a general C preprocessor and does not provide arbitrary
macro expansion.

Imported functions, types, and data are checked through exported signatures.
Private members remain private across module boundaries.

Module identity is based on the resolved input/module path and the compiler's
module registration rules, not merely on a display filename. An import must
resolve to exactly one module. The compiler must reject missing imports,
duplicate module identities, import cycles, and inaccessible private symbols.
Public symbol conflicts must be diagnosed before target emission. These rules
are required so adding a package/module system later does not change the
meaning of an existing import silently.

## 5. Program structure

The supported top-level forms are:

- scalar, character-buffer, string, pointer, array, record, and class data;
- `struct`/`record` type declarations;
- `class` declarations;
- function definitions;
- qualified out-of-class method and constructor definitions;
- `import` and supported `copy` directives; and
- supported file declarations.

The entry program must provide a `void main()` entrypoint for the normal
forward compilation workflow. Helper functions may return values. A function's
parameter list is part of its signature; overloads are not general-purpose
function overloading, although constructor overload selection is supported as
described below.

Declarations must be visible according to the normal source-order and module
registration rules. A qualified method definition must follow the declaration
of its owning class in the translation-unit/module information available to the
compiler.

## 6. Type system

The type system is statically checked. Every data item has a declared kind,
optional type name, capacity or array metadata, scope, mutability, and target
representation.

### 6.1 Scalar types

The supported scalar surface includes:

| Source type | Meaning | Notes |
| --- | --- | --- |
| `bool` | Boolean value | Lowered to the target's boolean/flag representation. |
| `char` | Character or fixed character storage | Array form represents a buffer. |
| `int` | Signed integral value | Width is selected by the compiler's type policy. |
| `long` | Widened signed integral value | Supported where the current backend has a matching representation. |
| `long long` | Further widened signed integral value | Supported where the current backend has a matching representation. |
| `float` | Floating value | Backend representation is target-defined but documented by the ABI. |
| `double` | Double-precision floating value | COBOL emission uses the documented floating representation. |
| `void` | No value | Valid as a function/method return type only. |

The exact target pictures and calling slots are specified in
[`abi_spec.md`](abi_spec.md). Source code must not depend on a target's byte
layout unless it uses the explicit pointer/ABI rules in this standard.

### 6.2 Character buffers and arrays

`char name[n]` declares fixed-size character storage. Numeric and aggregate
arrays use the corresponding element type and a compile-time count. The count
must be positive and representable in compiler metadata.

Array indexing and pointer indexing are supported only in the forms accepted by
the current parser and semantic checks. The language does not promise automatic
runtime bounds checking for every index. A statically provable invalid access
must be rejected; an unprovable access has target/runtime behavior and is not a
portable way to avoid bounds validation. Future bounds-checking features must
define their cost, failure result, and interaction with pointer arithmetic
without changing existing pointer ABI rules.

### 6.3 Built-in `string`

`string` is a managed, fixed-capacity value object, not a null-terminated C
pointer. A capacity is supplied using the supported constructor syntax:

```cblc
string greeting(32);
string message(64) = "hello";
```

The object stores logical length and backing capacity. Assignments and mutating
operations must not silently exceed capacity. The semantic pass reports a
truncation/capacity diagnostic when it can determine that the operation cannot
fit. Runtime helpers report the documented status for dynamic failures.

Supported operations include construction, assignment, append, clear, length,
capacity, empty, equality, comparison, contains, starts-with, and ends-with in
the spellings recognized by the parser and runtime. A string array uses the
explicit array-plus-capacity form supported by the declaration parser.

The object is copied according to value semantics in supported assignments and
copy-constructor-style flows. It must not be treated as a raw pointer or
manually mutated through undocumented backing storage. The standard-library
operation table is the authoritative list of supported string members; a name
appearing in editor completion or a backend helper is not automatically a
source-language operation.

### 6.4 Records and structs

`record` and `struct` declarations introduce aggregate types. Fields are stored
in declaration order and accessed with member selection. Records are data
aggregates; classes additionally provide methods and lifecycle behavior.

Nested fields and supported arrays preserve their declared member path in the
semantic model and generated group/storage representation.

### 6.5 Classes

A class combines member storage with named constructors and methods. It is not
a virtual object system. The following are unsupported unless explicitly added
by a future standard revision: inheritance, virtual dispatch, vtables,
operator overloading, lambdas, exceptions, and arbitrary namespaces.

Classes support public and private members, in-class method bodies, qualified
out-of-class method definitions, constructors, methods, const enforcement, and
supported destructor-like lifecycle hooks.

Member access is checked semantically. A private field or method cannot be used
from an unauthorized caller merely because the generated target could represent
the access.

### 6.5.1 Method emission and reuse

A user-defined method is one implementation, not a macro. The compiler must
not paste a method's body into every call site. The required lowering model is:

```text
CBL-C method declaration/body
        ↓ semantic method IR
one generated target method block
        ↑ receiver + arguments + result slot
each call site emits only a call
```

The semantic method IR must retain symbolic receiver/member references rather
than permanently replacing `this` with the caller's storage name. The target
emitter resolves those references once while emitting the method block. A
method block must contain its own parameter/local storage contract, receiver
contract, return contract, and lifecycle/error behavior.

For the COBOL backend, the block is a generated callable paragraph/subprogram
according to the target ABI, with the
receiver passed by reference and value results/status passed through explicit
slots. A call site may contain argument preparation and result movement, but it
must not contain the method's implementation statements.

The implementation may specialize a method for a concrete receiver type, but
that specialization must still be emitted once and referenced by name. Stable
names must include enough module/type/method/signature information to avoid
collisions without depending on source call-site order.

### 6.5.2 Bounded type templates

CBL-C supports a bounded compile-time type-substitution facility. A template is
monomorphized into an ordinary concrete type or function before COBOL emission;
it does not introduce runtime polymorphism, inheritance, virtual dispatch, or
dynamic type information.

The supported declaration forms are:

```cblc
template <typename T>
struct box { T value; };

template <typename T, typename U>
struct pair_value { T first; U second; };

template <typename T>
T identity(T value) { return value; }
```

The parameter list must contain one to four distinct `typename` parameters.
Non-type parameters, parameter packs, defaults, constraints, specialization,
and template-template parameters are unsupported and must be rejected.

An explicit type application uses an ordered, comma-separated argument list:

```cblc
box<int> scalar;
box<int*> pointer_value;
box<int[3]> fixed_values;
pair_value<int, char> pair;
```

Struct fields may use the explicit borrowed-pointer form for the supported
pointer kinds, for example `borrowed int *ptr;` or `borrowed Node *next;`.
This annotation is required for pointer fields participating in a supported
struct-array ABI; an unannotated or owning pointer field is not implicitly
treated as borrowed.

Nested applications are permitted when each inner application resolves to a
concrete type. The supported argument shapes are the existing CBL-C scalar,
string, struct/class, pointer, and fixed-array shapes. Pointer depth and the
fixed array count are part of type identity; `int*`, `int**`, and `int[3]` are
not interchangeable with `int`.

Template fields, function parameters, method parameters, and dependent returns
are substituted structurally. A dependent field retains the argument's kind,
struct identity, pointer shape, and fixed-array count. A template definition is
immutable, and repeated requests for the same ordered canonical argument list
must reuse one concrete instantiation.

For concrete struct and class arguments, struct identity includes a versioned
layout fingerprint (`CBLC-LAYOUT@1`). The fingerprint covers field order and
names, storage kinds, lengths, array bounds, constness, declared and nested
type identities, and whether the enclosing type is a class. Exported module
metadata carries the fingerprint, and importing a same-named type with a
different fingerprint is rejected; an imported definition must never silently
overwrite a local or previously imported layout.

Free-function applications may be explicit, for example
`identity<int>(value)`. The current deduction subset also permits exact
one-parameter deduction for supported concrete arguments. Deduction must not
infer through conversions, overload sets, pointer covariance, or runtime values.

Concrete names are compiler-generated from the template name and canonical
argument key. Names must be deterministic, bounded, and collision-safe; source
punctuation used for pointer and array shape must not be copied directly into a
COBOL identifier. The generated COBOL program must contain only concrete
paragraphs, groups, and calls, with no unresolved `T` or `typename`.

The current template feature is partial. Fixed-array arguments are supported in
function parameter positions for integer, explicitly-capacitated string, and
trivially-copyable concrete struct element arrays. Integer arrays use an
exact-shape, by-value-copy ABI: the
caller must pass a named `int[N]` item and the callee receives an independent
`OCCURS N TIMES` item. String arrays use the form `string(C)[N]`; both `C` and
`N` are part of type identity, the caller must pass a named array with exactly
the same capacity and count, and each element's length and active buffer are
copied into callee-owned storage. No array-to-pointer decay, implicit bounds
conversion, dynamic-pointer transfer, or truncation is permitted. Trivially
copyable struct arrays use exact concrete struct identity and per-element group
copies of scalar fields, including fixed character buffers. Mutable string
fields are deep-copied per element: the callee receives independent `PTR`,
`CAP`, `LEN`, and buffer storage, and the generated lifecycle releases every
callee allocation. Structs containing pointer fields are accepted in this
array ABI only when the field is explicitly declared `borrowed`; those fields
are copied shallowly and the callee does not free or otherwise own the pointee.
Owning pointer fields remain rejected. Indexed expressions, literals, mismatched
capacities/bounds, and unsupported return/storage shapes remain rejected until
their ABI is specified. Within a struct array, a
`const string(C)` field is represented as a fixed character buffer and copied
by value. A null mutable source element produces a null destination element;
non-null elements preserve the source capacity and active length.
Struct arrays may provide constructor arguments using `Type[N](arguments)`.
The selected constructor is applied independently to every element in index
order after the element's storage is initialized. Constructor parameters use
the ordinary by-value/by-reference ABI, and constructor receiver fields are
lowered with the active OCCURS subscript. A constructor must be valid for the
supplied arguments; array construction without arguments still requires a
default constructor when the type has user-declared constructors.
Adding any new template form requires updating this section, the feature
registry, diagnostics, formatter/tooling behavior, and both direct and
GnuCOBOL-backed tests.

The string-array ABI is versioned separately from the integer ABI. Its
capacity-bearing type identity prevents a smaller callee buffer from silently
truncating a source element. Dynamic source pointers remain owned by the
caller; the callee allocates and cleans up its own element storage. The
enabled struct-array subset carries concrete struct identity and element count
and copies each complete element group. The current layout fingerprint crosses
the module metadata boundary so imported struct identities cannot silently
drift. Borrowed pointer values must remain valid for the complete callee use;
the compiler does not extend their lifetime or detect dangling aliases. Nested
owning pointer fields, richer layout-version migration beyond `CBLC-LAYOUT@1`,
and other
non-trivial ownership forms remain deferred extensions.

Template instantiation failures retain their stable diagnostic code and message
and also record structured context for frontend and tooling consumers. When
available, that context contains the template's source name, the canonical
ordered argument key, the template definition offset, and the failing use
offset. An unavailable offset is represented by the invalid-offset sentinel;
consumers must not infer a source location from the diagnostic message. The
canonical argument key is the same key used for cache identity and generated
names, so diagnostics and artifacts cannot disagree about which specialization
failed.

### 6.6 Pointers

The supported pointer surface includes `void *`, `char *`, `int *`, pointers to
supported struct/class types, pointer indexing, supported pointer arithmetic,
address-of, dereference, casts, and the registered memory calls
`std::malloc`, `std::realloc`, and `std::free`.

Pointer assignments and call arguments are checked for compatible pointer kinds.
Struct-pointer compatibility includes the struct type identity. A cast may make
an explicitly supported conversion valid; it does not make arbitrary memory
access safe or portable.

Nullability, lifetime, alignment, and allocation failure behavior follow the
runtime helpers and target ABI. CBL-C does not inherit the complete C pointer
model or C's permission to perform arbitrary unchecked casts.

### 6.7 Const and mutability

`const` data is initialized once according to its declaration form. Writes to a
const item, const string, or protected member are semantic errors. This applies
to direct assignment and supported indirect/member access paths.

### 6.8 Built-in operations are intrinsics, not copied method bodies

Built-in `string` operations and other compiler-known methods must be represented
as intrinsic operations in the semantic/lowering layer. Each intrinsic maps to
one shared runtime/target helper contract, for example:

```text
string.append       → CBLC string-append helper
string.length       → CBLC string-length helper
string.starts_with  → CBLC string-prefix helper
```

The mapping is resolved once during semantic analysis. The CBL-C-to-COBOL
backend emits a call to the generated standard-library subprogram or one shared
generated paragraph. The COBOL backend must not reproduce the complete
append/length/prefix algorithm separately at every source call site.

The source of truth for the currently registered string intrinsic names,
argument counts, mutability, and COBOL target associations is
`src/transpiler/transpiler_cblc_intrinsics.cpp`. A registry entry is not a
claim that a COBOL program exists: the COBOL target association remains unset
until its calling convention and generated artifact are implemented and tested.

If an operation needs target-specific preparation—such as obtaining a dynamic
string buffer view—that preparation belongs in a reusable lowering helper or a
single target helper contract. It must not become an independent copy of the
operation's semantics in each caller.

## 7. Declarations and scope

Global data is visible according to translation-unit and import rules. Local
data belongs to the function/block scope where it is declared. A nested block
may shadow an outer binding only where the semantic rules permit it; ambiguous
or prohibited shadowing is diagnosed.

Local storage is scoped to the source block. In the generated target, backing
storage may be allocated in a broader working area to satisfy COBOL layout, but
the source binding must not remain semantically visible after its block ends.

Declarations with initializers are initialized according to their type:

- scalar literals are converted under the normal range/type rules;
- character buffers receive fixed-buffer contents under capacity checks;
- strings run their constructor/initializer behavior;
- records/classes initialize their supported members and lifecycle state; and
- pointers receive only supported pointer/null/allocator results.

Uninitialized or potentially uninitialized reads are diagnosed when the
semantic analysis can establish them. The compiler must not silently invent a
source-level value to hide a required initialization.

## 8. Functions, methods, and calls

Functions and methods may be `void` or return a supported value. Non-void
functions must provide a valid return on every semantically reachable path.
`return;` is valid only for `void`; `return expression;` is required for a
value-returning function.

Calls require an exact supported argument count and compatible argument types.
The result of a void function cannot be assigned. Methods require a receiver of
the owning type and are subject to member visibility.

At the source level, a value-returning call behaves as an expression. In the
COBOL target, the compiler implements this with an explicit trailing result
slot rather than relying on COBOL `RETURNING`. All logical arguments precede
the result slot; status slots used by runtime helpers follow their documented
ABI. See [`abi_spec.md`](abi_spec.md).

Argument evaluation and receiver evaluation must be sequenced before a target
call is emitted. If the language does not define an order for two side effects,
the compiler must reject the ambiguous form or require it to be split into
separate statements. A backend must not choose an order accidentally because
of its target language's calling convention.

## 9. Expressions

The supported operator precedence, from lowest to highest, is:

1. `||`
2. `&&`
3. `==`, `!=`
4. `<`, `<=`, `>`, `>=`
5. `+`, `-`
6. `*`, `/`, `%`
7. unary `!`, unary `+`, unary `-`, and `ABS`
8. primary expressions, calls, member access, indexing, and parentheses

Expressions may contain literals, identifiers, supported calls, member access,
length access, pointer operations, and parenthesized subexpressions.

Numeric operands are widened or converted according to the compiler's type
compatibility policy. Mixed integral/floating operations must not rely on
implementation-specific C promotions. String equality and comparison use the
runtime string/buffer semantics, not pointer identity.

The language does not define unspecified evaluation order as a mechanism for
program behavior. Short-circuit behavior for `&&` and `||` must be specified by
the condition feature before code relies on side-effect suppression; until then,
implementations must not claim target-specific short-circuit behavior as a CBL-C
guarantee. Code with conflicting side effects in one expression must be
rejected or rewritten into sequenced statements.

## 10. Statements and control flow

The implemented statement families are:

- assignment, including supported member/index/pointer targets;
- increment and decrement where accepted by the parser;
- expression/function/method calls;
- `if`/`else`;
- `while`;
- supported `perform` calls;
- `display`;
- `return`; and
- supported file `open`, `close`, `read`, and `write` forms.

Blocks use braces. Empty statements are accepted for parser recovery where the
frontend supports them. Conditions must be convertible to the language's
boolean condition model; arbitrary target-language truthiness is not a promise.

`if` selects exactly one branch. `while` reevaluates its condition before each
iteration. `perform` is a resolved named operation, not an escape hatch for
arbitrary COBOL paragraph ranges. The compiler preserves structured branch and
loop behavior in the supported CBL-C-to-COBOL backend.

The language does not currently include general `for`, `switch`, `goto`,
exceptions, or user-defined control-flow operators.

## 11. File and record I/O

The forward compiler supports line-sequential file declarations and the basic
open/read/write/close statement contract:

```cblc
file input input "input.txt";
file output output "output.txt";
open(input, "r");
read(input, record_value);
write(output, record_value);
close(input);
```

The declaration may use an explicit role and name (`file input input ...`) or
the compact form used by samples (`file input ...`). The first open mode binds
an unqualified declaration as input (`"r"`) or output (`"w"`/`"a"`). Records
are currently fixed-length `char` arrays; the first read/write establishes the
COBOL FD record length. A generated read includes an `AT END` action that sets
the shared EOF flag.

The forward compiler supports the restricted file-copy loop form
`while (read(file, record))`. It lowers to a COBOL `PERFORM UNTIL` loop whose
read operation sets the shared EOF flag at `AT END`. General boolean loop
conditions and other loop forms remain governed by the broader control-flow
rules in this standard.

Indexed/relative organization, report-writer features, advanced COBOL file
clauses, and arbitrary file-control layouts are outside this version. The
compiler must not imply that reverse recovery of a file operation guarantees
forward emission of every recovered form.

## 12. Standard library and runtime

Qualified standard-library calls use the registered `std::` surface. The
current catalog includes string, conversion, character classification,
rounding, date, trigonometric, logarithmic, exponential, power, min/max, and
memory operations documented in [`runtime_api_reference.md`](runtime_api_reference.md).

The generated COBOL helpers use by-reference arguments and explicit result and
status slots. Status values have shared meanings: success, invalid argument,
range error, and domain error, with helper-specific conditions documented in
the runtime reference.

The runtime owns representation-specific behavior such as string buffers,
records, files, CSV, encoding, collation, sorting, and allocation. Language
features must call the shared helper contract rather than duplicating target
runtime logic at individual call sites.

## 13. Diagnostics and rejection behavior

Diagnostics are categorized conceptually as lexical, syntax, semantic, module,
lowering, and backend errors. The parser may recover from independent syntax
errors to report more than one issue, but the compilation result remains
failure if any error was recorded.

The compiler reports, where applicable:

- unknown or duplicate names;
- invalid scope or private-member access;
- wrong argument count or incompatible argument type;
- invalid return expression or missing non-void return;
- writes to const/immutable data;
- unsafe or unsupported conversions;
- string and buffer truncation;
- overflow, range, and domain failures;
- invalid pointer compatibility or unsupported pointer operations;
- unresolved imports and inaccessible external symbols; and
- unsupported source constructs.

Warnings do not authorize behavior that the standard leaves undefined or
unsupported. A backend must not silently lower a construct it cannot represent
faithfully.

Diagnostic codes and severity categories are part of the compiler contract;
human-readable wording and formatting are not stable API unless explicitly
marked so. A diagnostic change must state whether it changes only wording, the
code, the severity, or the point at which compilation fails. Tests should assert
codes and relevant source spans rather than entire prose messages wherever
possible.

Each diagnostic should identify the phase that owns it and, when applicable,
include the feature ID, source span, primary symbol, and actionable related
span. Recovery diagnostics must not allow emission of an artifact that semantic
analysis has declared invalid.

For template diagnostics, the primary diagnostic remains backward-compatible;
structured definition/use context is carried separately from human-readable
wording. This allows tooling to add related spans, instantiation traces, and
machine-readable argument displays later without making message text an ABI.

## 14. Target behavior

### 14.1 CBL-C to COBOL

The COBOL backend emits the supported source model into the repository's target
dialect profile. Scalars, buffers, records, classes, helper state, functions,
methods, calls, control flow, and lifecycle operations use shared lowering
rules. Generated source must satisfy the ABI and dialect requirements in
[`abi_spec.md`](abi_spec.md) and [`cobol_dialect_requirements.md`](cobol_dialect_requirements.md).

COBOL representation details such as `PIC`, `COMP-5`, `COMP-2`, group layout,
working-storage naming, linkage parameters, and result slots are target
contracts, not alternate CBL-C types.

Method and intrinsic implementations must be emitted as reusable callable
blocks. Repeated source calls may repeat `CALL`/`PERFORM` statements and
argument/result plumbing, but must never repeat the implementation body. The
current COBOL backend emits receiver-specialized reusable paragraphs for
user methods returning `void`, `int`, or a supported struct, including methods
with parameters. Parameter storage and non-void result slots are explicit
working-storage ABI elements. Constructors/destructors, pointer receivers, and
other return kinds still require shared lifecycle/receiver ABI work in the
remaining gaps; supported constructor and destructor bodies are already emitted
as receiver-specialized paragraphs. Built-in string operations with parameters
use the same model: each receiver has reusable argument groups, result/status
slots, and one paragraph per registered operation. This rule is especially
important for string capacity checks, allocation, copying, comparison, and
lifecycle cleanup.

### 14.2 Generated artifacts and dependencies

Generated files are build artifacts, not accidental side effects of individual
source functions. The compiler must distinguish these artifact classes:

| Artifact | Purpose | Current repository behavior |
| --- | --- | --- |
| translated module | User program/module output | Written to the matching `--output` path. |
| target runtime helper | Shared COBOL implementation support | COBOL standard-library programs and shared generated paragraphs provide the target support; no C runtime artifact is emitted. |
| standard-library subprogram | External callable helper with a stable ABI | `standard-library` mode currently emits every registered helper as its own `.cob`. |
| metadata/manifest | Dependency and build description | Both standard-library and normal translation modes emit `cblc.manifest.json`; COBOL output records the transitive standard-library closure referenced by generated targets. Target entries record direct source hashes and imported module source hashes in `module_dependencies`. The manifest also records `template_contract: CBLC-TEMPLATE-TYPE-SUBSTITUTION@6`; consumers must reject or invalidate artifacts from an unknown template contract. |

The current `standard-library` command is therefore a packaging operation:

```text
standard-library
    └── CBLC-*.cob
```

It intentionally validates and writes each catalog entry independently. Normal
translation detects referenced standard-library program identifiers, emits the
required `.cob` helper programs and their generated-program dependencies beside
the targets, and records them in the manifest. The manifests use stable FNV-1a content hashes; the
standard-library manifest describes the complete catalog package, while normal
translation describes the generated target plus its required helper closure.

The scalable model for future work is dependency-closed packaging. During
semantic analysis, every intrinsic, standard-library call, runtime helper, and
generated method records a stable artifact identifier. The build then computes
the transitive dependency closure and emits:

```text
build/<target>/<module>.cob
build/<target>/runtime/<required-helper>.cob
build/<target>/cblc.manifest.json
```

Each generated artifact includes a `dependencies` array of stable artifact IDs.
The manifest should list source modules, generated artifacts, dependencies,
target language, ABI version, compiler version, and hashes. It allows a build
system to compile only required files, prevents duplicate helper definitions,
and makes incremental builds reproducible. A `--runtime` or equivalent policy
may later choose between `embedded`, `generated`, and `preinstalled` runtime
modes, but the source-language semantics and helper ABI must remain identical.

This is the same general pattern used by mature transpilers: keep compiler
generated support code in a runtime/library layer, package it as linkable
artifacts, and use dependency metadata to decide what is needed. Some systems
embed a small runtime into each output file; that is simpler for distribution
but is unsuitable here once multiple COBOL modules share helpers because it can
create duplicate program names and duplicated state.

### 14.3 Reverse COBOL to CBL-C

The reverse pipeline reconstructs only the documented recoverable subset. It may
normalize names, comments, declarations, values, groups, copy directives,
paragraph structure, and supported control flow. Reverse output is not proof
that the original COBOL program belonged to the complete forward CBL-C language.

Unsupported or partial reverse areas include `ALTER`, `ENTRY`, `RENAMES`, broad
`INSPECT` forms, advanced packed decimal cases, complex table features, and
other items listed in the COBOL dialect requirements.

## 15. Unsupported language surface

The following are not part of this version's CBL-C standard:

- general C/C++ compatibility;
- non-type template parameters, parameter packs, defaults, constraints,
  specialization, and template-template parameters;
- inheritance and virtual dispatch;
- operator overloading;
- arbitrary pointer casts and unchecked memory reinterpretation;
- general `for`, `switch`, `goto`, and arbitrary paragraph control flow;
- arbitrary COBOL syntax embedded in CBL-C;
- complete indexed/relative/report-writer file support; and
- unimplemented numeric pictures, packed-decimal, table, screen, and report
  features.

An unsupported feature may be added only by updating this standard, the
semantic rules, both relevant emitters, diagnostics, examples, and tests in the
same change. If the feature changes syntax, its exact production must be added
to the syntax section and covered by lexer/parser tests.

## 16. Extension framework

The language is extended by capability, not by ad hoc parser acceptance. Every
new capability receives a stable feature identifier in the form `CBLC-<area>-<name>`
and progresses through these states:

| State | Meaning |
| --- | --- |
| **proposed** | Design is being discussed; programs must not depend on it. |
| **specified** | Syntax and semantics are written here, but implementation is incomplete. |
| **implemented** | Parser, semantic checks, lowering, runtime behavior, and tests agree. |
| **partial** | Only the explicitly listed subset is supported. |
| **deprecated** | Accepted for compatibility but should not be used in new code. |
| **removed** | Rejected by the current compiler; migration guidance is required. |

The initial feature registry is:

| Feature ID | Capability | Status | Primary sections |
| --- | --- | --- | --- |
| `CBLC-MOD-IMPORT` | Multi-file imports and public signatures | current/partial | 4, 14.3 |
| `CBLC-TYPE-STRING` | Fixed-capacity built-in string values | current/partial | 6.3, 6.8, 12 |
| `CBLC-TYPE-CLASS` | Classes, methods, constructors, and lifecycle | current/partial | 6.5, 8, 14 |
| `CBLC-TEMPLATE-TYPE-SUBSTITUTION` | Bounded type templates and monomorphization | partial | 6.5.2, 8, 14 |
| `CBLC-MEM-POINTER` | Supported pointer kinds and allocator calls | current/partial | 6.6, 12 |
| `CBLC-IO-FILE` | File declarations and sequential I/O | partial | 11, 14 |
| `CBLC-ABI-COBOL` | COBOL linkage/result/status conventions | current | 8, 12, 14.1 |
| `CBLC-ARTIFACT-RUNTIME` | COBOL standard-library packaging and dependency manifests | current/partial | 14.3 |

`current/partial` means the named capability exists, but one or more
interactions, target paths, or artifact contracts remain incomplete. A registry
entry must never be promoted to `current` solely because a parser or one backend
accepts an example.

Each feature entry must define five layers:

1. **Surface:** exact tokens, grammar, declarations, and examples.
2. **Meaning:** types, scope, evaluation order, mutation, errors, and lifetime.
3. **Compiler contract:** symbol registration, semantic validation, diagnostics,
   lowering, and generated source behavior.
4. **Runtime contract:** helper calls, storage, status codes, ABI, and failure
   behavior where applicable.
5. **Evidence:** positive/negative tests, backend tests, round-trip tests,
   documentation tests, and a sample when the feature is user-facing.

Features must be designed so that adding a later feature does not silently
change the meaning of an existing valid program. If interaction with an
existing feature is not specified, the new feature is incomplete and must not
be promoted to **implemented**.

The standard is organized so future additions can be made as new subsections
without moving the rules for existing features. New syntax must be added to the
lexical and syntax sections, new meaning to the type/statement/expression
sections, and target-specific details to the target and ABI references. The
feature identifier should be mentioned in all corresponding tests and tracker
entries.

The standard itself is intentionally the only normative language document.
Appendices and workflow guides may explain implementation details, but they may
not introduce a competing syntax or semantic rule. If an appendix needs a new
rule, the rule must first be added here and the appendix must link back to it.

For tooling and documentation checks, the repository paths of the subordinate
references are explicitly:

```text
docs/runtime_api_reference.md
docs/abi_spec.md
docs/cblc_sample_inventory.md
docs/getting_started.md
docs/onboarding_checklist.md
docs/contributing.md
docs/cli_usage_examples.md
docs/ci_pipeline.md
docs/cobol_dialect_requirements.md
```

## 17. Conformance evidence and change procedure

Every language feature must have all of the following before it is considered
implemented:

1. an exact source-syntax entry in this standard;
2. explicit accepted and rejected cases;
3. a semantic rule and diagnostic behavior;
4. a defined COBOL lowering;
5. a defined C lowering or an explicit reason it is unavailable;
6. runtime/ABI documentation if representation or calls are involved;
7. at least one positive and one negative test;
8. a representative sample when the feature is user-facing; and
9. synchronized formatter, source-map, reverse-pipeline, and IDE behavior when
   those components claim to support the construct.

The authoritative test families are the lexer, parser, semantic, codegen,
compiler, round-trip, runtime, CLI, and documentation tests under `tests/`.
Passing a narrow test does not establish conformance for an entire feature
family; the relevant syntax, semantics, lowering, and runtime behavior must be
covered.

## 18. Versioning and compatibility

This standard is versioned with the repository. A change is backward-compatible
only if existing valid programs retain their meaning and previously rejected
programs do not acquire an incompatible interpretation without an explicit
version decision.

Compatibility is tracked independently across these dimensions:

| Dimension | Breaking change example |
| --- | --- |
| source | Previously valid syntax is rejected or changes parse structure. |
| semantic | The same valid source produces a different result or error behavior. |
| diagnostic | A stable diagnostic code/severity or source-span contract changes. |
| ABI | Parameter order, result/status slots, storage, or calling convention changes. |
| artifact | Generated filenames, program IDs, dependency names, or manifest schema changes. |
| tooling | Formatter, source-map, reverse, or editor behavior changes incompatibly. |

Every release/change that affects one dimension must say whether the other
dimensions remain compatible. New syntax should normally be additive and
reserved before implementation. Removed or changed features require a
migration note and, where practical, a compiler warning period.

Changing a generated target layout, calling convention, string representation,
diagnostic category, or accepted syntax is a language/compiler compatibility
change and must be called out in the changelog and ABI documentation.

## 19. Current conformance gaps

This section is intentionally explicit: these are known areas where the
repository has implementation or documentation work remaining. They must not
be described as fully implemented until the evidence requirements in section 17
are met.

### 19.1 Remaining implementation TODOs

The following work items are the next release-gated tasks for completing the
remaining intrinsic and runtime artifact contracts:

- [ ] **Parameterized COBOL intrinsic ABI (`CBLC-TODO-INTRINSIC-ABI`).** The
  reusable receiver-specialized ABI is implemented for the current one-argument
  string registry (`append`, `equals`, `starts_with`, `ends_with`, `compare`,
  and `contains`): argument groups, result/status slots, literal/variable
  argument moves, collision-safe paragraph names, dynamic growth, and fixed
  capacity handling are emitted once per operation. Complete the release gate
  by adding executable COBOL coverage for every operation, aliasing and repeated
  calls, truncation/status behavior, and future multi-argument signatures.
  Update the intrinsic registry and this ABI section together whenever an
  operation is added.

| Area | Current status | Required hardening |
| --- | --- | --- |
| exact grammar | **partial** | Keep accepted syntax examples and parser tests synchronized with this document. |
| method reuse | **partial** | COBOL supports receiver-specialized paragraphs for supported methods and custom constructors/destructors. Extend parameterized destructor ABI and other result kinds. |
| built-in string intrinsics | **partial** | The operation registry, canonical parser operands, registry-driven arity validation, and reusable COBOL paragraphs for zero- and one-argument string operations are centralized; executable COBOL validation and broader ABI coverage remain release gates. |
| runtime dependency closure | **partial** | COBOL output emits the transitive standard-library closure; broader artifact-level dependency validation remains. |
| forward file-control generation | **partial** | The sequential declaration/open/read/write/close subset and restricted read loop are defined; add indexed/relative organizations, report-writer clauses, and broader file-control validation incrementally. |
| reverse translation | **partial** | Keep reverse-recoverable syntax separate from forward language conformance. |
| feature registry | **current framework** | Assign stable `CBLC-*` IDs and status records to every new capability. |

The gap table is a release gate, not a second specification. Closing a row
requires updating the relevant normative sections, implementation, and tests;
removing a row without evidence is not permitted.
