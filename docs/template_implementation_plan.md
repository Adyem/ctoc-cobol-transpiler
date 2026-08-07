# Basic CBL-C Templates: Implementation Plan

**Status:** the basic member, exact-deduction, fixed-array, and ownership-safe
mutable-string and borrowed-pointer phases are implemented and tested. Owning
pointer-bearing struct arrays remain an explicit rejected ABI tier; richer
layout migration and advanced type forms remain future work. The proven subset
is specified normatively in the authoritative language standard.
**Target:** CBL-C to COBOL forward compilation
**Scope:** compile-time type substitution (monomorphization)
**Initial non-goals:** polymorphism, inheritance, virtual dispatch, template
specialization, non-type parameters, parameter packs, and general C++
compatibility

This document describes a small, C++-like template facility for CBL-C. It is
an implementation plan, not a language specification. The accepted syntax and
observable semantics must be copied into `docs/cblc_language_standard.md` only
after the implementation and tests prove them.

## 1. Intended result

The first template implementation should allow a programmer to define a type or
function once, select concrete types at compile time, and generate ordinary
non-template CBL-C/COBOL behavior for each concrete use.

Illustrative proposed syntax:

```cblc
template <typename T>
struct box {
    T value;
};

template <typename T>
T identity(T value) {
    return value;
}

void main() {
    box<int> item;
    int result;
    result = identity<int>(7);
}
```

The accepted declaration syntax and semantic contract are now recorded in
`docs/cblc_language_standard.md`; this document tracks implementation design
and remaining work:

1. `T` is a compile-time type parameter, not a runtime value.
2. `box<int>` is distinct from `box<char>`.
3. `identity<int>` is a concrete function generated from the template body.
4. Each concrete instantiation is type-checked after substitution.
5. The COBOL emitter sees only concrete types and functions.

The first useful target is type substitution, not a general C++ template
language. The framework should make these later additions possible without a
second mechanism:

- limited type deduction for function calls;
- multiple type parameters;
- pointer and string type arguments;
- default type arguments;
- constrained type parameters;
- explicit specialization; and
- separate template and instantiation modules.

## 2. Current architecture and pressure points

The current forward compiler already has the main pieces needed for
monomorphization, but several representations assume that every declaration
has one concrete type.

### Existing extension points

- `t_cblc_translation_unit` owns parsed data items, struct/class types,
  functions, imports, files, and generated metadata.
- `t_cblc_struct_type` owns fields, methods, constructors, and destructor
  statements.
- `t_cblc_function`, `t_cblc_method`, and `t_cblc_parameter` carry parameter
  and return type names plus parsed statements.
- `t_cblc_data_item` and `t_cblc_struct_field` carry storage kind, declared type
  name, struct type name, length, and array count.
- The context layer exports concrete function/type signatures across modules.
- The COBOL generator consumes concrete translation-unit data and emits `PIC`,
  group, paragraph, and call forms.

### Constraints to resolve

1. A type is generally a name plus `t_cblc_data_kind`; `T` cannot safely be
   represented as an ordinary unresolved struct name.
2. Source names and COBOL names are stored together; instantiation must not
   overwrite a template definition.
3. Function and method statements are stored in arrays; cloning must substitute
   type-dependent references without mutating the definition.
4. Context exports assume one signature per source symbol; concrete instances
   need deterministic identities distinct from the template declaration.
5. The hand-written CBL-C parser writes directly into the translation unit;
   definitions need a boundary before ordinary semantic resolution.
6. Diagnostics and source maps need both the template definition location and
   the concrete argument/use location.

These concerns belong in a shared semantic layer, not in the COBOL emitter.

## 3. First-version language boundary

### 3.1 Supported in the basic version

Support the following first:

- `template <typename T>` through a bounded list of type parameters (currently
  at most four);
- templated `struct` declarations;
- templated `class` declarations as storage/method containers, without
  inheritance or virtual behavior;
- templated free functions;
- concrete arguments resolving to built-in or concrete user-defined types;
- explicit applications such as `box<int>` and `identity<int>(value)`;
- substitution in fields, parameters, returns, locals, calls, and member access;
- repeated-use caching;
- deterministic generated source and COBOL names; and
- independent semantic validation of each concrete instance.

The first release should prefer explicit arguments whenever deduction would be
ambiguous. It must not silently guess.

### 3.2 Explicitly rejected initially

Reject these with dedicated diagnostics rather than accepting a fragment and
failing later:

- more than four type parameters in the current bounded basic implementation;
- non-type parameters such as `template <int N>`;
- parameter packs and ellipses;
- template-template parameters;
- partial or full specialization;
- explicit instantiation declarations as separate syntax;
- deduction through conversions or overload candidates;
- dependent base classes, inheritance, virtual methods, and polymorphism;
- recursive instantiation beyond the configured depth; and
- arbitrary textual macros presented as templates.

## 4. Internal model

The long-term design should replace string-only type decisions incrementally.
The first implementation can retain the current fields as resolved compatibility
views while adding a structural type-reference layer.

### 4.1 Canonical type references

Introduce an internal reference with no target-specific names:

```text
t_cblc_type_ref
    kind: builtin | named | template_parameter | applied
    builtin_kind
    named_type: stable source/type ID
    parameter_index: index for T
    template_id: stable template definition ID for applied types
    arguments: ordered type-reference list
    pointer_depth: initially zero or phase-enabled
    array_count
```

The reference must be structural. Do not represent `box<int>` only as text;
the compiler needs the template name, argument list, pointer/array shape, and
canonical identity for equality, caching, diagnostics, and name mangling.

After resolution, populate `declared_type_name`, `struct_type_name`, `kind`,
length, and array fields from the canonical reference so existing emitters can
continue to operate while the migration proceeds.

Current status: canonical references are deduplicated structurally within a
translation unit, and recursive delimiter-aware parsing now supports nested
one-parameter applications such as `box<box<int>>`. Instantiation records retain
the canonical applied-reference ID, so nested cache keys can refer to resolved
structure rather than display-name text. Basic pointer and fixed-array shapes
are now retained in canonical references and substituted fields/signatures; the
compatibility views remain the lowering interface. Struct and free-function
applications now accept ordered multi-parameter type arguments within the
bounded limit.

### 4.2 Definitions and parameters

Add records equivalent to:

```text
t_cblc_template_parameter
    source_name
    kind: type
    declaration_span

t_cblc_template_definition
    stable_id
    source_name
    declaration_kind: struct | class | function
    parameters[]
    parameter_count
    immutable parsed definition/body
    definition_span
    owning_module
```

The parsed definition must be immutable after collection. Instantiating with
`int` must clone or derive a concrete entity; it must never rewrite the `T`
stored in the generic definition.

### 4.3 Instantiation keys and cache

Use a canonical key:

```text
t_cblc_instantiation_key
    template_stable_id
    canonical_argument_refs[]
    argument_count
```

Cache entries should carry:

```text
t_cblc_instantiation
    key
    state: unseen | resolving | complete | failed
    concrete_type_or_function_id
    generated_source_name
    definition_span
    first_use_span
```

Compare keys structurally, not by display spelling. The `resolving` state is
required to diagnose recursive instantiation rather than exhausting the stack.

### 4.4 Registry placement

Template definitions and instantiations should be separate registries on the
translation unit and, later, on context metadata:

- definitions describe reusable source entities;
- concrete `struct_types` and `functions` describe entities available to the
  emitter and call resolver; and
- an instantiation cache maps a definition plus canonical arguments to one
  concrete entity.

This prevents the emitter from treating a template definition as a runnable
COBOL paragraph or emitting it once per use site.

## 5. Parsing strategy

### 5.1 Preserve structure before substitution

Do not implement templates with textual replacement. It cannot safely handle
comments, strings, nested delimiters, shadowed names, member access, or source
locations.

The parser should:

1. recognize `template` and its parameter list;
2. parse the following struct/class/function into an immutable definition;
3. record every type-bearing position as a type reference or deferred reference;
4. keep the definition out of concrete emission lists; and
5. parse each applied type/call into an explicit instantiation request.

The existing parser may be extended incrementally, but nested `<...>` must be
parsed using tokens and delimiter-aware lookahead, never a raw string search.

### 5.2 `<` and `>` ambiguity

The parser must distinguish:

```cblc
box<int> value;       // type application
a < b;                // comparison
identity<int>(value); // explicit template call
```

Use parser context and lookahead:

- after a known template name, parse `<...>` as type arguments;
- after an expression operand, retain comparison parsing;
- require a complete, valid argument list and valid following token; and
- restore the cursor or issue a precise error when an attempted application is
  malformed.

Template parameter scope must cover only the definition's type positions,
parameters, return, locals, and body. Parameters must not leak into modules or
unrelated functions.

## 6. Resolution and monomorphization pipeline

The forward pipeline should become:

```text
source
  -> lex/parse definitions and ordinary declarations
  -> collect module/template/type/function symbols
  -> parse explicit instantiation requests
  -> resolve arguments to canonical concrete type references
  -> look up or create a cache entry
  -> clone the definition into a concrete entity
  -> substitute type references and dependent names
  -> register concrete symbols and resolve calls/member access
  -> run ordinary semantic validation
  -> lower only concrete entities to COBOL
```

### 6.1 Argument resolution

For each argument:

1. resolve built-in names;
2. resolve visible struct/class names through normal import/visibility rules;
3. resolve an applied template recursively;
4. reject unresolved, private, incomplete, or disallowed types; and
5. canonicalize before cache lookup.

A template must not bypass module privacy merely because expansion happens at
compile time.

### 6.2 Structural substitution

Walk the cloned definition structurally:

- `T field` becomes the concrete field type;
- `T function(T value)` becomes a concrete signature;
- `box<T>` becomes an applied type with the substituted argument;
- pointer/array shape is retained while its element type is substituted;
- member access is resolved against the concrete receiver; and
- templated calls create further cache requests.

Any remaining template parameter after this walk is an error. The emitter must
never receive an unresolved template-parameter reference.

If a current statement stores only raw expression text, add a typed/deferred
expression representation before substituting it. Do not replace arbitrary
occurrences of `T` in raw text.

### 6.3 Cache behavior

The same key must produce one entity:

```cblc
box<int> first;
box<int> second;
```

Different keys must produce distinct entities even if readable names collide.
Cache lookup must occur before concrete symbol export and COBOL emission.

## 7. Naming and COBOL emission

### 7.1 Central deterministic mangling

Use one central mangler rather than concatenating source names in each emitter.
A readable proposal is:

```text
CBLC-TPL-BOX-I32
CBLC-TPL-IDENTITY-I32
```

The mangler must:

- normalize identifiers through the existing helper;
- assign fixed codes to built-in types;
- include stable module/type components for user types;
- encode nested arguments without ambiguous separators;
- obey COBOL identifier limits; and
- append a deterministic bounded hash when a readable name is too long.

The name map must be tested independently and be stable across repeated and
parallel generation.

Current status: struct and function instantiations now use one shared bounded
mangler. Readable names are preserved when they fit the target identifier
limit; oversized names use a deterministic FNV-1a-derived suffix so distinct
template/argument pairs remain distinguishable without truncation collisions.

### 7.2 Concrete-only backend

After substitution, the existing COBOL generator should handle a template
instance exactly like an ordinary concrete type or function:

- fields become normal group items;
- concrete types use the existing `PIC`/group mapping;
- functions use existing paragraphs, calls, and return slots; and
- no template runtime helper or dynamic dispatch is emitted.

The emitter should fail with an internal lowering diagnostic if an unresolved
template reference reaches it. Template behavior is compile-time only.

### 7.3 Parallel generation

Resolve templates in a deterministic single-threaded prepass initially. Pass an
immutable concrete snapshot to the existing parallel COBOL generator. Do not
allow worker threads to mutate a shared instantiation cache without an explicit
thread-safe design.

## 8. Modules, context, and ABI

For the first phase, require template definitions and explicit uses to be in
the same translation unit. This establishes the core without inventing an
artifact format prematurely.

Later, export template metadata containing:

- stable template ID and owning module;
- declaration kind and parameter list;
- visibility;
- dependent signature/field metadata; and
- a versioned body/IR reference sufficient for instantiation.

A concrete signature cannot recreate a missing template body. If an imported
body is unavailable, fail with a targeted diagnostic rather than silently
generating an incomplete entity.

Once instantiated, the result uses the existing COBOL ABI: parameter passing,
return slots, record layout, visibility, helper dependencies, and module
initialization remain unchanged. Template definitions are not ABI symbols;
only concrete exported instances can participate in cross-module linkage.

## 9. Diagnostics and resource limits

Reserve dedicated diagnostics for:

- malformed template parameter list;
- unsupported parameter kind;
- argument count mismatch;
- invalid or unknown type argument;
- unknown template;
- unapplied template where a concrete type is required;
- unresolved parameter after substitution;
- inaccessible definition or type argument;
- recursive instantiation;
- depth/instantiation-count limit exceeded;
- duplicate/conflicting definition;
- generated-name collision; and
- unavailable imported body.

Diagnostics should show the use site, template name, concrete arguments, and
definition location. Nested expansion needs a bounded instantiation backtrace.

Make these initial limits configuration fields, not scattered constants:

- maximum parameters: 1;
- maximum instantiation depth: 32;
- maximum concrete instances per unit: 1024; and
- maximum diagnostic backtrace: 16 frames.

Resource-limit failures must be deterministic.

Current status: the translation unit exposes configurable maximum instance and
depth fields, with defaults of 1024 concrete instances and depth 32. Both
limits are enforced by the instantiation paths with stable deterministic
diagnostics; the depth guard is exception-safe across early materialization
failures.

## 10. Tooling obligations

Templates affect more than the compiler:

- **Formatter:** preserve `template <typename T>`, normalize nested argument
  spacing, and distinguish `>` from comparisons.
- **LSP:** expose definitions, complete template names/arguments, and show the
  concrete type without replacing the source definition.
- **Source maps:** retain definition span, use span, and argument spans for each
  generated concrete field/function/paragraph.
- **Semantic dumps:** show template definitions, cache keys, and concrete
  instances separately.
- **Round trips:** generated COBOL must remain ordinary concrete COBOL; reverse
  translation must not invent template syntax unless that feature is separately
  specified.

## 11. Staged implementation plan

### Phase 0: scaffolding

- assign feature ID `CBLC-TEMPLATES-BASIC`;
- add canonical type-reference, parameter, key, cache, and limit structures;
- reserve diagnostics;
- add unit tests for structural equality, canonical keys, and name mangling; and
- keep accepted syntax unchanged.

Exit criterion: all existing tests pass with no behavior change.

### Phase 1: one-parameter templated structs

- parse `template <typename T>` followed by `struct`;
- store an immutable definition;
- allow `T` in fields;
- parse explicit applications such as `box<int>`;
- clone and resolve a concrete `t_cblc_struct_type`; and
- emit one valid COBOL group per cache key.

Exit criterion: `box<int>` and `box<char>` are distinct, while repeated
`box<int>` uses share one concrete definition.

Current status: the same implementation path also supports bounded ordered
multi-parameter struct applications, basic pointer/fixed-array arguments, and
retains the one-parameter compatibility fields for existing consumers.

### Phase 2: templated free functions

- parse dependent parameter/return types;
- parse explicit calls such as `identity<int>(value)`;
- clone statements and substitute structured type references;
- reuse existing call resolution and return-slot ABI; and
- validate arguments after substitution.

Exit criterion: generated COBOL contains only concrete paragraphs and calls.

Current status: explicit multi-parameter function applications now share the
canonical argument-reference cache and substitute dependent parameters and
returns independently. Pointer return kinds are supported by the shared shape
resolver. Integer fixed-array parameters now use an exact-shape, by-value-copy
ABI into callee `OCCURS` storage; capacity-bearing string arrays and
recursively supported concrete struct arrays use explicit ownership/shape
rules. Explicitly `borrowed` pointer fields use shallow aliasing; owning
pointer fields and other non-trivial ownership layouts remain rejected until
their lifecycle contract is defined. Deduction remains intentionally limited to
the existing exact one-parameter rules.

### Phase 3: members and locals

- support templated methods, constructors, and enclosing-type parameters;
- substitute local declarations and member accesses;
- run lifecycle injection after instantiation; and
- apply visibility checks to the concrete receiver/type.

Current status: implemented for one-parameter classes, including dependent
method parameters, concrete method storage, member calls, and COBOL emission.

Exit criterion: instantiated class behavior matches an ordinary concrete class.

### Phase 4: limited deduction

After explicit applications are stable, support only exact deduction:

1. inspect function parameter type references;
2. match already-resolved argument types;
3. infer `T` only from exact identity;
4. reject conflicting deductions; and
5. use the same canonical cache key as explicit applications.

Do not infer through conversions, overload sets, pointer covariance, or runtime
values in this phase.

Current status: implemented for exact integer, string-variable, and
concrete-struct argument deduction. Explicit and deduced calls share the same
canonical instantiation cache; string literals with conversion-sensitive
shapes, conversions, overloads, and other ambiguous argument forms remain
rejected.

### Phase 5: modules and artifacts

- export/import template metadata and bodies;
- define versioning and invalidation;
- validate visibility before instantiation;
- resolve imported dependencies deterministically; and
- extend parallel generation only after single-threaded resolution is stable.

Current status: public templated struct/class and free-function metadata now
cross the existing module context boundary. Dependent fields, parameters,
return metadata, pointer/fixed-array field shapes, and immutable function/type
bodies are copied into importing units, which instantiate them through the same
local cache and concrete COBOL lowering path. Generated manifests now carry a
versioned template contract marker and direct source hashes, and the
incremental cache invalidates entries when the compiler/template contract
changes. Target manifest entries also record imported module source hashes so
external build tooling can invalidate dependents when an imported template
changes. Richer ownership/source references remain outstanding.

### Phase 6: standardization and expansion

- move proven syntax/semantics into the authoritative standard;
- add additional type shapes and raise the parameter bound only with a revised
  artifact/diagnostic contract;
- add defaults or constraints only with explicit rules; and
- review specialization separately from polymorphism/inheritance.

## 12. Required code-area changes

### Header/model

Add canonical type references, template definitions/parameters/keys/cache,
limits, and diagnostic identifiers. Keep existing concrete fields as resolved
compatibility views during migration.

### Parser and storage helpers

Add token-aware template syntax, nested type-argument parsing, immutable
definition storage, explicit applications, scope handling, cloning, structural
substitution, recursion guards, and centralized mangling.

### Context/module integration

Separate template metadata from concrete exports; export body references only
when the artifact contract can provide them; and register only concrete
instantiations as linkable symbols.

### COBOL generator

Keep template-specific behavior out of ordinary emission wherever possible.
Consume only resolved concrete types and use the central concrete-name map.

### Tooling and tests

Update formatter, frontend analysis, completions, semantic dumps, source maps,
sample inventory, parser tests, semantic tests, generated-source tests, and
GnuCOBOL compile/execution tests.

## 13. Test matrix

### Unit and parser tests

Cover type-reference equality, cache hits/misses, scope/shadowing, malformed
delimiters, nested applications, comparison ambiguity, unsupported parameter
kinds, and deterministic name mangling.

### Semantic tests

Cover distinct `box<int>`/`box<char>` types, repeated-use deduplication,
invalid substituted fields, visibility, nested applications, call/return-slot
resolution, recursion, and resource limits.

### COBOL and integration tests

Verify valid bounded names, correct `PIC`/group layouts, no `T`/`typename` in
generated COBOL, one artifact per key, unchanged ABI, GnuCOBOL compilation and
execution, module collision handling, byte-identical parallel output,
formatter reparsing, and definition/use source maps.

## 14. Invariants and review gates

1. No unresolved template parameter reaches semantic validation or emission.
2. A cache key completely identifies a concrete instantiation.
3. Definitions are immutable after parsing.
4. Names are deterministic, collision-safe, and centrally generated.
5. Instantiation is compile-time only; no runtime polymorphism is introduced.
6. Existing concrete ABI and COBOL layout rules remain authoritative.
7. Diagnostics identify definition and concrete use context.
8. Limits are deterministic and configurable.
9. Parallel emission does not race on resolution state.
10. Unsupported advanced syntax is explicitly rejected.

Before a phase enters the authoritative standard, require parser, semantic,
generator, formatter, tooling, and GnuCOBOL execution coverage plus deterministic
repeated/parallel output and duplicate-artifact checks.

## 15. Future extension map

| Future feature | Reuse from the basic framework |
| --- | --- |
| Multiple type parameters | ordered parameter and argument arrays |
| Function deduction | canonical type matching and the same cache |
| Pointer/array arguments | structural type-reference shape |
| Default arguments | completion before key canonicalization |
| Constraints | validation hook before cloning |
| Specialization | candidate selection before cache lookup |
| Imported templates | context registry and body artifact references |
| Better diagnostics | instantiation stack and definition/use spans |
| Parallel expansion | immutable pre-resolved cache snapshots |
| IDE support | definition/use relationships and canonical type display |

Specialization, overload resolution, and polymorphism require separate design
reviews. They add ordering and behavioral rules that are not needed for basic
type substitution and should not be smuggled into the first implementation.

The current diagnostic slice records the template source name, canonical
ordered argument key, and definition/use offsets on the translation unit while
preserving stable error codes and messages. Future work can expose this
metadata through frontend related spans and add a bounded instantiation trace
without changing the primary diagnostic contract.

The first array ABI is intentionally narrow: `int[N]`, explicit
capacity-bearing `string(C)[N]`, and recursively trivial concrete `Type[N]`
arguments are accepted in dependent function parameters. The caller must
provide a named array with exactly matching shape; lowering copies each
integer element, string element length and active buffer, or struct field into
callee-owned `OCCURS N TIMES` storage. Array decay, implicit bound or capacity
conversion, indexed expressions, literals, and non-trivial struct layouts
remain rejected. This creates a deterministic ABI boundary that can later be
generalized to other element layouts without changing template cache identity.

### 10.1 String array ABI and trivially-copyable struct array ABI

String arrays now use a capacity-bearing element layout, and a bounded
trivially-copyable struct-array subset is enabled:

1. String array arguments use `string(C)[N]`. Capacity and count are included
   in the canonical reference and generated name. Caller and callee require
   identical capacity and count; lowering copies each `LEN` plus active `BUF`
   contents into callee-owned storage. A dynamic source does not transfer its
   `PTR` or `CAP`; ownership remains with the source. The ABI contract is
   versioned and covered by local and compiler-backed tests.
2. Trivial struct arrays use concrete struct identity and exact element count.
   Lowering emits an `OCCURS` group and copies each element group with explicit
   subscripts. The accepted layout contains scalar fields and recursively
   trivial nested structs, fixed character buffers, and `const string(C)` fields
   represented as fixed buffers. Mutable string fields use per-element deep
   copies with source-capacity preservation and explicit destination cleanup;
   explicitly `borrowed` pointer fields are copied shallowly without transferring
   pointee ownership, while owning pointer fields remain rejected.
3. Borrowed pointer fields are a bounded aliasing tier: construction initializes
   each pointer to NULL, array argument lowering copies pointer values, and
   destruction never frees the pointee. The caller must keep each pointee alive
   through callee use. Layout identity for imported structs is versioned as
   `CBLC-LAYOUT@1`; future work should add richer layout-version migration and
   an owning-pointer contract. Constructor-argument forwarding for struct arrays
   is implemented for the bounded element-wise ABI and covered by execution
   tests.
