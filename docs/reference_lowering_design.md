# CBL-C Reference Semantics and COBOL Lowering Design Sheet

**Status:** implementation design; the implemented subset is normative through
the authoritative language standard
**Primary target:** GnuCOBOL, with a portable COBOL ABI where practical
**Purpose:** introduce safe source-level references for classes, records,
strings, vectors, scalars, and function results without exposing pointer
semantics to ordinary CBL-C code.

## Current implementation status

The implemented subset covers compile-time aliases for local scalar, string,
and class/record references; local function reference parameters; safe
reference returns that lower through the existing typed pointer result slot;
and `vector<T>::at`, `front`, and `back` element reference results.
The parser accepts `T& name = target;` and `const T& name = target;`; bindings
are recorded separately from pointer kinds and direct aliases use the target's
COBOL storage. Local reference parameters currently use a centralized
copy-in/copy-out paragraph ABI, preserving mutation semantics while the native
`BY REFERENCE` linkage layout is completed. Const writes, uninitialized
references, incompatible targets, reference-to-reference bindings, literal
arguments, and returns of local/by-value parameter storage are rejected.

Native `BY REFERENCE` linkage groups, a dedicated hidden reference-result
slot, reference descriptors, imported reference signatures, and
invalidation-generation checks remain planned extensions. The current vector
element references use the existing typed pointer result slot and have no
generation-tracked descriptor. They are specified below and must not be
treated as implemented merely because the parser or metadata can represent
part of them.

## 1. Design goals

CBL-C references should provide C++-style aliasing for the basic cases:

```cpp
Widget& select_widget(Vector<Widget>& widgets, int index);
const string& name_of(const Widget& widget);

void rename(Widget& widget, const string& replacement)
{
    widget.name = replacement;
}
```

A reference is an alias to an existing object. It is not an owning pointer and
must not inherit all pointer operations merely because the COBOL lowering may
use an address internally.

The initial framework must provide:

- non-null references;
- mandatory initialization;
- no reference reseating after binding;
- mutable and `const` references;
- references as parameters and return values;
- references to globals, caller-owned objects, class members, strings, vector
  elements, and other objects with a provably sufficient lifetime;
- deterministic diagnostics for dangling, invalid, or unsupported bindings;
- one ABI shared by ordinary functions, methods, templates, and standard
  library helpers; and
- room for future slices, iterators, reference members, and checked handles.

The first version should not include rvalue references, reference arithmetic,
nullable references, ownership transfer, or implicit lifetime extension.

## 2. Semantic distinction from pointers

References and pointers must remain separate types in the frontend and
intermediate representation.

| Property | `T&` | `const T&` | `T*` |
|---|---:|---:|---:|
| Must bind during initialization | yes | yes | no |
| May be null | no | no | yes |
| May be reseated | no | no | yes |
| Supports pointer arithmetic | no | no | where supported |
| Implicit member access | yes | yes | no |
| Allows mutation of `T` | yes | no | depends on pointee type |
| Owns the object | no | no | no by default |
| Source syntax exposes an address | no | no | yes |

The compiler may represent a reference with a COBOL pointer or address-backed
view, but that is a lowering detail. It must not make null assignment,
allocation, deallocation, arithmetic, or reseating legal for a source
reference.

## 3. Source syntax for the basic version

Use familiar C++ forms:

```cpp
int value;
int& alias = value;
const int& read_only = value;

Widget& find_widget(Vector<Widget>& widgets, int index);
void inspect(const Widget& widget);
```

Initial restrictions:

1. Every local reference declaration requires an initializer.
2. The initializer must be an addressable lvalue with a compatible type.
3. `T&` cannot bind to a `const T` object.
4. `const T&` may bind to mutable or immutable lvalues.
5. Temporaries and literals cannot bind to references in the first version.
6. Assignment through a reference changes the referenced object; it never
   changes which object the reference denotes.
7. Taking the address of a reference, reference-to-reference types, and arrays
   of references are deferred.
8. Reference data members are deferred until constructor binding and object
   copy rules are specified.

References collapse during basic template substitution:

```cpp
template <typename T>
T& first(Vector<T>& values);
```

The instantiated signature retains reference qualification independently of
the substituted base type. A template argument that is itself a reference is
rejected initially rather than applying C++ reference-collapsing rules.

## 4. Lifetime and validity rules

The compiler should classify every possible referent by storage provenance:

```text
global/static
caller-owned parameter
receiver object (`this`)
member subobject
local automatic object
dynamic/runtime-owned object
vector element
string character or buffer view
temporary/result slot
unknown imported storage
```

Each reference binding records:

- base type and applied template identity;
- mutability;
- referent provenance;
- defining lexical scope;
- owner function or receiver, where applicable;
- minimum guaranteed lifetime region;
- whether the referent can be invalidated by container mutation; and
- ABI representation selected during lowering.

### 4.1 Safe return sources

A function returning `T&` or `const T&` may return only:

- a global/static object;
- an incoming reference parameter;
- a member of an incoming reference parameter;
- the receiver or a receiver member when the receiver outlives the call;
- an element or subobject reached through an incoming reference, subject to
  its invalidation contract; or
- explicitly runtime-owned storage whose lifetime contract exceeds the call.

It must reject references to:

- local automatic variables;
- by-value parameters and their members;
- temporary values;
- ordinary by-value function return slots;
- destructed objects; and
- storage whose provenance cannot be proven safe under the selected ABI.

This rule should be checked before COBOL generation. The backend must never
silently copy a value to make an invalid reference return appear to work.

### 4.2 Container invalidation

Vector and string references require explicit invalidation rules.

For `Vector<T>`:

- references to the vector object remain valid for its lifetime;
- references to elements are invalidated by reallocation, destruction, clear,
  assignment that replaces storage, and any operation documented to relocate
  elements;
- insertion or erasure may invalidate references at or after the modified
  position even without reallocation; and
- read-only operations do not invalidate element references.

For `string`:

- a reference to the string object remains valid for its lifetime;
- references or views into character storage are invalidated by destruction,
  assignment, clear, reserve/reallocation, or any mutating operation that may
  replace the buffer; and
- `const string&` does not freeze the original mutable object when another
  alias can still mutate it.

The first implementation should diagnose invalidation when it is visible in
the same function. Cross-function invalidation remains governed by the
documented contract until deeper borrow analysis exists.

## 5. Internal type model

Do not encode a reference by adding another pointer kind such as
`CBLC_DATA_KIND_STRUCT_POINTER`. Add reference qualification to the canonical
type model:

```cpp
typedef enum e_cblc_reference_kind
{
    CBLC_REFERENCE_NONE = 0,
    CBLC_REFERENCE_MUTABLE,
    CBLC_REFERENCE_CONST
} t_cblc_reference_kind;
```

Add `reference_kind` to `t_cblc_type_ref`, parameters, function/method return
metadata, resolved expressions, and bindings. Keep the base data kind as
`INT`, `STRING`, `STRUCT`, or the instantiated vector/class type.

A resolved expression should also carry an lvalue category:

```text
value                 not bindable in v1
mutable lvalue        bindable to T& and const T&
const lvalue          bindable only to const T&
reference result      preserves the returned alias
```

This avoids special cases for classes, strings, and vectors. They all use the
same reference qualifier and differ only in layout and invalidation policy.

## 6. Recommended COBOL representation

Use three lowering modes selected from context.

### 6.1 Compile-time alias mode

For a local reference whose target is statically known and never escapes, emit
no reference storage. Resolve every use directly to the target COBOL name.

```cpp
int& current = total;
current = 4;
```

Conceptual COBOL:

```cobol
MOVE 4 TO TOTAL
```

This is the safest and cheapest representation. It should be preferred for
simple locals, direct members, and references whose binding is known at
compile time.

### 6.2 Native parameter alias mode

For reference parameters, use COBOL's native `BY REFERENCE` calling convention
and a `LINKAGE SECTION` declaration with the exact referent layout.

Conceptual source:

```cpp
void increment(int& value);
```

Conceptual COBOL callee:

```cobol
LINKAGE SECTION.
01 VALUE-REF PIC S9(18) COMP-5.

PROCEDURE DIVISION USING BY REFERENCE VALUE-REF.
    ADD 1 TO VALUE-REF
    GOBACK.
```

For classes, strings, and vectors, the linkage item is the complete ABI group,
not merely its first field. This allows ordinary COBOL field access while the
caller retains ownership.

This mode should be the default for internal CBL-C calls because it maps most
closely to a true reference and avoids manual address manipulation.

### 6.3 Address-backed view mode

Use a COBOL pointer plus `BASED` storage only when the referent is chosen at
runtime, returned from a function, or selected from dynamic container storage.

Conceptual representation:

```cobol
WORKING-STORAGE SECTION.
01 CBLC-REF-PTR USAGE POINTER.
01 CBLC-REF-VIEW BASED.
   05 FIELD-A PIC S9(18) COMP-5.
   05 FIELD-B ...

SET CBLC-REF-PTR TO ADDRESS OF SOME-OBJECT
SET ADDRESS OF CBLC-REF-VIEW TO CBLC-REF-PTR
```

The pointer is never exposed as the source value. The compiler controls all
`SET ADDRESS OF` operations and emits normal field operations against the
typed view.

Not every COBOL compiler implements `BASED` and pointer association identically.
The target capability table must record support for:

- `USAGE POINTER`;
- `ADDRESS OF`;
- `SET ADDRESS OF based-item`;
- pointer values in `LINKAGE SECTION`; and
- pointer-sized return or hidden-result parameters.

GnuCOBOL is the initial required implementation. Other backends may reject
address-backed references while still supporting compile-time and parameter
aliases.

## 7. Reference-return ABI

Do not return a class or string reference by copying its value into the normal
return slot. A reference result must preserve object identity.

Use a hidden caller-provided reference-result slot in the portable internal
ABI:

```text
source:       Widget& select(Vector<Widget>& values, int index)

lowered ABI:  select(values BY REFERENCE,
                     index BY VALUE,
                     hidden-result-address BY REFERENCE)
```

The callee computes the address of the selected object and stores that address
in the hidden result slot. The caller associates a typed `BASED` view with the
returned address.

Conceptual COBOL callee:

```cobol
LINKAGE SECTION.
01 VALUES-REF ...
01 INDEX-VALUE PIC S9(18) COMP-5.
01 CBLC-RETURN-REF USAGE POINTER.

PROCEDURE DIVISION USING
    BY REFERENCE VALUES-REF
    BY VALUE INDEX-VALUE
    BY REFERENCE CBLC-RETURN-REF.

    SET CBLC-RETURN-REF TO ADDRESS OF SELECTED-ELEMENT
    GOBACK.
```

Conceptual caller:

```cobol
CALL "SELECT" USING
    BY REFERENCE VALUES
    BY VALUE INDEX-VALUE
    BY REFERENCE CBLC-CALL-RETURN-REF
END-CALL
SET ADDRESS OF SELECTED-WIDGET-VIEW TO CBLC-CALL-RETURN-REF
```

The hidden slot is preferable to depending on dialect-specific pointer
`RETURNING` behavior. A target may use a direct pointer return as an optimized
ABI only when both caller and callee share a capability-qualified ABI version.

Reference-return expressions must remain references through assignment:

```cpp
Widget& selected = select(widgets, index); // bind identity
selected = replacement;                    // assign into selected Widget
```

Assigning a reference result into a by-value `Widget` performs a copy. Binding
it to `Widget&` preserves identity.

## 8. Class and member references

A class reference uses the class's existing generated COBOL group layout.
Method dispatch receives the referenced object as the receiver linkage item.

```cpp
Widget& WidgetStore::current();
string& Widget::name();
```

Returning `this` or one of its members is valid only if the receiver arrives by
reference and the result cannot outlive that receiver. The return metadata
should record a provenance relation such as:

```text
return borrows parameter 0
return borrows receiver
return borrows member(receiver, "name")
```

This relation is more useful than a generic `borrowed` boolean. It enables
later interprocedural lifetime checking without changing the ABI.

Virtual dispatch, inheritance adjustments, and multiple base subobjects are
outside the initial reference design.

## 9. String references

Distinguish these concepts:

- `string&`: reference to the entire string object and its metadata;
- `const string&`: read-only access through this alias;
- `char&`: reference to one character, deferred until indexed dynamic buffer
  association is reliable;
- `string_view`: future non-owning `(address, length)` range, not equivalent to
  `const string&`.

A `string&` parameter passes the whole string ABI group by reference. Existing
capacity, length, and buffer preparation helpers remain the only code allowed
to manipulate dynamic buffer association. Reference lowering must call those
helpers instead of duplicating their COBOL fragments.

Returning `string&` is safe for a global string, an incoming string reference,
or a member of a live referenced object. Returning a local string reference is
always an error because its destructor runs when the function exits.

## 10. Vector references

The vector ABI should be a stable object header passed by reference:

```text
data address
element count
capacity
element layout/type fingerprint
optional mutation generation
```

`Vector<T>&` refers to this header. `T&` returned from `front`, `back`, or
`at` refers to element storage selected at runtime and therefore uses the
current typed address-result mode. Reference-returning `operator[]` remains a
later extension because the existing pointer-oriented indexing and `->` member
call surface must remain compatible.

Recommended element-address lowering:

```text
element-address = data-address + checked-index * element-stride
```

The arithmetic must live in one vector runtime/lowering helper. Individual
methods must not paste pointer arithmetic or COBOL address fragments into each
emitted function.

For debug or checked builds, store a mutation generation in the vector header
and optionally in a reference descriptor. Dereference can then diagnose a
reference invalidated by reallocation. Release builds may omit the check while
retaining the same source-level invalidation rules.

Bounds behavior is independent of reference behavior:

- `at(index)` checks bounds and returns `T&` on success;
- `operator[](index)` follows the language's eventual unchecked-index policy;
- an out-of-range operation must never manufacture a null reference.

## 11. ABI descriptors and metadata

Extend function and method signatures with:

```text
parameter.reference_kind
parameter.referent_type_id
return.reference_kind
return.referent_type_id
return.borrow_source
return.borrow_parameter_index
return.borrow_member_path
```

Imported/exported signatures must serialize these fields. Signature equality
must include reference qualification, mutability, concrete template arguments,
and layout fingerprint.

Add an ABI feature/version flag for reference returns. A module compiled
without the hidden-result reference ABI must not be linked as though it
supports reference-returning functions.

Suggested internal binding representation:

```cpp
typedef struct s_cblc_reference_binding
{
    size_t type_ref_id;
    t_cblc_reference_kind kind;
    t_cblc_reference_provenance provenance;
    size_t scope_id;
    size_t source_binding_index;
    size_t borrow_parameter_index;
    char member_path[TRANSPILE_STATEMENT_TEXT_MAX];
    int may_be_invalidated;
    int escapes_function;
} t_cblc_reference_binding;
```

Do not store raw pointers into growable compiler arrays. Store stable IDs or
indices and resolve them after capacity changes.

## 12. Semantic checks and diagnostics

Add stable diagnostics for at least:

- reference declaration without initializer;
- binding mutable reference to const object;
- binding reference to a non-lvalue;
- incompatible referent type or template application;
- returning reference to local, by-value parameter, or temporary;
- use after visible container invalidation;
- assignment that attempts to reseat a reference;
- unsupported reference member/array/nesting form;
- imported signature with incompatible reference ABI; and
- backend lacking the required address-backed reference capability.

Diagnostics should identify both the reference use and the declaration or
mutation that made it invalid where source locations are available.

An internal lowering failure must not be reported as a generic syntax error.
Reference syntax, semantic rejection, ABI rejection, and backend capability
rejection are distinct stages.

## 13. Lowering architecture

Reference handling should be centralized behind operations such as:

```text
bind_reference(target, source_lvalue)
load_through_reference(reference)
store_through_reference(reference, value)
address_of_referent(reference)
pass_reference_argument(reference)
return_reference(reference)
associate_typed_view(type, address)
invalidate_container_borrows(container, operation)
```

Each operation has one semantic implementation and one target-specific
lowering path. String, vector, and class functions request these operations;
they do not print repeated COBOL snippets themselves.

The IR should distinguish:

```text
BIND_REF
LOAD_REF
STORE_REF
PASS_REF
RETURN_REF
ASSOCIATE_REF_VIEW
INVALIDATE_BORROWS
```

This makes it possible to add another backend or a safer runtime descriptor
without rewriting parsing and type checking.

## 14. Implementation phases

### Phase 1: semantic foundation

- Add reference qualifiers to canonical type references and signatures.
- Parse `T&` and `const T&` in declarations, parameters, and returns.
- Add lvalue classification and compatibility checks.
- Add provenance and borrow-source metadata.
- Reject temporary binding, reseating, dangling returns, reference members,
  and reference arrays.

### Phase 2: compile-time and parameter aliases

- Lower statically known local references as direct aliases.
- Lower reference parameters through `BY REFERENCE` and `LINKAGE SECTION`.
- Support scalars, fixed buffers, records/classes, and whole strings.
- Add same-module and multi-module ABI tests.

### Phase 3: reference returns

- Add the hidden reference-result slot to call signatures.
- Add typed `BASED` views for returned class/record/string references.
- Support returning globals, incoming references, receiver members, and safe
  subobjects.
- Verify identity preservation and mutation through returned references.

### Phase 4: vectors

- Finalize the vector object ABI and central element-address helper.
- Implement `Vector<T>&`, `const Vector<T>&`, `operator[]` element `T&`
  results, and generation-tracked element descriptors.
- Define mutation invalidation tables for each vector method.
- Add optional generation checks for checked builds.

### Phase 5: broader tooling and optimization

- Serialize reference metadata for imports and IDE queries.
- Add cross-function borrow checking where contracts permit it.
- Elide address-backed views when a direct alias is provably sufficient.
- Add target capability negotiation and optional direct pointer-return ABI.
- Consider `string_view`, slices, iterators, and reference members as separate
  proposals.

## 15. Required test matrix

### Parsing and typing

- mutable and const reference syntax for every supported base type;
- reference parameters and returns in functions, methods, and templates;
- rejection of missing initializer, literal binding, incompatible type,
  mutable-to-const violation, arrays of references, and references to `void`;
- overload/signature distinction between value, mutable reference, and const
  reference forms.

### Lifetime

- return global reference succeeds;
- return incoming reference succeeds;
- return receiver/member reference succeeds;
- return local, temporary, and by-value parameter reference fails;
- local reference cannot escape through a global or longer-lived object;
- visible vector/string invalidation is diagnosed.

### COBOL generation

- direct aliases emit no unnecessary pointer storage;
- parameters emit exact `BY REFERENCE`/`LINKAGE SECTION` layouts;
- class and string groups preserve full layout;
- reference returns use one hidden result slot and a typed view;
- vector element addressing is emitted through the shared helper;
- no repeated handwritten reference-lowering blocks appear in generated
  functions.

### Runtime identity

- mutation through a reference changes the original scalar, string, class,
  and vector element;
- copying from a reference produces a separate value;
- binding a returned reference preserves identity;
- two references to the same object observe each other's mutations;
- const references compile for reads and reject writes;
- reference-return calls work across translation units.

### Backend and ABI

- unsupported COBOL capabilities produce a precise backend diagnostic;
- reference ABI version mismatch is rejected before link;
- GnuCOBOL execution tests cover scalar, class, string, and vector cases;
- generated code is tested with bounds checking and mutation-generation checks
  both enabled and disabled.

## 16. Initial completion criteria

The basic reference feature is complete when:

1. `T&` and `const T&` are distinct semantic types, not pointer aliases.
2. References are non-null, initialized once, and non-reseatable.
3. Scalar, class/record, and whole-string reference parameters work through
   native COBOL `BY REFERENCE` calls.
4. Safe class/record/string reference returns preserve identity through the
   hidden-result ABI.
5. Returning a local or temporary reference is deterministically rejected.
6. `Vector<T>&` and checked element `T&` access use the shared vector ABI and
   centralized address helper.
7. Imports, templates, methods, diagnostics, and IDE metadata preserve
   reference qualification.
8. The complete GnuCOBOL execution suite proves mutation, identity, constness,
   lifetime rejection, invalidation behavior, and cross-module compatibility.

## 17. Recommended first vertical slice

Implement one narrow end-to-end case before vector element references:

```cpp
class Counter
{
public:
    int value;
};

Counter global_counter;

Counter& counter()
{
    return global_counter;
}

void increment(Counter& item)
{
    item.value = item.value + 1;
}

void main()
{
    Counter& selected = counter();
    increment(selected);
    return;
}
```

This slice exercises parsing, type identity, class layout, a reference
parameter, a safe reference return, the hidden-result ABI, a typed COBOL view,
and mutation of the original object. Once it works across translation units,
the same machinery can support `string&` and then runtime-selected
`Vector<T>::at()` element references.
