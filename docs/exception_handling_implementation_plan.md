# Native COBOL Exception Handling and RAII Implementation Plan

**Status:** implementation in progress; the native COBOL lowering slice and versioned exception ABI are implemented, while the full payload ABI, runtime policy, and class hierarchy remain planned
**Target:** CBL-C source with C++-like `try`, `catch`, and `throw`, lowered to portable generated COBOL  
**Hard constraint:** no DWARF metadata, platform unwinder, C++ exception runtime, `setjmp`/`longjmp`, or hidden native stack walking

## 1. Intended result

CBL-C should provide structured exceptions that feel familiar to a C++ programmer while retaining deterministic CBL-C object lifetimes and producing ordinary COBOL control flow.

The current implementation covers parsing and lowering of integer and string throws,
typed and catch-all handlers, rethrows, lexical cleanup metadata, propagation from local
throwing calls through shared generated COBOL exception state, a fatal guard for
double exceptions during generated raising or cleanup, and typed payload storage for
trivially copyable user-defined structs/classes. It also supports recursively copyable
payloads containing inline or dynamically owned string fields by generated field-wise
`BUF`/`LEN` copying and allocation/free cleanup. Pointer fields, arrays, and user-defined
destructors remain rejected until generated move/copy construction and destruction
dispatch are available. Scalar, string, trivial-struct, and supported managed-struct
`const Type&` catches bind to the active payload group. Directly throwing class methods
are emitted as exception-aware paragraphs and callers check the shared context after
method or constructor `PERFORM` calls.
Constructor field progress is tracked in declaration order so a constructor failure
cleans previously completed managed fields while leaving the currently failing field
unclaimed. Nested constructor failure uses the same rule for the enclosing object.
Single inheritance is now accepted for classes with one
non-inherited base. Derived type IDs are included in base-catch matching, and a
generated base payload view is materialized before the handler so base references and
values use the base layout. Multiple inheritance and inherited constructor dispatch
remain outside this slice.
Only public inheritance participates in an external base catch; private and protected
inheritance is preserved in imported metadata but is not treated as an accessible base
conversion.
For the supported single-base form, a defined zero-argument base constructor is emitted
before derived-field and derived-body statements; a base requiring explicit constructor
arguments is rejected until initializer-list forwarding is implemented.
The complete design remains broader: multiple-inheritance matching, context parameters for every
future exception-aware procedure shape, and source-location-rich diagnostics are release-gated
follow-up work. The current ABI already passes the context to exception-aware external module
entries and preserves a shared context for local paragraphs. Exported throwing function
signatures now carry exception ABI version 1, and exported type signatures carry that
version plus their deterministic exception type ID; imported throwing calls and type
stubs reject incompatible versions. Generated context records now include and populate
`CBLC-EX-PAYLOAD-SIZE` for scalar, string, struct, file-status, and allocation failures.
Registration also rejects different exported
type names that claim the same nonzero type ID with a dedicated diagnostic. Type IDs
include the exception ABI version in their deterministic hash input. Runtime
throws of strings whose live length exceeds the bounded 256-byte string payload
storage terminate through `CBLC-TERMINATE` instead of silently truncating. The generated context also keeps
separate dynamic-type and payload-owner fields so derived-to-base matching does not erase the
concrete type identity and payload cleanup can be audited explicitly.
Direct `throw` statements now populate a deterministic source file ID plus portable
source line and column fields in that context; clearing a handled payload resets them,
and `CBLC-TERMINATE` reports the original location when a double exception occurs.
When the second failure is a direct `throw`, its own line and column are retained in
secondary location fields and reported separately by `CBLC-TERMINATE`. Call-site frame
chains remain future metadata. File, raw-call, allocation, and arithmetic failure
adapters also retain the source position of their originating statement when available.
Looped sequential-file reads now retain the position of the source `while (read(...))`
operation as well.
Destructors are rejected if their bodies contain direct throws, exception regions, or
call/file operations whose non-throwing effect cannot yet be proven; cleanup therefore
remains non-throwing in the implemented subset.
External entry points in a unit with any possible exception are emitted with the hidden
`CBLC-EX-CONTEXT` linkage item, so a throwing entry cannot return through an
exception-unaware COBOL procedure signature. This boundary is covered by a generation
regression test.
The parser also preserves source line and column metadata on `TRY_BEGIN` and
`CATCH_BEGIN` statements, extending the existing throw and adapter locations for future
diagnostics and generated debug comments.
Statement substitution used for constructor and method lowering preserves exception
payload-copy requirements and source locations, so managed catches retain their ownership
and diagnostic semantics after lowering.
COBOL emission now protects exact COBOL reserved-word collisions in user identifiers
with a deterministic `CBLC-USER-` prefix; this keeps exception payload declarations and
their generated handler references valid, including class payloads caught by base type.
Generated try/catch labels are qualified by their containing COBOL paragraph, so
propagation across multiple exception-aware functions cannot collide at link or
syntax-validation time.
The current parser accepts one explicit base-class declaration and records its base fields in
the concrete payload layout. Multiple inheritance, inherited constructor dispatch, access-path
metadata remain future ABI work; the current type signature exports the single base name so
separately compiled consumers preserve the same matching relationship. Inheritance must not be
inferred from names.

```cblc
try {
    string name(64) = load_name();
    account value;
    value.open(name);
}
catch (const file_error& error) {
    display(error.message());
}
catch (const runtime_error& error) {
    throw;
}
catch (...) {
    display("unknown failure");
}
```

The required observable behavior is:

1. A thrown value is matched against catches in source order.
2. Derived exceptions match catches for accessible base classes.
3. Every fully constructed automatic object left by control flow is destroyed exactly once, in reverse construction order.
4. Cleanup occurs for normal block exit, `return`, `break`, `continue`, and `throw`.
5. A partially constructed object destroys only the bases, fields, and array elements that completed construction.
6. Managed strings release owned dynamic storage at the end of their lifetime. Borrowed storage is never released by the borrower.
7. A handled exception is destroyed after its handler and catch parameter finish. A rethrow preserves the same exception object.
8. Generated programs implement propagation with COBOL data items, `CALL`, `IF`/`EVALUATE`, `PERFORM`, and generated paragraphs.
9. If a second exception is raised while another exception is being created, propagated, or cleaned up, the program is aborted immediately through `CBLC-TERMINATE`. The second exception must never replace, hide, or resume propagation of the first. This is an explicit termination rule, not an uncaught-exception fallback.

This is C++-like source behavior, not binary compatibility with the C++ exception ABI.

## 2. Design principles

### 2.1 Use explicit propagation, not machine unwinding

Every potentially throwing generated procedure receives a hidden exception-context argument by reference. A callee raises an exception by filling that context and returning through its generated cleanup path. A caller checks the context immediately after a potentially throwing call and branches to the appropriate lexical cleanup landing pad.

This makes exception flow visible in generated COBOL and independent of DWARF, compiler-vendor exception extensions, operating-system structured exceptions, and native stack layout.

### 2.2 Make cleanup a compiler IR concept

The current CBL-C model already records `t_cblc_scope`, `scope_id`, local destructor targets, constructor state, borrowed fields, and `CBLC_STATEMENT_DESTRUCT`. These are a useful foundation, but appending destructor statements while parsing is insufficient once one statement can leave through several edges.

Introduce a control-flow and cleanup lowering stage between semantic analysis and COBOL emission. Parsing should describe source structure; semantic analysis should establish lifetimes and ownership; cleanup lowering should create all exit paths; emission should only print the resulting operations.

### 2.3 Preserve the existing ABI shape

The current ABI passes logical arguments by reference and appends a hidden result slot for value-returning functions. Extend this consistently:

```text
[logical arguments] [optional result slot] [exception context]
```

The exception context must always be last. Non-throwing ABI wrappers may omit it, but calls inside exception-aware CBL-C must use one versioned convention consistently across modules.

### 2.4 Prefer compile-time cleanup plans

Most cleanup order is statically known. Do not build a general heap-allocated runtime destructor stack for ordinary locals. Generate one initialization flag per conditionally initialized object and direct cleanup paragraphs in reverse order. This is easier to inspect, more portable COBOL, and cheaper at runtime.

## 3. Proposed source language

### 3.1 Initial syntax

```cblc
throw expression;
throw;                         // rethrow, valid only in a catch

try {
    statements
}
catch (Type value) {
    statements
}
catch (Type& value) {
    statements
}
catch (const Type& value) {
    statements
}
catch (...) {
    statements
}
```

The first release should support exception values that are complete, non-pointer values with a known CBL-C type. `const Type&` should be the recommended catch form because it avoids a copy and preserves the dynamic exception type.

### 3.2 Deliberate initial restrictions

- No exception specifications or `noexcept` syntax until propagation is stable.
- No catch filters, `finally`, resumable exceptions, or asynchronous exceptions.
- No pointer throws in the first version; their ownership is ambiguous.
- No throwing destructors. Destructors are implicitly non-throwing.
- No `goto` across an initialization or handler boundary.
- No exception escaping a COBOL interoperability entry point that lacks the exception-aware ABI.

RAII makes `finally` unnecessary for resource cleanup. A later `finally` feature may be added for non-resource actions, but it must lower through the same cleanup graph.

### 3.3 Catch matching rules

Catches are examined in source order. A catch matches when:

- its type equals the dynamic exception type;
- its type is an accessible, unambiguous base of that type; or
- it is `catch (...)`.

Semantic analysis must reject duplicate handlers, a handler hidden by an earlier base handler, more than one catch-all, or any handler after a catch-all. Catch-by-value creates a local copy and may slice; catch-by-reference aliases the exception payload and must be represented as borrowed.

## 4. Exception context and native COBOL representation

### 4.1 Context record

Generate a versioned COBOL group item equivalent to:

```cobol
01  CBLC-EXCEPTION-CONTEXT.
    05 CBLC-EX-ACTIVE             PIC X VALUE 'N'.
       88 CBLC-EX-IS-ACTIVE       VALUE 'Y'.
    05 CBLC-EX-TYPE-ID            PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-DYNAMIC-TYPE-ID    PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-PAYLOAD-SIZE       PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-PAYLOAD-OWNER      PIC X VALUE 'N'.
    05 CBLC-EX-SOURCE-FILE-ID     PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-SOURCE-LINE        PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-SOURCE-COLUMN      PIC 9(9) COMP-5 VALUE 0.
    05 CBLC-EX-PAYLOAD            PIC X(CBLC-EX-PAYLOAD-MAX).
```

Exact numeric pictures should follow existing portability conventions. The context belongs to the outermost exception-aware call chain and is passed by reference, which avoids process-global state and permits recursion. Threaded execution must allocate one context per task or thread.

### 4.2 Type IDs and inheritance

Assign each concrete throwable type a deterministic ID derived from its fully qualified source identity and ABI version, with collision detection during compilation/link-manifest validation. Emit a type registry containing:

- dynamic type ID;
- direct base type IDs;
- payload size and alignment policy;
- copy/move construction dispatch;
- destruction dispatch; and
- optional diagnostic name.

COBOL should perform matching through generated `EVALUATE` paragraphs. Do not depend on procedure pointers. Each module can emit an `EVALUATE CBLC-EX-DYNAMIC-TYPE-ID` dispatcher, and the link manifest must identify which module owns each concrete type operation.

### 4.3 Payload storage

For the first portable implementation, use bounded `PIC X(n)` payload storage with generated `REDEFINES` views. Compute the required maximum from all throwable types visible to the linked program, subject to a configured upper bound. Reject a throwable whose representation exceeds the bound with a clear diagnostic.

The payload is a real owned CBL-C object, not merely copied bytes when its type has managed members. Raising code must invoke the type's generated move/copy constructor into exception storage, and clearing the context must invoke the generated destructor dispatcher. Raw `MOVE` is valid only for trivially copyable payloads.

For separately compiled or dynamically loaded modules, include required payload size and exception ABI version in module metadata. A later ABI can replace bounded inline storage with a native COBOL allocated payload, without changing source semantics.

### 4.4 Logical stack traces without DWARF

Stack traces, if added, should use compiler-generated logical frame records: stable function ID, source file ID, line, and optional call-site ID. Calls explicitly push/update a bounded diagnostic frame list in the context and restore it on normal return. This remains portable COBOL metadata and does not inspect the machine stack.

It should be a later opt-in feature; exception correctness must not depend on it.

## 5. Control-flow lowering

### 5.1 Required intermediate representation

Extend or replace the flat `t_cblc_statement` array with a structured function IR that can represent:

- basic blocks and explicit successors;
- lexical scopes and parent scopes;
- construction-complete operations;
- cleanup actions with initialization predicates;
- potentially throwing calls (`invoke`-like terminators);
- `throw` and `rethrow` terminators;
- try regions and ordered handler lists;
- normal return, break, and continue targets; and
- exception propagation targets.

The source parser may initially produce structured statement nodes which are then flattened into this IR. Do not encode handler structure in string fields such as `target` and `source`.

### 5.2 Cleanup graph

For every control-flow edge from scope A to target scope B:

1. Find the lowest common ancestor of A and B.
2. Enumerate scopes exited from A up to, but excluding, that ancestor.
3. Emit each exited scope's cleanup actions in reverse completed-construction order.
4. Continue to the return, loop target, handler dispatcher, or caller propagation block.

Cleanup blocks may be shared when the cleanup sequence and destination are identical. Name generated paragraphs deterministically, for example `CBLC-CLEANUP-S004-TO-S001-THROW`.

### 5.3 Generated COBOL shape

Conceptually, a throwing call inside a try lowers as follows:

```cobol
CBLC-TRY-0001-BODY.
    CALL "LOAD-NAME"
        USING BY REFERENCE WS-NAME
              BY REFERENCE LNK-CBLC-EXCEPTION
    END-CALL
    IF CBLC-EX-IS-ACTIVE
        GO TO CBLC-CLEANUP-SCOPE-0004-THROW
    END-IF
    ...
    GO TO CBLC-TRY-0001-DONE.

CBLC-CLEANUP-SCOPE-0004-THROW.
    IF WS-VALUE-INIT = 'Y'
        PERFORM ACCOUNT-DESTRUCT-WS-VALUE
        MOVE 'N' TO WS-VALUE-INIT
    END-IF
    IF WS-NAME-INIT = 'Y'
        PERFORM STRING-DESTRUCT-WS-NAME
        MOVE 'N' TO WS-NAME-INIT
    END-IF
    GO TO CBLC-TRY-0001-DISPATCH.

CBLC-TRY-0001-DISPATCH.
    PERFORM CBLC-MATCH-FILE-ERROR
    IF CBLC-EX-MATCHED = 'Y'
        GO TO CBLC-TRY-0001-CATCH-0001
    END-IF
    ...
    GO TO CBLC-FUNCTION-PROPAGATE.
```

The exact syntax must be validated against every supported COBOL dialect. `GO TO` here is generated structured plumbing, not exposed as the primary source-language model.

### 5.4 Calls that cannot throw

Semantic analysis should compute `may_throw` for every function, method, constructor, intrinsic, and imported declaration. A call proven non-throwing needs no post-call branch. Initially, be conservative: unknown external calls are potentially throwing only if declared through an exception-aware CBL-C signature; raw COBOL calls cannot populate the context unless wrapped.

## 6. Precise lifetime and RAII rules

### 6.1 Lifetime start

An object's lifetime begins only after its initialization completes. Set its generated initialization flag after successful construction, never before. Trivial scalar locals need no cleanup flag unless later analyses require definite initialization.

For a class object, construction order is:

1. base subobjects in declaration order;
2. fields in declaration order, regardless of initializer-list spelling;
3. constructor body.

If any step throws, destroy only completed subobjects, in reverse order. Do not invoke the destructor body for the not-fully-constructed outer object.

### 6.2 Ordinary block exit

At a closing brace, destroy automatic objects declared in that block in reverse order. A local declared inside a loop body is destroyed at the end of every iteration. An object declared in a loop initializer remains alive for the loop statement's defined enclosing scope.

### 6.3 Return

Evaluate and fully materialize the return value before destroying locals. For class or string return values:

1. construct or move into the caller-provided result slot;
2. mark the result slot valid;
3. clean up local scopes;
4. return with no active exception.

If materializing the return value throws, the result slot remains invalid, ordinary local cleanup runs, and the exception propagates. Never destroy storage that has already transferred ownership to the result slot.

### 6.4 Break and continue

`break` destroys scopes nested inside the loop and then branches to the loop exit. `continue` destroys the current iteration's nested scopes and then branches to the condition/increment target. The cleanup graph should provide these without special destructor insertion in the parser.

### 6.5 Temporaries and full expressions

Track materialized temporaries explicitly. Except when lifetime extension is defined, a temporary is destroyed at the end of the full expression in reverse creation order. A temporary needed to initialize an exception payload must survive until the payload's move/copy completes, then be destroyed before propagation begins.

This phase is necessary before supporting complex nested calls safely. Until implemented, reject expressions whose required temporary lifetime cannot be represented correctly.

The current native CBL-C subset enforces this boundary conservatively: `throw` accepts
literal values, existing scalar/string/struct objects, and rethrow, but does not accept
temporary constructor expressions or other unnamed materializations. Those forms fail
parsing instead of entering the exception context without a tracked owner.

### 6.6 Managed strings

The current language defines `string` as a managed fixed-capacity value object, while the emitter can also use dynamic backing storage for relevant forms. Give every string value an explicit ownership category:

- **inline owned:** buffer is part of the value; destructor resets metadata but does not free storage;
- **dynamic owned:** destructor releases its allocated pointer exactly once and clears pointer, length, and capacity;
- **borrowed view/reference:** destructor performs no release;
- **moved-from:** valid empty non-owner; destructor is harmless.

String cleanup happens at the same lexical boundaries as class cleanup. A string field is destroyed as part of its containing object's reverse field destruction. String arrays are destroyed in reverse element order, limited to the initialized element count.

Assignment must not change lexical lifetime. It replaces the held value using an exception-safe sequence: prepare new storage/value first, then release old ownership, then commit metadata. Self-assignment and aliasing require explicit handling.

### 6.7 Catch parameter and exception lifetime

The exception payload is owned by the exception context from successful raise construction until one of these events:

- the selected handler completes normally;
- the handler exits by `return`, `break`, or `continue` where legal;
- the handler throws a different exception, after the old payload is cleared;
- the handler rethrows, in which case ownership remains in the context; or
- an interoperability boundary translates and consumes it.

A reference catch parameter is borrowed and ends at the handler's closing brace. A value catch parameter is a separate local and is destroyed before the original exception payload is cleared. Ensure cleanup order is: handler locals, value catch parameter, original payload.

### 6.8 Throwing during cleanup

Initially, destructors and cleanup helpers are implicitly non-throwing. More generally, once exception processing has started, any second exception raised while constructing the exception payload, propagating it, copying a catch-by-value parameter, or running cleanup is a fatal double exception. Call a generated `CBLC-TERMINATE` paragraph immediately, report both type IDs and source locations where available, and abort the generated program according to the configured runtime policy. No catch handler may intercept this second exception, and silently replacing the original exception is forbidden.

A bare `throw;` is not a second exception: it continues propagation of the currently active payload. Throwing a new exception from a catch is allowed only after the caught exception and catch-owned state have been transitioned according to the replacement-throw rules; it must not leave two active payloads in one context.

Later `noexcept(false)` support requires an explicit state machine and should not be added until this rule is tested across all cleanup paths.

## 7. Frontend and semantic implementation

### 7.1 Lexer and parser

Add tokens for `try`, `catch`, and `throw`, plus ellipsis recognition if not already available. Add structured nodes or CBL-C statement forms for:

- try statement with one body and one or more handlers;
- typed catch declaration and catch-all;
- throw expression; and
- rethrow.

Preserve source spans for the try keyword, each catch type, throw expression, and handler body. Parser recovery should synchronize at `catch`, closing braces, and semicolons so one malformed handler does not obscure the rest of a file.

### 7.2 Type checking

Semantic analysis must verify:

- thrown type is complete, storable in the exception ABI, and copy- or move-constructible;
- catch type is complete and derives from the throwable root if such a root is required;
- references do not outlive the handler;
- catch ordering is reachable;
- bare `throw;` appears in a dynamically active catch;
- constructor/destructor accessibility permits payload creation and cleanup;
- no forbidden control transfer enters a try or catch scope; and
- imported function exception ABI metadata is compatible.

Do not require every exception to derive from one base in the first internal model. It is useful to provide a standard `exception` root and recommend it, but arbitrary complete class values can remain supportable if the type registry handles them.

### 7.3 Effect analysis

Compute a function-level summary:

```text
never throws
may throw {known type set}
may throw unknown
```

Known sets improve unreachable-catch diagnostics and allow call-site checks to be removed. Recursive call graphs need a fixed-point analysis. Imported declarations carry a conservative summary in their manifest. This effect system should remain internal initially and can later support `noexcept`, documentation, and optimization.

The current implementation exports a bounded set of known exception type IDs plus an
`unknown` flag. Direct typed throws populate the set; rethrows, file operations, and
exception-capable external calls conservatively set `unknown`, so later optimizations
cannot accidentally suppress propagation checks.
Local call graphs are resolved with a bounded monotone fixed-point pass, so a throw is
also visible through transitive calls regardless of function declaration order. Export
registration performs this pass before publishing function metadata.
The same bounded summary fields are now present on exported method and constructor
signatures; direct member-body throws are recorded, while unresolved member effects
remain marked unknown.

## 8. Class hierarchy and exception safety

The existing `t_cblc_struct_type` currently models class/struct identity, fields, methods, constructors, and destructors but does not expose base-class metadata in the shown representation. Exception inheritance requires adding explicit base descriptors rather than inferring relationships from names.

Each base descriptor should include source identity, access, layout offset/view information, and an unambiguous path identifier. Catch conversion uses this metadata to bind a base reference view into the payload. Multiple inheritance may be deferred, but the metadata must not assume that every base begins at offset zero.

Define exception-safety guarantees for generated operations:

- constructors: no leak on partial failure;
- string assignment and growth: strong guarantee where allocation can fail;
- ordinary class assignment: basic guarantee initially unless a type is proven safely swappable;
- destructor: non-throwing;
- exception raise: either payload construction succeeds and activates the context, or the runtime terminates if it cannot represent the failure safely.

## 9. Mapping native COBOL failures

COBOL has operation-specific failure mechanisms, not one portable general exception system. Integrate them at the operation site:

- `ON SIZE ERROR` maps arithmetic overflow to a configured CBL-C numeric exception;
- `INVALID KEY` maps indexed-file failures to a file exception carrying status information;
- `AT END` remains ordinary EOF control flow unless an API contract requests an exception;
- `CALL ... ON EXCEPTION` maps a missing/unavailable subprogram to a call exception;
- file status fields are captured before cleanup or another I/O operation overwrites them.

`DECLARATIVES` and `USE AFTER STANDARD ERROR PROCEDURE` may support file-level adapters, but they must not implement lexical `try` by themselves. Their control rules and portability differ by dialect. Normalize any caught COBOL condition into the same exception context, then use the normal cleanup graph.

The current lowering implements the arithmetic portion of this boundary for computations inside
a `try`: generated `ON SIZE ERROR` clauses raise an integer-compatible numeric failure, apply
the active/raising/cleaning double-exception guard, and enter ordinary handler dispatch. File-status
handling is also implemented for the generated sequential-file model: each file has a `FILE STATUS`
item, OPEN/CLOSE/WRITE failures and non-EOF READ failures become string payload exceptions, and
status `10` remains ordinary `AT END` behavior. Native CBL-C indexed declarations using
`file indexed <name> \"<path>\";` now emit `ORGANIZATION IS INDEXED`, a record key, and an
`INVALID KEY` scope on writes; the resulting file status is normalized through the same
string exception payload and cleanup path. External-call adapters remain release-gated
follow-up work. Raw external calls made inside a `try` now use native
`CALL ... ON EXCEPTION` lowering with the same integer-compatible context payload and
double-exception guard; calls already using the exception-aware ABI continue to pass the
shared context directly.
Raw calls in a unit that already has exception-aware code are also adapted outside a
lexical `try`: a native call failure fills the shared context and exits through the
function's normal cleanup/propagation path. Units with no exception context retain the
ordinary raw-call behavior. The effect analysis marks these adapted calls as potentially
throwing so local callers still emit their immediate context checks.

Raw external COBOL calls should use generated adapters with one of three declared policies:

1. cannot throw;
2. returns a status translated into a CBL-C exception; or
3. exception-aware and accepts the CBL-C context ABI.

Pointer allocation through the built-in `std::malloc`/`std::realloc` lowering is also
checked inside a `try`: after native `ALLOCATE`, a null returned pointer is translated
into the same integer-compatible exception context. This keeps allocation failure from
silently continuing past a potentially throwing operation; allocation failure during
active exception processing still terminates through the double-exception guard.
Managed-string exception-payload copies perform the same null check while the raising
state is active, so failure to construct or copy the payload reaches `CBLC-TERMINATE`
instead of leaving a partially initialized exception visible to a handler.

## 10. Integration with existing features

### 10.1 Functions, methods, and constructors

Add `may_throw` and exception ABI version to `t_cblc_function`, `t_cblc_method`, and constructor metadata. Generated method calls pass the receiver, logical arguments, optional result slot, then exception context. Constructor calls additionally expose construction progress to their failure cleanup blocks. Function, method, and constructor exports carry bounded known throw IDs plus an unknown-effect flag.

### 10.2 Templates

Exception lowering occurs after template instantiation. Each concrete instantiation receives its concrete throwable types, payload requirements, cleanup actions, and effect summary. Template definitions should preserve source locations so diagnostics report both the throw/catch inside the definition and the instantiation that made it invalid.

### 10.3 Imports and incremental compilation

Extend exported module metadata with:

- exception ABI version;
- maximum payload requirement;
- exported throwable type IDs and hierarchy edges;
- copy/move/destruct operation ownership;
- per-function throw summary, consisting of bounded known exception type IDs plus an
  unknown-effect flag; and
- runtime policy fingerprint.

The current implementation has begun this contract: throwing function signatures carry
`exception_abi_version` and a runtime-policy fingerprint, type signatures carry the same
version and fingerprint plus a deterministic `exception_type_id`, and import/call resolution
rejects incompatible versions or policy fingerprints with dedicated diagnostics. Payload
bounds and operation-owner registries remain future metadata fields; the current metadata
now carries a bounded per-type payload-size requirement and rejects values above
`CBLC_EXCEPTION_PAYLOAD_MAX`.
Function signatures also carry the bounded throw summary described in section 7.3.

Include these fields in incremental cache keys and layout fingerprints. A mismatched ABI must be a link/transpile diagnostic, never silently accepted.

### 10.4 Intrinsics and runtime helpers

Give each intrinsic a throw policy. Existing status-returning helpers can remain internally status based; their generated wrapper translates non-success into an exception when called through a throwing CBL-C API. This avoids rewriting every runtime helper at once and keeps low-level runtime code reusable.

### 10.5 Diagnostics and debugging

Add diagnostics for escaping exceptions at non-aware boundaries, unreachable catches, illegal rethrow, payload overflow, destructor throws, and unsupported lifetime forms. Generated COBOL comments should identify source try regions and cleanup paragraphs when debug comments are enabled.

No diagnostic feature should depend on DWARF. Source mapping can use existing source spans and generated sidecar maps.

## 11. Recommended implementation sequence

### Phase 0: freeze semantics and invariants

- Add this design's accepted subset to the language standard only after tests prove it.
- Decide the portable payload bound and termination policy.
- Define exception ABI version 1 and module metadata schema.
- Add negative compile fixtures before enabling syntax publicly.

**Gate:** approved examples have an unambiguous destruction trace on paper.

### Phase 1: structured scopes and cleanup IR

- Preserve current `scope_id` data but add parent-aware control-flow exits.
- Convert existing parse-time local destructor insertion into cleanup actions.
- Lower normal fallthrough and `return` through cleanup blocks.
- Add initialization flags for conditional class/string construction and initialized counts for arrays.

**Gate:** all existing lifecycle tests pass, plus nested return/loop/string cleanup tests, before exceptions are introduced.

### Phase 2: parser and semantic model

- Implement `try`, typed catches, catch-all, `throw expression`, and rethrow.
- Add structured handler/type metadata.
- Implement handler ordering, accessibility, completeness, and rethrow diagnostics.
- Add conservative `may_throw` analysis.

**Gate:** parser and semantic tests pass without COBOL emission.

### Phase 3: exception ABI and payload lifecycle

- Emit the exception context and hidden final parameter.
- Generate stable type IDs, hierarchy matching, and payload dispatchers.
- Implement raise by copy/move, clear, rethrow, and terminate-on-double-exception.
- Version exported metadata and reject incompatible imports.

**Gate:** direct throw/catch/rethrow works for scalar-like and class payloads with exact destructor counts.

### Phase 4: COBOL propagation and handlers

- Mark throwing calls and insert immediate active-context checks.
- Generate cleanup landing paragraphs and ordered handler dispatch.
- Implement propagation out of functions and methods.
- Deduplicate equivalent cleanup blocks without changing order.

**Gate:** nested and cross-function exceptions work in each supported COBOL dialect.

### Phase 5: complete RAII integration

- Implement partial constructor cleanup for bases, fields, and arrays.
- Complete string ownership and move-state tracking.
- Implement temporary full-expression cleanup.
- Cover return-value construction, catch-by-value, and replacement throws.

**Gate:** leak/fault-injection tests pass at every possible throwing construction step.

### Phase 6: COBOL and runtime error adapters

- Translate arithmetic, file, allocation, and external-call statuses.
- Add wrappers for exception-unaware COBOL entry points.
- Ensure status fields are captured before unwinding cleanup.

**Gate:** native COBOL failures are catchable as typed CBL-C exceptions and do not bypass RAII.

### Phase 7: optimization and future features

- Remove checks from proven non-throwing calls.
- Merge cleanup blocks and compact type dispatch tables.
- Add optional logical stack traces.
- Consider `noexcept`, `exception_ptr`, nested exceptions, and dynamic payload ABI v2.

## 12. Test strategy

### 12.1 Compile-time tests

- every valid grammar form and malformed recovery case;
- illegal rethrow and try without catches;
- duplicate, hidden, inaccessible, ambiguous, and post-catch-all handlers;
- incomplete or oversized payload types;
- incompatible imported ABI and colliding type IDs;
- illegal control transfer across lifetime boundaries.

### 12.2 Runtime trace tests

Use test classes whose constructors, destructors, copies, and moves append numeric markers to a fixed buffer. Assert exact traces for:

- normal nested block exit;
- throw before and after each local construction;
- return from try and catch;
- break/continue inside try;
- nested try with inner handle and outer propagation;
- rethrow and replacement throw;
- derived-to-base catch;
- catch-by-value and catch-by-reference;
- partial base/field/array construction;
- throw while constructing a return value;
- string inline, dynamic, borrowed, moved-from, field, and array lifetimes;
- recursive calls with one shared explicit context;
- cross-module propagation.

### 12.3 Fault injection

Every allocating or status-producing operation should support deterministic test failure at operation N. Run constructor, string growth, payload copy, file adapter, and return-value tests with failure injected at each point. Assert no double destruction, leak, stale active flag, or use of an invalid result slot.

### 12.4 Generated COBOL verification

For each supported dialect:

- compile generated source;
- run behavior tests;
- inspect that every potentially throwing `CALL` has an immediate context check;
- ensure no forbidden runtime/unwinder symbols are linked;
- validate context layout and argument ordering across separate modules; and
- retain golden snippets for representative cleanup graphs, not entire generated files.

## 13. Acceptance invariants

The feature is not complete until all of these are mechanically tested:

1. Destruction count equals successful construction count for every owned object.
2. Destruction order is the exact reverse of completed construction order.
3. Borrowed pointers and references are never freed by cleanup.
4. Moved ownership has exactly one eventual destructor owner.
5. No ordinary statement executes after a call reports an active exception.
6. A handler sees a fully constructed payload of the correct dynamic type.
7. Rethrow preserves payload identity and dynamic type.
8. A handled exception context is empty before normal execution resumes.
9. Cross-module calls agree on context layout, type IDs, and payload bounds.
10. Generated exception handling contains no DWARF or native unwinder dependency.
11. A second exception during raise, propagation, catch-parameter construction, or cleanup always reaches `CBLC-TERMINATE`; it is never dispatched to another catch.

## 14. Architectural changes recommended first

The most important enabling change is not adding three parser keywords. It is making scope exit explicit.

In the current implementation, `t_cblc_function` owns a flat statement array and `local_destructor_targets`; parser code appends destructor statements around returns and scope endings. Evolve this in small compatible steps:

1. Add a cleanup-action table keyed by existing `scope_id`.
2. Record construction points and ownership without immediately appending destruction statements.
3. Add explicit exit statements carrying destination kind and destination scope.
4. Run a cleanup-lowering pass that emits `CBLC_STATEMENT_DESTRUCT` and string cleanup operations for every edge.
5. Verify existing behavior, then add exception edges and handler dispatch.
6. Once stable, retire parser-owned destructor insertion and make the cleanup pass the sole authority.

This ordering reduces risk: deterministic cleanup becomes correct for ordinary control flow first, and exception support then adds a new exit destination rather than inventing a second lifetime system.

## 15. Future extensibility

The proposed model leaves room for:

- `noexcept` and compiler-verified non-throwing APIs;
- standard exception base classes with message and error-code fields;
- `exception_ptr` through ref-counted or copied context payloads;
- nested exceptions and causal chains;
- logical portable stack traces;
- coroutine/async suspension, provided cleanup frames become resumable state;
- thread/task-local top-level contexts;
- a larger dynamically allocated payload ABI;
- richer class inheritance and multiple-base matching;
- foreign COBOL status adapters; and
- optimization from whole-program throw-set analysis.

All of these should build on the same four abstractions: explicit exception context, stable type metadata, structured control-flow exits, and compiler-generated cleanup actions. None should introduce a separate unwinding route.

## 16. Recommended first milestone

Implement cleanup IR for existing classes and strings before exposing `try` syntax. The milestone should support nested lexical scopes, normal fallthrough, and `return`, with initialization flags and exact reverse cleanup. Once that is proven, implement a minimal exception slice:

```cblc
try {
    throw runtime_error("message");
}
catch (const runtime_error& error) {
    display(error.message());
}
```

Lower it through one explicit exception context and generated COBOL paragraphs. Then expand to function propagation, nested handlers, class inheritance, constructor failure, and COBOL status adapters in that order. This produces useful vertical progress while keeping lifetime correctness as the release gate at every phase.
