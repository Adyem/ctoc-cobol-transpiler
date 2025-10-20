# CBL-C Runtime ABI Specification

This document describes the Application Binary Interface (ABI) that generated COBOL
programs and the shared runtime use to exchange data across module boundaries. It
captures calling conventions, return-slot expectations, linkage section layout, and
alignment assumptions so external systems can interoperate with the transpiler's
output without reverse-engineering the generated source.

## 1. Calling Convention

Generated call sites always issue COBOL `CALL` statements with by-reference
arguments. Each positional argument becomes a `BY REFERENCE` entry in the `USING`
phrase, and value-returning functions append a dedicated return slot as the final
argument. 【F:transpiler_codegen.cpp†L401-L428】

Unit tests exercise the same convention by constructing calls that pass explicit
buffers followed by the trailing result slot, ensuring the generator always emits
the return storage last. 【F:tests/codegen_tests.cpp†L332-L361】

Standard-library subprograms follow the same layout: operands appear first, the
result slot (when required) is next, and any status flag is appended after the
result so callers can check errors without reordering arguments. 【F:transpiler_standard_library_min.cpp†L16-L34】

## 2. Return Slots and Status Codes

CBL-C functions that return values do not rely on COBOL's `RETURNING` clause.
Instead, the semantics layer allocates a hidden variable and threads it through the
call so the callee stores the value into a by-reference slot. That slot is always
last in the call sequence described above. 【F:transpiler_codegen.cpp†L401-L428】

Many helpers also expose a trailing status flag that reports truncation, overflow,
or domain errors. The checked copy helpers and numeric subprograms map these status
flags to single-digit `PIC 9` fields so callers can branch on zero versus non-zero
results without decoding strings. 【F:transpiler_standard_library_min.cpp†L16-L34】【F:runtime_string.cpp†L72-L112】 The
generator now centralizes those codes in `t_transpiler_standard_library_status` so
every template uses the same literals for "success" (`0`), "invalid argument" (`1`),
"range error" (`2`), and domain-specific violations such as invalid day values (`3`).
Templates embed the associated macros when moving values into or comparing against
`LNK-STATUS`, ensuring downstream emitters and documentation stay synchronized with
the canonical meanings. 【F:cblc_transpiler.hpp†L1044-L1055】【F:transpiler_standard_library_log.cpp†L20-L36】【F:transpiler_standard_library_strtod.cpp†L30-L139】

## 3. Data Representation

### 3.1 Integral Types

Integer parameters and locals map to `USAGE COMP-5` elementary items with explicit
picture clauses sized to the host type. Ten-digit signed values back C `int`, while
`long` and `long long` widen to their respective picture widths with separate
helpers. Signed operands append `SIGN IS LEADING SEPARATE` so COBOL preserves the
expected two's-complement orientation. 【F:transpiler_cobol_types.cpp†L264-L316】

### 3.2 Floating-Point Types

Floating operands map to `USAGE COMP-2` entries for IEEE double precision. The
standard library templates that consume floating values declare each linkage slot as
`USAGE COMP-2` and compute results in that representation. 【F:transpiler_standard_library_min.cpp†L16-L28】

### 3.3 Booleans and Flags

Boolean fields emit as `PIC X` buffers initialized to `'N'`. One-byte booleans leave
the picture unqualified, while wider flag groups add an explicit length multiplier
before the default value so COBOL callers always see `'N'`-filled storage. 【F:transpiler_cobol_types.cpp†L150-L214】

### 3.4 Alphanumeric Buffers

Alphanumeric items expand into `PIC X(n)` pictures sized to the caller declaration.
The runtime string helpers operate on raw character arrays with explicit length and
capacity tracking so COBOL linkage sections and the in-memory representation stay in
sync without implicit null-termination requirements. 【F:transpiler_cobol_types.cpp†L150-L214】【F:runtime_string.cpp†L1-L112】

## 4. Record Layout and Alignment

Record values are represented as contiguous character buffers backed by
`t_runtime_record`. The helper maintains a byte-accurate copy of the COBOL record,
padding unused bytes with spaces and avoiding hidden alignment gaps. Generated COBOL
records therefore map fields sequentially, and the runtime copies buffers without
introducing padding or sentinel markers beyond the caller-declared length. 【F:cblc_transpiler.hpp†L23-L37】【F:runtime_record.cpp†L1-L88】

Because COBOL `USAGE COMP-5` already defines binary alignment, the transpiler does
not inject filler items or adjust offsets—fields appear in the order they are
declared, and any required alignment is handled by the target COBOL compiler.
External programs that interoperate with generated modules should therefore follow
COBOL's natural alignment for COMP-5 storage when sharing memory regions.

## 5. Linking and Interoperability Checklist

1. Allocate argument buffers using the picture sizes described above and pass them
   by reference when invoking transpiled COBOL from native code.
2. Provide writable storage for trailing result and status slots; they must appear
   after all logical arguments.
3. Treat runtime record buffers as fixed-length, space-padded regions without null
   terminators. Trim or pad data explicitly when exchanging structures.
4. Honor COMP-5 alignment rules when embedding COBOL records into other languages;
   no hidden filler or metadata is inserted by the transpiler.

Following these rules ensures native integrations observe the same layout and
calling semantics that the transpiler enforces within its generated COBOL source.
