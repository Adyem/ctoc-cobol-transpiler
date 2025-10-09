# COBOL Dialect Requirements

This note captures the boundaries of the COBOL surface that the transpiler currently targets.
It clarifies which language edition the pipeline assumes and documents unsupported constructs
so new work can stay aligned with the agreed subset.

## Target Dialect Profile

- **Baseline:** ANSI-85 compatible COBOL with source accepted by GnuCOBOL without vendor
  extensions.
- **Divisions:** Programs must supply IDENTIFICATION, ENVIRONMENT, DATA, and PROCEDURE
  divisions with the standard section ordering.
- **Files:** Line sequential text and fixed-length sequential record files are required; the
  pipeline does not generate or accept indexed/relative files.
- **Arithmetic:** ADD, SUBTRACT, MULTIPLY, DIVIDE, COMPUTE, and simple MOVE statements are
  assumed to behave per ANSI-85 with integer operands. Floating-point and packed decimal
  arithmetic are not required for the current corpus.
- **Copybooks:** Simple `COPY copybook-name.` directives (without `REPLACING`) are supported as
  long as the copybook's data items are registered with the driver so declared lengths and
  immutability flags propagate into semantic analysis.

## Runtime & Formatting Expectations

- Programs rely on explicit status codes from file operations instead of declaratives or ON
  EXCEPTION handlers.
- WORKING-STORAGE data names must remain within the 30-character ANSI limit so generated
  identifiers remain portable.
- Source formatting follows fixed-format columns with area A beginning in column 8; generated
  listings use 4-space indentation within area B for readability.
- Generated programs assume a CONTIGUOUS literal area; `COPY REPLACING` and other advanced
  library features remain unsupported.

## Procedural Conventions

- User-defined paragraphs are emitted with void-only function signatures; any results must be
  passed back through reference parameters or WORKING-STORAGE records.
- Programs expose a single `void main()` entrypoint. When `argc`/`argv` are present they are
  copied into WORKING-STORAGE so COBOL paragraphs operate on the mirrored argument data.
- Level-78 WORKING-STORAGE constants remain immutable; MOVE statements that target them are
  rejected during semantic analysis so accidental writes surface immediately.【F:transpiler_semantics.cpp†L459-L577】

## Unsupported Features (Require Future Investigation)

- `STRING` / `UNSTRING`, `INSPECT`, and intrinsic functions. The reverse pipeline does not yet
  normalize or regenerate these transformations.
- Altering control flow through `GO TO`, `ALTER`, or PERFORM THRU ranges wider than a single
  paragraph.
- Table features that require `OCCURS DEPENDING ON`, variable-length groups, or RENAMES
  clauses.
- Advanced file features: indexed or relative organization, report writer, or SORT / MERGE
  verbs.
- COBOL 2002/2014 additions (OBJECT REPOSITORY, METHOD-ID, XML GENERATE, etc.).

## Documentation Checklist

1. Update this file whenever the accepted COBOL surface changes.
2. Keep tests synchronized with the declared unsupported features so regressions are flagged.
3. Reference this document from design materials so contributors discover the agreed dialect
   limitations.
