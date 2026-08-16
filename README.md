# CBL-C ↔ COBOL Transpiler

`ctoc_cobol_transpiler` is a source-to-source compiler for moving from a small C-style language, **CBL-C**, to COBOL. The project is aimed at COBOL modernization, regression testing, and experiments where business logic should be easier to author while still producing COBOL that can be compiled with GnuCOBOL.

The tool currently supports three CLI directions:

- `cblc-to-cobol`: generate COBOL from CBL-C.
- `cobol-to-cblc`: recover CBL-C from the supported COBOL dialect.
- `standard-library`: emit the bundled COBOL helper programs.

## Quick Start

Build the transpiler and test binary:

```sh
git submodule update --init
make all
make tests
```

Translate a COBOL sample into CBL-C:

```sh
./ctoc_cobol_transpiler --direction cobol-to-cblc \
    --input samples/cobol/minimal_program.cob \
    --output build/minimal_program.cblc \
    --diagnostics verbose
```

Translate CBL-C into COBOL:

```sh
./ctoc_cobol_transpiler --direction cblc-to-cobol \
    --input samples/cblc/return_numeric.cblc \
    --output build/return_numeric.cob
```

The CLI creates missing output directories automatically. See [`docs/getting_started.md`](docs/getting_started.md) and [`docs/cli_usage_examples.md`](docs/cli_usage_examples.md) for more command examples.

## What Is Implemented

### CBL-C Language

CBL-C is intentionally C-like, but its data model maps onto COBOL storage and calling conventions. Implemented language features include:

- Global scalar declarations for `int`, `long`, `long long`, `float`, `double`, `bool`, `char`, fixed-size `char[]`, and `string`.
- Local block storage for scalars, arrays, strings, pointers, and struct/class instances. Block-local aliases stop being visible after the closing brace, while generated backing storage remains unique for COBOL.
- Arithmetic, comparison, boolean, assignment, unary, and `ABS` expressions over integral and floating types, with widening and diagnostics for unsafe conversions.
- `void` and value-returning functions, including parameter passing and generated return slots for COBOL.
- Multi-file CBL-C builds with repeated `--input` / `--output` pairs and `import "file.cblc"` support.
- `struct` and `record`-style storage, nested fields, arrays, and generated COBOL group items.
- `class` declarations with public/private members, constructors, methods, copy-constructor style flows, `const` member enforcement, receiver-specialized COBOL method paragraphs, and C++-style out-of-class method definitions.
- Bounded compile-time type templates for supported structs, classes, functions, methods, pointers, and fixed arrays, with deterministic monomorphization and imported template metadata.
- Basic non-null scalar, string, record, and class references, including const references, reference parameters, and supported reference returns.
- Native `vector<T>` support for approved scalar and value-like element types, including growth, indexing, `at`, `front`, `back`, insertion, erasure, resizing, and lifecycle operations.
- Pointer support for `void *`, `char *`, `int *`, struct pointers, pointer indexing, pointer arithmetic, address-of, dereference, casts, `std::malloc`, `std::realloc`, and `std::free`.
- Built-in `string` behavior including constructor-style initialization, assignment, append, clear, length, capacity, empty, equality, compare, contains, starts-with, and ends-with operations.
- `display`, `return`, `if` / `else`, `while`, function calls, method calls, bounded `try` / `catch` / `throw` exception handling, and selected file-style syntax used by the reverse pipeline.

The authoritative language and compiler-behavior standard is [`docs/cblc_language_standard.md`](docs/cblc_language_standard.md). The samples in [`samples/cblc`](samples/cblc) and [`samples/feature_showcase`](samples/feature_showcase) show larger examples.

The bounded template subset and its current restrictions are specified in the language standard. The broader design and future extensions are documented in [`docs/template_implementation_plan.md`](docs/template_implementation_plan.md).

The implemented source-reference subset and its COBOL lowering design are documented in
[`docs/reference_lowering_design.md`](docs/reference_lowering_design.md).
The standard also marks the remaining reference-descriptor, invalidation, and
other ABI extensions that are not yet complete.

Exception handling currently supports bounded value throws, typed and catch-all
handlers, rethrows, propagation through throwing calls, and cleanup-aware COBOL
lowering. See [`docs/exception_handling_implementation_plan.md`](docs/exception_handling_implementation_plan.md)
for the supported subset and restrictions.

### COBOL Generation

The forward backend can emit COBOL for the supported CBL-C subset. Implemented generation includes:

- Working-storage entries for scalar, string, array, pointer, struct, class, and helper state.
- COBOL procedure generation for functions, methods, constructors, destructors, assignments, arithmetic, calls, displays, conditionals, loops, returns, and lifecycle hooks; supported void, integer-returning, and struct-returning user methods (including parameterized methods) are emitted as reusable receiver-specialized paragraphs.
- Multi-module output with deterministic module initialization and parallel emission.
- Standard-library calls through generated COBOL subprograms and trailing status / return slots.
- Source maps and semantic IR dumps for diagnostics and debugging.

Forward file-control generation supports line-sequential files, fixed-length records, basic open/read/write/close operations, and the restricted `while (read(file, record))` copy loop. Indexed/relative organizations, report-writer clauses, and arbitrary recovered file-control layouts remain outside the current forward subset.

### COBOL → CBL-C Reverse Pipeline

The reverse translator supports a practical ANSI-85-oriented subset:

- Identification, data, and procedure structure for supported programs.
- WORKING-STORAGE scalars and group items, including `PIC X(n)`, `PIC 9(n)`, signed numerics, long / long long widths, and floating patterns.
- Level 01 group recovery into CBL-C records, with subordinate fields preserved.
- `COPY` reconstruction as CBL-C `copy` directives when copybook usage can be recovered.
- `VALUE` defaults for supported scalar declarations.
- Paragraph bodies for common statements such as `MOVE`, `IF`, `PERFORM`, `READ`, `WRITE`, `DISPLAY`, and `STOP RUN`.
- Comment preservation and layout modes for normalized or preserved regenerated CBL-C.
- Copybook dependency graph output for debugging include order.

Unsupported or partial reverse features include `ALTER`, `ENTRY`, `RENAMES`, some `INSPECT` forms, advanced packed decimal cases, deeper legacy control-flow reconstruction, and some complex nested group scenarios. The current dialect notes are in [`docs/cobol_dialect_requirements.md`](docs/cobol_dialect_requirements.md).

### Standard Library And Runtime

The repository includes a generated COBOL standard-library catalog and compiler runtime services. Implemented helper areas include:

- String and memory helpers: strlen, strnlen, strcmp, strcpy, strncpy, strcat, memcmp, checked memory movement, case conversion, and string-to-number conversion.
- Math helpers: abs, fabs, floor, ceil, rounded, banker rounding, sqrt, min, max, power, exp, log, sin, cos, and tan.
- Character classification: isdigit and isalpha.
- Date helpers: YYYYMMDD validation and date-duration calculation.
- Runtime services for scalar operations, strings, records, files, CSV, sorting, memory, encoding, collation, and fixed/variable record handling.

The ABI and runtime contracts are documented in [`docs/abi_spec.md`](docs/abi_spec.md) and [`docs/runtime_api_reference.md`](docs/runtime_api_reference.md).

### Diagnostics And Tooling

Implemented tooling includes:

- Parser error recovery that reports multiple syntax errors in one run.
- Semantic checks for type compatibility, immutable/const writes, private access, string truncation, conversion warnings, overflow, shadowing, unused/uninitialized values, and unreachable code.
- Warning controls such as `-Wconversion`, `-Woverflow`, `-Wstring-trunc`, `-Wshadow`, `-Wunused`, and `-Werror` / `--warnings-as-errors`.
- AST graph export with `--dump-ast`.
- Copybook graph export with `--dump-copybook-graph`.
- Semantic IR dump support with `--dump-semantic-ir`.
- A deterministic CBL-C formatter with normalize and preserve layout modes.
- A CBL-C LSP/editor integration path documented in [`docs/ide_integration.md`](docs/ide_integration.md).
- Fuzzing, property, differential, stress, round-trip, and integration test suites.

## Example CBL-C

```cblc
class Counter
{
    private:
    int value;

    public:
    Counter(int start);
    void add(int delta);
    int current();
};

Counter::Counter(int start)
{
    value = start;
}

void Counter::add(int delta)
{
    {
        int next;
        next = value + delta;
        value = next;
    }
    return;
}

int Counter::current()
{
    return value;
}

void main()
{
    Counter counter(4);
    counter.add(5);
    display(counter.current());
    return;
}
```

This exercises class signatures, out-of-class method bodies, constructor initialization, method calls, return slots, and block-local storage.

## Repository Layout

- [`src`](src): lexer, parser, CBL-C parser/generator pieces, COBOL emitter, semantics, runtime services, standard-library generators, formatter, and LSP code.
- [`tests`](tests): unit, integration, round-trip, standard-library, runtime, compiler, stress, fuzz-adjacent, and validation tests.
- [`samples`](samples): COBOL, CBL-C, multi-module, and feature-showcase programs.
- [`docs`](docs): language, CLI, ABI, runtime, dialect, CI, onboarding, and editor documentation.
- [`compiler_feature_tracker.md`](compiler_feature_tracker.md): detailed implementation tracker and remaining work.

## Testing

Build the test binary:

```sh
make tests
```

Run the full suite:

```sh
./automated_tests
```

Other useful targets:

```sh
make test
make coverage
make fuzz
```

Some COBOL execution tests require `cobc` from GnuCOBOL. The test harness auto-detects it where possible; see [`docs/development_environment.md`](docs/development_environment.md) and [`docs/platform_bootstrap.md`](docs/platform_bootstrap.md) for setup details.

On systems without `cobc`, install a local GnuCOBOL toolchain under `/goinfre` and run the COBOL-backed suite with:

```sh
make install_cobc_goinfre
make tests_with_cobc
```

Forward translation tests are enabled automatically when `cobc` is detected. Set `CTOC_ENABLE_FORWARD_TRANSLATION=0` to skip them, or set it to `1` to force-enable them.

## Current Limitations

The project is active and does not yet cover all COBOL or all C/C++ syntax. Notable gaps include:

- Full forward file-control emission for indexed/relative organizations, report-writer clauses, and arbitrary CBL-C file declarations.
- Legacy COBOL constructs such as `ALTER`, `ENTRY`, broad `INSPECT` support, and `RENAMES`.
- Full packed-decimal and advanced numeric picture coverage beyond the implemented heuristics.
- All possible COBOL table, report-writer, screen-section, and environment-division variants.
- Full C++ template, reference, vector, and exception semantics; the documented CBL-C subsets remain bounded and target-specific.
- General-purpose C++ compatibility; CBL-C only implements the C/C++-like surface needed by the transpiler.

For the normative language rules, use [`docs/cblc_language_standard.md`](docs/cblc_language_standard.md). For implementation progress and open work, use [`compiler_feature_tracker.md`](compiler_feature_tracker.md).
