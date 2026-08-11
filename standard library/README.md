# Native CBL-C standard library

This directory is the source of truth for standard-library implementations.

The files in this directory are CBL-C source, not COBOL templates. The
compiler embeds the source at build time, so a released compiler does not
depend on this directory being present at runtime. The embedded source is
then parsed and lowered through the normal CBL-C-to-COBOL pipeline when the
native standard-library backend is enabled.

Each source unit keeps the public COBOL program name in its entry function
name (`CBLC_STRLEN` becomes `CBLC-STRLEN`). String overloads use the managed
CBL-C `string` value type. Their storage is copied according to the normal
string value ABI, and the compiler-generated lifecycle releases temporary
string storage at function exit, return, and exceptional control-flow edges.

Migration rules:

1. Add the CBL-C source here first.
2. The build-time embedder automatically includes every `.cblc` file here.
3. Validate its generated COBOL and runtime behavior against the existing ABI
   tests before enabling it by default.
4. Remove the corresponding C++ COBOL generator only after the native source
   produces equivalent output and status behavior.

All cataloged units are represented here as `.cblc` sources, including the
character, string, numeric, math, rounding, date, and conversion helpers. The
`vector<T>` template container is also defined here. It provides the core
value-oriented operations `size`, `max_size`, `capacity`, `empty`, `reserve`,
  `shrink_to_fit`, `resize`, `push_back`, `emplace_back`, `insert`, `erase`,
  `pop_back`, `clear`, `assign`, `at`, `front`, and `back`. Storage is dynamically allocated through CBL-C
pointer lowering; growth copies existing elements before releasing the old
block, so it does not rely on a platform-specific realloc extension.

The current element contract is value assignment plus byte-relocatable storage.
Scalar types such as `int` are supported. Full C++-style iterator/reference
semantics, allocator customization, and constructor/destructor dispatch for
non-trivial class or string elements remain planned lifecycle extensions; those
must be implemented before advertising `vector<string>` or arbitrary RAII
element support.
four C++ files remaining under `src/standard_library` are infrastructure only:
the catalog, usage-state tracking, the source embedder/native lowering bridge,
and ABI compatibility wrappers. None contains a COBOL template.

The native backend is the default and the build embeds every source in this
directory into the compiler executable. `CTOC_LEGACY_STANDARD_LIBRARY=1` is
retained only as a diagnostic switch for older consumers; it does not select a
second source implementation. Generated COBOL is syntax-checked as part of the
standard-library validation path; ABI-width and behavioral parity tests remain
the required follow-up for helpers whose historical COBOL ABI used wider
numeric pictures or date-specific intrinsics.

The plan for replacing low-value `status` output parameters with normal return
values, typed results, or exceptions is in
[`STATUS_PARAMETER_MIGRATION.md`](STATUS_PARAMETER_MIGRATION.md).
