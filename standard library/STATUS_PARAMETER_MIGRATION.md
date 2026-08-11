# Replacing standard-library status parameters

## Purpose

Many native standard-library entry points currently use this shape:

```cblc
void F(input, result, status)
```

That shape is convenient for a low-level COBOL `CALL`, but it is a poor public
CBL-C API. Successful calls assign a meaningless zero, the real result looks
like an input argument, and integer statuses have no shared type or documented
meaning.

CBL-C should not reproduce undefined C behavior blindly. It should offer
familiar functions with deterministic, platform-independent behavior. A
status parameter should not survive merely because an older generated COBOL
helper needed one.

## Separate the public API from the helper ABI

There are two interfaces:

1. The public expression, such as `double value = std::sqrt(input);`.
2. The generated COBOL helper program and its linkage parameters.

The public API should use normal return values. During migration, the compiler
may still lower it to a legacy helper containing hidden `result` and `status`
fields. The catalog must describe those as compatibility ABI details rather
than exposing them in CBL-C source signatures.

## Decision rules

Apply these rules in order:

1. A total function with one result returns that result and has no status.
2. A managed mutator that cannot fail under its type invariants returns `void`.
3. An exceptional failure uses a typed exception.
4. An expected failure uses a typed result object with a named error enum.
5. An operation with several outputs returns a class or struct.
6. A raw integer status remains only at an external compatibility boundary.

Exceptions follow the language exception rules. If an exception is thrown
while another exception is being propagated, the program aborts. Normal RAII
cleanup still runs during ordinary unwinding before a handler is entered.

## Remove status and return the value

These functions either always report success today or naturally produce one
value:

| Current helper | Recommended public API | Reason |
| --- | --- | --- |
| `fabs(operand, result, status)` | `double std::fabs(double operand)` | Status is always zero. |
| `sin`, `cos`, `tan` | Return `double` | Status is always zero. |
| `fmin`, `fmax` | Return `double` | Status is always zero. |
| `floor`, `ceil`, `round` | Return `double` | The result is the useful output. |
| `date_duration_days` | Return `int` | Status is always zero. |
| `strcmp`, `memcmp` | Return `int` | The comparison result is the only meaningful output. |
| `isalpha`, `isdigit` | Return `int` | These predicates have one Boolean-like result. |

Representational limits in the COBOL implementation are not a second result.
Define a numeric overflow policy and, where needed, throw `range_error`.

## Remove status from managed String methods

Managed strings own their capacity. They should grow or throw, not report C
buffer truncation through an integer parameter.

```cblc
class String
{
    void assign(string source);
    void assign(string source, int count);
    void append(string source);
    void to_upper();
    void to_lower();
};
```

| Current method | Recommended method |
| --- | --- |
| `strcpy(source, status)` | `assign(source)` |
| `strncpy(source, request, status)` | `assign(source, request)` |
| `strcat(left, right, status)` | Assignment followed by `append`, or a static `concat` returning `String`. |
| `toupper(status)` | `to_upper()` returning `void`. |
| `tolower(status)` | `to_lower()` returning `void`. |

Allocation failure should use the allocation exception. If strings retain a
fixed maximum length, exceeding it should throw `length_error`; it should not
silently truncate and set status 1.

## Return a value and throw for exceptional failures

| Function family | Return | Failure policy |
| --- | --- | --- |
| `abs` | `int` | Throw `overflow_error` for an unrepresentable most-negative value. |
| `exp` | `double` | Throw `range_error` on overflow. |
| `log` | `double` | Throw `domain_error` for values less than or equal to zero. |
| `sqrt` | `double` | Throw `domain_error` for negative values. |
| `pow` | `double` | Throw `domain_error` for invalid operands and `range_error` on overflow. |
| `banker_round` | `double` | Throw `invalid_argument` for an unsupported scale and `range_error` on overflow. |

Exact limits must be defined by the CBL-C numeric model, not host C `errno`,
floating-point exception flags, or platform-specific integer widths.

## Parsing needs a typed result

Replacing `atoi`, `atol`, `atoll`, or `strtod` status with a bare scalar does
not solve ambiguous failures. Provide a familiar compatibility API and a
strict typed API:

```cblc
int std::atoi(string source);
ParseIntResult cblc::parse_int(string source);
double std::strtod(string source);
ParseDoubleResult cblc::parse_double(string source);
```

Suggested result classes:

```cblc
class ParseIntResult
{
    public:
    int value;
    int consumed;
    ParseError error;

    int ok();
};

class ParseDoubleResult
{
    public:
    double value;
    int consumed;
    ParseError error;

    int ok();
};
```

`ParseError` should use named values such as `none`, `invalid_syntax`,
`out_of_range`, and `trailing_characters`. New code must not assign different
meanings to anonymous values such as 1, 2, and 3 in each function.

Do not wrap one scalar in a result struct. A one-value operation returns its
scalar directly (or throws when failure is exceptional). A result object is
reserved for two or more independently meaningful outputs, such as the date
parser's year, month, day, packed value, serial value, and error code.

Define the compatibility forms explicitly. A reasonable deterministic policy
is to return zero when no conversion is possible, clamp on overflow, stop at
the first unconsumed character, and never invoke undefined behavior. Strict
applications use the typed parser or a future throwing convenience function.

## Multi-output operations return an object

`date_parse_yyyymmdd` currently outputs year, month, day, packed form, serial
form, and status. Return a coherent object:

```cblc
class DateParseResult
{
    public:
    int year;
    int month;
    int day;
    int packed;
    int serial;
    DateParseError error;

    int ok();
};
```

A strict `Date parse_yyyymmdd(string input)` may throw `invalid_argument`, while
`try_parse_yyyymmdd` returns the result above. Expected user-input failure is
usually clearer through `try_parse`; invalid program state is usually clearer
through an exception.

## Raw buffers need explicitly unsafe and checked APIs

The `char *` versions of `strcpy`, `strncpy`, and `strcat` cannot infer actual
destination capacity. An integer status does not make that interface safe.

Split the APIs:

- managed `String` methods grow safely and have no status;
- raw compatibility functions retain familiar behavior and eventually return
  the destination pointer where pointer returns are supported;
- checked raw functions take an explicit capacity and return a named result.

```cblc
class CopyResult
{
    public:
    int written;
    int truncated;
};

CopyResult cblc::copy_checked(char *destination, int capacity,
    char *source, int source_length);
```

Truncation is expected control flow and should not normally throw. Negative
lengths, impossible capacities, and invalid pointers are contract violations
and should be diagnosed before entering the helper where possible.

## Disposition of current status parameters

| Source unit or family | Action |
| --- | --- |
| `fabs`, `sin`, `cos`, `tan`, `min`, `max`, `date_duration` | Delete status immediately; it carries no information. |
| `toupper`, `tolower` | Character forms return the converted character; String forms mutate and return `void`. |
| `floor`, `ceil`, `rounded` | Return the result; throw `range_error` only when required. |
| `abs` | Return `int`; replace status 1 with `overflow_error`. |
| `exp`, `log`, `sqrt`, `powerof` | Return `double`; replace domain/range status with typed exceptions. |
| `banker_round` | Return `double`; status 2 becomes `invalid_argument`, status 1 becomes `range_error`. |
| `atoi`, `atol`, `atoll` | Direct return for compatibility; add typed parse-result APIs. |
| `strtod` | Direct return for convenience; add a typed result with consumed length and named error. |
| `strcmp`, `memcmp` | Return the comparison result directly; keep raw buffer lengths explicit. |
| `isalpha`, `isdigit` | Return the classification result directly. |
| `date_yyyymmdd` | Return `DateParseResult` or `Date`; replace numeric statuses with a named enum. |
| managed String copy/concatenation | Use assignment and append methods without status. |
| raw `strcpy`, `strncpy`, `strcat` | Keep compatibility separately; add checked APIs returning `CopyResult`. |

## Implementation plan

## Current implementation status

The first migration slice is implemented:

- total numeric helpers now return values directly, including comparisons and
  character predicates;
- managed String adapters no longer expose helper status parameters;
- direct-return lowering supports integer and double results for functions and
  class methods;
- `DateParseResult` is a genuine multi-field by-value result. Its generated
  COBOL ABI uses one hidden group containing all six fields; no one-field
  wrapper is introduced;
- the date result's helper functions are emitted as local COBOL paragraphs and
  the public `F` function is selected explicitly as the entry point;
- `cblc::parse_int` and `cblc::parse_double` now return three-field value
  objects (`value`, `consumed`, and `error`) for callers that need to
  distinguish invalid syntax, trailing characters, and range failures;
- managed `String` assignment, append, and concatenation now use the dynamic
  string value operations, so they grow with the owned value instead of
  silently stopping at a literal 255-byte limit;
- raw buffer copy status remains at the explicitly unsafe compatibility
  boundary;
- all embedded standard-library programs generate successfully after the
  signature changes, including both the new date-result entry and its legacy
  adapter.

Capacity-aware `strncpy` semantics and allocation-failure translation remain
follow-up work. Compatibility parsers still use deterministic scalar behavior,
while the typed parser entries expose the additional information needed by
strict callers. The raw fixed-size helper ABI must not be described as a
checked managed-string API.

### Phase 1: describe the real API in the catalog

Extend each catalog entry with its public return type, input list, hidden ABI
outputs, failure policy (`none`, `throws`, `result_object`, or
`compatibility`), legacy program name, and ABI version. Stop deriving public
semantics from the low-level `F(...)` signature.

Add validation that rejects a public status parameter unless the entry is
explicitly an external ABI adapter.

### Phase 2: remove meaningless statuses

Start with `fabs`, trigonometric functions, `min`, `max`, `date_duration`, and
case conversion. Change CBL-C lowering to use a normal return value. Keep old
`CBLC-*` wrappers temporarily if generated applications call them directly.

Tests must assert public signatures and behavior, not only generated COBOL
text.

### Phase 3: migrate managed String methods

Rename methods to `assign`, `append`, `to_upper`, and `to_lower`, and remove
status parameters. Test capacity growth and RAII cleanup on normal returns and
exception paths. Keep old `*_string.cblc` entry programs as thin adapters
during the ABI compatibility period.

### Phase 4: add typed failures

Add standard exception classes and error enums before changing math, parsing,
or date APIs. Implement result classes in CBL-C source so they obey the same
constructor, scope, and cleanup rules as user classes. Compatibility adapters
should be the only code translating named errors to old numeric statuses.

The date result is the first implementation of this phase's value-return ABI.
Its error member is currently an integer code with stable documented meanings;
the parser error taxonomy should be promoted to named constants when the
language's enum/constant facility is available, without adding a one-field
error wrapper.

### Phase 5: version and retire the helper ABI

Changing COBOL linkage is an ABI break. Either retain the existing `CBLC-*`
name for a legacy wrapper and introduce a versioned entry such as
`CBLC2-SQRT`, or version the manifest and regenerate all linked helpers
together. The compiler must never combine old callers with new helpers.

Remove wrappers only after tests prove no generated call site references the
old status fields.

## Required tests

For every migrated function, test:

- public signature and return type;
- generated linkage and argument order;
- normal behavior and every documented failure category;
- boundary values and overflow limits;
- consistent behavior across supported COBOL compilers;
- exception propagation and RAII cleanup;
- program abort when another exception occurs during active propagation;
- legacy wrapper equivalence during the compatibility period.

Add a repository-wide check that generated user code does not declare or pass
`CBLC-HELPER-STATUS` for functions whose failure policy is `none`.

## Completion criteria

The migration is complete when no public API exposes an anonymous status
output, total functions return values, managed strings use ordinary methods,
expected failures use typed results, exceptional failures use typed
exceptions, behavior is platform-independent, and old status-based COBOL
programs exist only as deliberate versioned adapters or have been retired.
