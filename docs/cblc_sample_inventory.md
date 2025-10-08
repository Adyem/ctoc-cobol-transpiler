# CBL-C Sample Inventory

This document captures the reference CBL-C snippets that exercise the currently defined surface area of the language. Each
sample lives in `samples/cblc` and is registered in `samples/cblc/manifest.txt` so automated checks can ensure the examples stay
in sync with the documentation. Executable statements appear inside named `function` blocks so the transpiler can emit matching
COBOL paragraphs, and every sample includes a `function void main()` entrypoint that invokes the showcased routine.

## Sample Coverage

### `samples/cblc/copy_file.cblc`
- **Purpose:** Demonstrates the baseline file copy loop used throughout the design doc and ensures the runtime string buffer path
  is represented in fixtures.
- **Constructs:** `file` declarations, scalar `char` buffers, `function` definitions (`process_file`, `main`), `open`/`close`
  pairs, `while` loops, `read`/`write` built-ins, and string literals.

```cblc
file in "input.txt";
file out "output.txt";
char line[256];

function void process_file() {
    open(in, "r");
    open(out, "w");
    while (read(in, line)) {
        write(out, line);
    }
    close(in);
    close(out);
}

function void main() {
    process_file();
}
```

### `samples/cblc/filter_prefix.cblc`
- **Purpose:** Captures filtering logic against line records to highlight conditional evaluation inside iterative file
  processing.
- **Constructs:** `function` definitions (`filter_prefix`, `main`), `if` statements nested within loops, `starts_with` string
  predicate, and reuse of the `read`/`write` built-ins.

```cblc
file in "input.txt";
file out "filtered.txt";
char line[128];

function void filter_prefix() {
    open(in, "r");
    open(out, "w");
    while (read(in, line)) {
        if (starts_with(line, "ERR")) {
            write(out, line);
        }
    }
    close(in);
    close(out);
}

function void main() {
    filter_prefix();
}
```

### `samples/cblc/record_writer.cblc`
- **Purpose:** Provides a minimal record declaration coupled with scalar assignments so the COBOL generator can verify DATA
  DIVISION layout requirements.
- **Constructs:** `record` blocks with nested field declarations, `function` definitions (`write_records`, `main`), scalar
  variables instantiated from record types, string assignments to record members, and `write` operations for structured records.

```cblc
record Person {
    char name[40];
    char id[10];
};

file people "people.dat";
Person person;

function void write_records() {
    person.name = "ALICE";
    person.id = "0001";

    open(people, "w");
    write(people, person);
    close(people);
}

function void main() {
    write_records();
}
```

### `samples/cblc/record_summary.cblc`
- **Purpose:** Mirrors the COBOL record summarization flow so the reverse pipeline can validate counter and accumulator recovery.
- **Constructs:** `record` types with scalar members, integer state tracked in global variables, `while` loops over `read` calls,
  and conditional aggregation guarded by `starts_with`.

```cblc
record RecordEntry {
    char status[2];
    int amount;
};

file input "records.dat";
RecordEntry entry;
int accepted_count;
int total_amount;

function void summarize_records() {
    accepted_count = 0;
    total_amount = 0;

    open(input, "r");
    while (read(input, entry)) {
        if (starts_with(entry.status, "A")) {
            accepted_count = accepted_count + 1;
            total_amount = total_amount + entry.amount;
        }
    }
    close(input);
}

function void main() {
    summarize_records();
}
```

### `samples/cblc/integration_showcase.cblc`
- **Purpose:** Exercises the current end-to-end feature surface by combining file I/O, record aggregation, conditional branching,
  and cross-function orchestration inside a single program.
- **Constructs:** `record` declarations, multiple `file` handles, scalar counters, helper functions (`reset_state`,
  `process_transactions`, `main`), `open`/`close` pairs, `while` loops, `if` statements, arithmetic updates, `write` calls, and
  `display` of aggregate values.

```cblc
record Transaction {
    char status[2];
    int amount;
};

file transactions "transactions.dat";
file accepted_log "accepted.txt";
file rejected_log "rejected.txt";

Transaction current;
char accepted_marker[32];
char rejected_marker[32];
int accepted_count;
int rejected_count;
int total_amount;

function void reset_state() {
    accepted_count = 0;
    rejected_count = 0;
    total_amount = 0;
    accepted_marker = "ACCEPTED ENTRY";
    rejected_marker = "REJECTED ENTRY";
}

function void process_transactions() {
    reset_state();
    open(transactions, "r");
    open(accepted_log, "w");
    open(rejected_log, "w");
    while (read(transactions, current)) {
        if (starts_with(current.status, "A")) {
            accepted_count = accepted_count + 1;
            total_amount = total_amount + current.amount;
            write(accepted_log, accepted_marker);
        } else {
            rejected_count = rejected_count + 1;
            write(rejected_log, rejected_marker);
        }
    }
    close(transactions);
    close(accepted_log);
    close(rejected_log);
}

function void main() {
    display("INTEGRATION SHOWCASE");
    process_transactions();
    display(accepted_count);
    display(rejected_count);
    display(total_amount);
}
```

### `samples/cblc/reverse_constructs.cblc`
- **Purpose:** Serves as the golden CBL-C output for the reverse emitter integration test that exercises COBOL control flow and
  file I/O recovery.
- **Constructs:** Uppercase identifier normalization, `open`/`close` pairs, negated `while` loops, nested `if`/`else` blocks,
  `read` statements that capture buffers, and `write` calls forwarding the recovered record.

```cblc
function void MAIN() {
    open(INPUT_FILE, "r");
    while (!(EOF_FLAG == true)) {
        read(INPUT_FILE, OUTPUT_RECORD);
        if (EOF_FLAG == false) {
            write(OUTPUT_FILE, OUTPUT_RECORD);
        } else {
            EOF_FLAG = true;
        }
    }
    close(INPUT_FILE);
    return ;
}
```

### `samples/cblc/reverse_normalization.cblc`
- **Purpose:** Captures the expected normalized output when COBOL paragraphs include lowercase identifiers, leading-zero
  numerics, and inconsistent spacing.
- **Constructs:** Multiple `function` blocks separated by a single blank line, uppercase identifier emission with collapsed
  underscores, canonical numeric literals, and normalized string literal quoting.

```cblc
function void ENTRY_PARAGRAPH() {
    SCRATCH_NOTE = "mixED Case value";
    RUNNING_TOTAL_VALUE = 0;
    STATUS_FLAG = true;
    return ;
}

function void NORMALIZE_VALUES() {
    RUNNING_TOTAL_VALUE = 7;
    SCRATCH_NOTE = "done";
    return ;
}
```

### `samples/cblc/reverse_control_flow.cblc`
- **Purpose:** Locks down the recovered CBL-C for nested conditionals and loops so the reverse emitter preserves complex
  control-flow structure.
- **Constructs:** `if` statements with negated conditions, `while` loops sourced from `PERFORM UNTIL` and `PERFORM VARYING`,
  literal sentinel assignments that mirror COBOL `MOVE` updates, numeric output routed through `display`, and trailing
  `return ;` statements for each recovered function.

```cblc
function void MAIN() {
    if (!(CONTROL_FLAG == true)) {
        while (!(PROGRESS_METER > PROGRESS_LIMIT)) {
            PROGRESS_METER = 11;
        }
        OUTPUT_VALUE = PROGRESS_METER;
    } else {
        PROGRESS_INDEX = 0;
        while (!(PROGRESS_INDEX >= PROGRESS_LIMIT)) {
            OUTPUT_VALUE = PROGRESS_INDEX;
            PROGRESS_INDEX++;
        }
    }
    display(OUTPUT_VALUE);
    return ;
}

function void NEXT_PARAGRAPH() {
    CONTROL_FLAG = true;
    return ;
}
```

### `samples/cblc/return_numeric.cblc`
- **Purpose:** Demonstrates scalar functions that return values through the trailing BY REFERENCE slot with integer arithmetic.
- **Constructs:** Global integer declarations, a value-returning helper, `return` statements with expressions, and a `function void main()` caller that captures the result.

```cblc
int addend_a;
int addend_b;
int sum_result;

function int compute_sum() {
    sum_result = addend_a + addend_b;
    return sum_result;
}

function void main() {
    addend_a = 12;
    addend_b = 30;
    sum_result = compute_sum();
    display(sum_result);
}
```

### `samples/cblc/return_boolean.cblc`
- **Purpose:** Captures boolean return semantics with conditional `return` paths so diagnostics can validate the trailing slot wiring.
- **Constructs:** Integer and boolean globals, modulo arithmetic, comparisons, boolean literals, and a `function void main()` consumer that branches on the returned value.

```cblc
int candidate_value;
int remainder_value;
bool is_even_result;

function bool is_even() {
    remainder_value = candidate_value % 2;
    if (remainder_value == 0) {
        return true;
    }
    return false;
}

function void main() {
    candidate_value = 9;
    is_even_result = is_even();
    if (is_even_result) {
        display("EVEN");
    } else {
        display("ODD");
    }
}
```

### `samples/cblc/return_character.cblc`
- **Purpose:** Showcases character return values forwarded through the trailing argument so callers can reuse the same storage.
- **Constructs:** Global character state, literal character assignments, function returns in expression contexts, and console output of the captured value.

```cblc
char current_grade;

function char fetch_grade() {
    return current_grade;
}

function void main() {
    current_grade = 'A';
    current_grade = fetch_grade();
    display(current_grade);
}
```

### `samples/cblc/numeric_precision.cblc`
- **Purpose:** Locks down widened arithmetic and comparison behavior across long, long long, float, and double operands so numeric helper coverage stays in sync with the golden fixtures.
- **Constructs:** Global accumulators, block-scoped temporaries, mixed-width arithmetic (`+`, `-`, `*`), relational comparisons (`>`, `>=`, `==`, `!=`), and console output that highlights each decision point.

```cblc
long day_total;
long long year_total;
long long threshold;
float day_ratio;
double combined_ratio;

function void analyze_precision() {
    long deposit;
    long withdrawal;
    long long bonus_pool;
    float seasonal_rate;
    double base_rate;

    deposit = 125000;
    withdrawal = 50000;
    bonus_pool = 4000000000;
    seasonal_rate = 1.25;
    base_rate = 2.5;

    day_total = deposit - withdrawal;
    year_total = bonus_pool + day_total;
    threshold = 5000000000;
    day_ratio = seasonal_rate * 2.0;
    combined_ratio = base_rate + day_ratio;

    if (year_total >= threshold) {
        display("YEAR ABOVE");
    } else {
        display("YEAR BELOW");
    }

    if (combined_ratio > base_rate) {
        display("RATE INCREASED");
    }

    if (day_ratio != seasonal_rate) {
        display("FLOAT SHIFT");
    }

    if (year_total > bonus_pool) {
        display("BONUS REACHED");
    }

    if (day_total == 75000) {
        display("DAY TARGET");
    }
}
```

### `samples/cblc/multi_module_main.cblc`
- **Purpose:** Demonstrates splitting a program across multiple translation units where the entrypoint resides in one file and helper routines live alongside it.
- **Constructs:** Global integers shared within a unit, helper functions invoked from `main`, cross-file calls to external routines, arithmetic updates, and console output of computed values.

```cblc
int accumulator;

function void add_once() {
    accumulator = accumulator + 1;
}

function void main() {
    accumulator = 0;
    show_banner();
    add_once();
    display(accumulator);
}
```

### `samples/cblc/multi_module_worker.cblc`
- **Purpose:** Provides the companion helper module for the multi-file workflow so the CLI can transpile several source units in one invocation.
- **Constructs:** Standalone helper function definitions, string literals, and console output emitted from a non-entry translation unit.

```cblc
function void show_banner() {
    display("WORKER READY");
}
```


## Maintenance Checklist

1. Add a new `.cblc` file under `samples/cblc` when introducing language features that need sample coverage.
2. Append the file path to `samples/cblc/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
