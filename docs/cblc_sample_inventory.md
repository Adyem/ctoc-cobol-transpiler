# CBL-C Sample Inventory

This document captures the reference CBL-C snippets that exercise the currently defined surface area of the language. Each
sample lives in `samples/cblc` and is registered in `samples/cblc/manifest.txt` so automated checks can ensure the examples stay
in sync with the documentation. Executable statements appear inside named `function` blocks so the transpiler can emit matching
COBOL paragraphs, and every sample includes a `function void main()` entrypoint that invokes the showcased routine.

## Sample Coverage

### `samples/cblc/minimal_program.cblc`
- **Purpose:** Mirrors the COBOL minimal program so the reverse pipeline has a trivially round-trippable example.
- **Constructs:** A single global buffer, `function void MAIN()` entrypoint, literal assignment, and explicit `return;` statement.

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
- **Constructs:** Boolean and buffer declarations recovered from WORKING-STORAGE, uppercase identifier normalization,
  `open`/`close` pairs, negated `while` loops, nested `if`/`else` blocks, `read` statements that capture buffers, and `write`
  calls forwarding the recovered record.

```cblc
bool EOF_FLAG = false;
char OUTPUT_RECORD[1];

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
- **Constructs:** Multiple `function` blocks separated by a single blank line, recovered integer/boolean/buffer declarations
  from WORKING-STORAGE, uppercase identifier emission with collapsed underscores, canonical numeric literals, and normalized
  string literal quoting.

```cblc
int RUNNING_TOTAL_VALUE = 5;
bool STATUS_FLAG = false;
char SCRATCH_NOTE[12] = "raw value";

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
- **Constructs:** Flag and counter declarations reconstructed from WORKING-STORAGE, `if` statements with negated conditions,
  `while` loops sourced from `PERFORM UNTIL` and `PERFORM VARYING`, literal sentinel assignments that mirror COBOL `MOVE`
  updates, numeric output routed through `display`, and trailing `return ;` statements for each recovered function.

```cblc
bool CONTROL_FLAG = false;
int PROGRESS_METER = 0;
int PROGRESS_LIMIT = 10;
int PROGRESS_INDEX = 0;
int OUTPUT_VALUE = 0;

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

### `samples/cblc/reverse_numeric_scalars.cblc`
- **Purpose:** Demonstrates how the reverse emitter now lifts decimal `PIC S9V9(n)` declarations into floating-point CBL-C
  variables while preserving wider precision as `double` and promotes single-character status indicators to booleans.
- **Constructs:** Mixed numeric declarations including recovered `float`/`double` scalars, integer counters, suffix-based
  boolean detection (`STATUS-IND`), literal `MOVE` statements converted into assignments, and canonical paragraph emission with
  trailing `return ;`.

```cblc
float FLOAT_APPROX = 0;
double DOUBLE_APPROX = 0;
int BINARY_COUNT = 0;
int PACKED_TOTAL = 0;
bool STATUS_IND = false;

function void MAIN() {
    FLOAT_APPROX = 1;
    DOUBLE_APPROX = 100;
    BINARY_COUNT = 10;
    PACKED_TOTAL = 42;
    STATUS_IND = true;
    return ;
}
```

### `samples/cblc/reverse_group_items.cblc`
- **Purpose:** Locks down recovery of COBOL level 01 group items by asserting that the reverse emitter produces a `record`
  declaration, mirrors the group variable, and still surfaces each subordinate field as an addressable scalar for existing
  procedure logic.
- **Constructs:** A generated `record` with alphanumeric and numeric members, the mirrored group variable, standalone scalars
  (including a retained VALUE clause initializer), and assignments that populate the recovered fields inside `function void
  MAIN()`.

### `samples/cblc/reverse_value_defaults.cblc`
- **Purpose:** Highlights how VALUE clauses on supported WORKING-STORAGE scalars become direct initializers in the generated
  CBL-C so default state matches the COBOL source.
- **Constructs:** Boolean flag initialization recovered from a single-character literal, integer and floating-point defaults,
  alphanumeric buffer expansion with a string initializer, and the empty `function void MAIN()` stub emitted for passive
  storage declarations.

```cblc
bool START_FLAG = true;
int CUSTOMER_COUNT = 12;
float DISCOUNT_RATE = 1.25;
char STATUS_MESSAGE[12] = "READY";

function void MAIN() {
    return ;
}
```

### `samples/cblc/reverse_copybook.cblc`
- **Purpose:** Demonstrates how the reverse emitter expands registered `COPY` books and keeps single-character codes as scalars
  while preserving buffer-style identifiers as arrays.
- **Constructs:** Leading comment for the originating `COPY` directive, boolean and character scalars, fixed-length buffers,
  numeric and floating-point fields, and an empty `function void MAIN()` body that mirrors the COBOL stub.

```cblc
/* COPY CUSTOMER-STATUS */
bool CUSTOMER_FLAG;
char CUSTOMER_CODE;
char CUSTOMER_NAME[32];
int CUSTOMER_RATING;
const double CUSTOMER_BALANCE;
char STATE_CODE = "A";
char BUFFER_NAME[8];

function void MAIN() {
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

### `samples/cblc/abs_magnitude_report.cblc`
- **Purpose:** Demonstrates using the `ABS` unary operator to normalize both integral and floating-point deltas before reporting inventory metrics.
- **Constructs:** Global integer and double storage, arithmetic subtraction, unary `ABS` applications, string and numeric `display` statements, and a helper invoked from `main`.

```cblc
int inbound_units;
int outbound_units;
int net_position;
int net_magnitude;

double revenue_delta;
double revenue_magnitude;

function void analyze_inventory() {
    inbound_units = 145;
    outbound_units = 172;
    net_position = inbound_units - outbound_units;
    net_magnitude = ABS net_position;

    revenue_delta = -12.375;
    revenue_magnitude = ABS revenue_delta;

    display("NET POSITION:");
    display(net_position);
    display("NET MAGNITUDE:");
    display(net_magnitude);
    display("REVENUE MAGNITUDE:");
    display(revenue_magnitude);
}

function void main() {
    analyze_inventory();
}
```

### `samples/cblc/multi_module_main.cblc`
- **Purpose:** Demonstrates splitting a program across multiple translation units where the entrypoint resides in one file and helper routines live alongside it.
- **Constructs:** Global integers shared within a unit, helper functions invoked from `main`, cross-file calls to external routines, arithmetic updates, and console output of computed values.

```cblc
import "multi_module_worker.cblc";

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


### `samples/cblc/project_scheduler/project_scheduler_loader.cblc`
- **Purpose:** Seeds the project scheduler backlog with string-based timing metadata so other modules can consume normalized numeric values.
- **Constructs:** Standard library calls (`std::strcpy`, `std::atoi`, `std::strcmp`), local character buffers, integer accumulation, conditional adjustments, and a non-void return that surfaces the computed total minutes to callers.

```cblc
function int load_backlog_minutes()
{
    char first_minutes[8];
    char second_minutes[8];
    char third_minutes[8];
    char phase_label[16];
    int total;
    int parsed;

    std::strcpy(first_minutes, "120");
    std::strcpy(second_minutes, "090");
    std::strcpy(third_minutes, "045");
    std::strcpy(phase_label, "ASSEMBLY");

    parsed = std::atoi(first_minutes);
    total = parsed;

    parsed = std::atoi(second_minutes);
    total = total + parsed;

    parsed = std::atoi(third_minutes);
    total = total + parsed;

    if (std::strcmp(phase_label, "ASSEMBLY") == 0)
    {
        total = total + 15;
    }

    return (total);
}
```

### `samples/cblc/project_scheduler/project_scheduler_metrics.cblc`
- **Purpose:** Converts the backlog duration into a floating-point prioritization score that blends elapsed minutes with descriptive string length analysis.
- **Constructs:** Standard library calls (`std::strcpy`, `std::strlen`, `std::fabs`, `std::sqrt`), intermediate double precision scalars, and a non-void helper that other modules reuse.

```cblc
function double compute_priority_score(int total_minutes)
{
    double minutes;
    double baseline;
    double offset;
    double root;
    char phase_label[32];
    int label_length;
    double score;

    minutes = total_minutes;
    baseline = 300.0;

    std::strcpy(phase_label, "Consolidated Review");
    label_length = std::strlen(phase_label);

    offset = std::fabs(minutes - baseline);
    root = std::sqrt(minutes);
    score = offset + label_length + root;

    return (score);
}
```

### `samples/cblc/project_scheduler/project_scheduler_presenter.cblc`
- **Purpose:** Coordinates the loader and metrics helpers to build a presentation string and emit consolidated scheduling diagnostics.
- **Constructs:** Module imports, shared global buffers, standard library usage (`std::strcpy`, `std::strcat`, `std::strlen`, `std::strcmp`), conditional suffix logic, and chained helper invocations.

```cblc
import "project_scheduler_loader.cblc";
import "project_scheduler_metrics.cblc";

char summary[256];
char headline[32];
char detail[32];
char stage[32];
int guard;
int stage_length;
int total_minutes;
double score;

function void present_schedule()
{
    std::strcpy(headline, "Draft Roadmap");
    std::strcpy(detail, "Assemble Budget");
    std::strcpy(stage, "Schedule Review");

    total_minutes = load_backlog_minutes();
    score = compute_priority_score(total_minutes);

    summary = "SCHEDULE SUMMARY:";
    std::strcat(summary, " ");
    std::strcat(summary, headline);
    std::strcat(summary, " / ");
    std::strcat(summary, detail);
    std::strcat(summary, " / ");
    std::strcat(summary, stage);

    stage_length = std::strlen(stage);
    if (stage_length > 12)
    {
        std::strcat(summary, "!");
    }
    else
    {
        std::strcat(summary, ".");
    }

    guard = std::strcmp(detail, "Assemble Budget");
    if (guard == 0)
    {
        std::strcat(summary, " READY");
    }

    display(summary);
    display(total_minutes);
    display(score);
}
```

### `samples/cblc/project_scheduler/project_scheduler_main.cblc`
- **Purpose:** Supplies the entrypoint that stitches the presenter module into the multi-file call chain so the CLI can emit every translation unit in one run.
- **Constructs:** Module import for the presenter helper and a minimal `main` entrypoint that delegates to `present_schedule`.

```cblc
import "project_scheduler_presenter.cblc";

function void main()
{
    present_schedule();
}
```

### `samples/cblc/floating_point_mix.cblc`
- **Purpose:** Showcases float and double scalars working together with boolean flags to document mixed precision arithmetic.
- **Constructs:** Global floating-point declarations, helper function performing addition and comparisons, boolean toggles, and console output of intermediate values.

```cblc
float seasonal_average;
float current_reading;
double yearly_projection;
double combined_projection;
bool trending_up;

function void analyze_readings() {
    seasonal_average = 21.5;
    current_reading = 24.0;
    yearly_projection = 18.75;
    combined_projection = 0.0;
    trending_up = false;

    combined_projection = yearly_projection + current_reading;
    if (current_reading > seasonal_average) {
        trending_up = true;
    }

    if (trending_up) {
        display("TREND UP");
    } else {
        display("TREND FLAT");
    }

    display(seasonal_average);
    display(current_reading);
    display(combined_projection);
}

function void main() {
    analyze_readings();
}
```

### `samples/cblc/mixed_numeric_types.cblc`
- **Purpose:** Demonstrates int, long, and long long scalars alongside a boolean sentinel for overflow-style thresholds.
- **Constructs:** Global integer declarations spanning multiple widths, cumulative arithmetic, relational checks, ternary-style branch pairs, and value logging.

```cblc
int order_count;
long regional_total;
long long national_total;
bool exceeded_limit;

function void track_totals() {
    order_count = 45;
    regional_total = 250000;
    national_total = 5000000000;
    exceeded_limit = false;

    national_total = national_total + regional_total;
    if (national_total > 5250000000) {
        exceeded_limit = true;
    }

    if (exceeded_limit) {
        display("LIMIT EXCEEDED");
    } else {
        display("LIMIT OK");
    }

    display(order_count);
    display(regional_total);
    display(national_total);
}

function void main() {
    track_totals();
}
```

### `samples/cblc/textual_priority_mix.cblc`
- **Purpose:** Combines character buffers, single-character flags, integers, and booleans to highlight textual state paired with numeric counters.
- **Constructs:** Global char array and scalar declarations, nested conditional branches updating a boolean, and sequential displays of string, character, and integer values.

```cblc
char warehouse_code[8];
char priority_level;
int pending_orders;
bool expedite;

function void report_schedule() {
    warehouse_code = "NW-01";
    priority_level = 'B';
    pending_orders = 18;
    expedite = false;

    if (priority_level == 'A') {
        expedite = true;
    } else {
        if (pending_orders > 20) {
            expedite = true;
        }
    }

    if (expedite) {
        display("EXPEDITE");
    } else {
        display("SCHEDULED");
    }

    display(warehouse_code);
    display(priority_level);
    display(pending_orders);
}

function void main() {
    report_schedule();
}
```

### `samples/example_project/hello_make_demo.cblc`
- **Purpose:** Provides a minimal end-to-end example that feeds into the standalone sample makefile, ensuring the quick start flow has a self-contained entrypoint.
- **Constructs:** Global character buffer, a `main` routine that assigns a literal, emits a `display` call, and terminates with an explicit `return;`.

```cblc
char greeting[32];

function void main() {
    greeting = "HELLO FROM CBL-C";
    display(greeting);
    return;
}
```

## Maintenance Checklist

1. Add a new `.cblc` file under `samples/cblc` when introducing language features that need sample coverage. (The hello make demo lives alongside its makefile in `samples/example_project`.)
2. Append the file path to `samples/cblc/manifest.txt` so tooling and tests can locate the new example.
3. Update this document with a short description and construct summary so downstream contributors understand what the sample is
   intended to cover.
