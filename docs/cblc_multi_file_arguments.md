# Multi-File Argument Passing in CBL-C

The transpiler's module system lets you split a CBL-C program across several
translation units and wire them together with `import` directives.  Each
translation unit exposes any `function` it defines, so a caller can pass
arguments into helper routines implemented in a different source file.  The
examples below illustrate common patterns and focus on the way arguments move
through those boundaries.

Each listing shows the file name on the first line so you can drop the snippet
into `samples/cblc/` (or your own project hierarchy) and experiment with the
call flow. The examples below highlight the argument passing semantics while
the CLI exposes the reverse (COBOL → CBL-C) pipeline. The forward command will
report a diagnostic until the code generator is wired in, but the intended
invocation will eventually resemble:

```
ctoc_cobol_transpiler --direction cblc-to-cobol \
  --input metrics_main.cblc --output build/metrics_main.cob \
  --input metrics_worker.cblc --output build/metrics_worker.cob
```

## Example 1 — Numeric Totals with Cross-File Helpers

`metrics_main.cblc`

```cblc
import "metrics_worker.cblc";

int running_total;

function void main() {
    running_total = 0;

    running_total = add_sale(running_total, 42);
    running_total = add_sale(running_total, 18);

    display(running_total);
}
```

`metrics_worker.cblc`

```cblc
function int add_sale(int current_total, int delta) {
    int updated_total;

    updated_total = current_total + delta;
    return updated_total;
}
```

**What it shows:**

* The main translation unit imports a helper file so the compiler sees
  `add_sale` when it type-checks `main`.
* The helper accepts the caller's total and the sale amount as parameters,
  computes the new total, and returns it to the caller.
* The argument values cross the file boundary exactly the way they would inside
  a single translation unit; no extra plumbing is required.

## Example 2 — Boolean Decisions Routed Through a Worker Module

`inventory_main.cblc`

```cblc
import "inventory_rules.cblc";

int on_hand;
bool restock_flag;

function void main() {
    on_hand = 120;

    restock_flag = should_restock(on_hand, 150);
    report_status(restock_flag);
}
```

`inventory_rules.cblc`

```cblc
function bool should_restock(int quantity, int target) {
    bool needs_order;

    if (quantity < target) {
        needs_order = true;
    } else {
        needs_order = false;
    }
    return needs_order;
}

function void report_status(bool flag) {
    if (flag) {
        display("REORDER REQUESTED");
    } else {
        display("STOCK ADEQUATE");
    }
}
```

**What it shows:**

* Multiple helpers live in the worker file and both accept arguments from the
  main module.
* A boolean return value communicates the decision back to the caller, while a
  `void` helper consumes the flag as an argument and performs presentation logic.
* The entire decision pipeline resides outside the entrypoint file, making it
  easy to reuse the rules from other programs by importing the same worker.

These patterns scale to larger programs: translate every reusable helper into a
separate module, declare the parameters it expects, and `import` that module in
any translation unit that needs access to its subfunctions.  Keep helper
functions `private` when they should only be invoked inside their home module;
calls from other translation units now trigger a
`TRANSPILE_ERROR_FUNCTION_PRIVATE_ACCESS` diagnostic so you can tighten the
module boundary before shipping.
