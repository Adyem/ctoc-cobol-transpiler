# Feature Showcase Project

This sample expands on the minimal hello world demo by walking through a
slightly longer sequence of string updates alongside numeric outputs. It
highlights the current CBL-C to COBOL support for declaring multiple
buffers, reassigning them, surfacing numeric literals, performing
arithmetic with `+`, `-`, `*`, and `/`, and displaying both literal
messages and stored values.

## Layout

- `Makefile` — mirrors the smaller example project but targets the
  `message_showcase` source in this directory. It emits the transpiled
  COBOL program and compiled executable directly in the project root so
  everything stays easy to inspect.
- `message_showcase.cblc` — drives the scenario. It stages several
  headline and detail strings, reassigns them, derives numeric
  calculations with each arithmetic operator, and displays the evolving
  state as the walkthrough progresses.
- `message_showcase.cob` — COBOL emitted by the transpiler during the
  build. The file is regenerated on each run.
- `message_showcase` — executable built from the generated COBOL when
  `make` runs.

## Scenario Overview

1. Initialize the headline and supporting buffers with literal messages.
2. Reassign the buffers to simulate multi-step progress, including a
   copy from one buffer into another to demonstrate identifier-based
   assignments.
3. Surface numeric values calculated via addition, subtraction,
   multiplication, and division.
4. Display both literal strings and stored values to show how the
   transpiled COBOL mirrors the CBL-C control flow.

## Usage

1. Run `make transpile` to regenerate the COBOL source.
2. Run `make` (or `make compile`) to produce the executable.
3. Run `make run` to execute the program and view the staged output.
4. Run `make clean` to delete generated artifacts and reset the
   directory.

The makefile uses the same helper targets as the smaller example to
ensure the transpiler is compiled, `cobc` is available, and `libft` is
initialized before building the showcase.
