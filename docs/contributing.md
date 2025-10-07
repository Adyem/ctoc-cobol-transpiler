# Contributor Guide

This guide explains how to work on the COBOL↔CBL-C transpiler and the expectations for code style, testing, and project hygiene.

## Getting Started

- Clone this repository and initialize the `libft` submodule before building: `git submodule update --init`.
- Build and run the full test suite with `make test`. The harness builds every component and executes all runtime and documentation checks.
- Use the feature tracker (`compiler_feature_tracker.md`) to identify the next task to implement. Each change should advance one of the pending items and mark it as completed once the work ships.

## Coding Standards

All code must follow the guidelines enforced across the project:

- Write C++ using 4-space indentation and Allman-style braces.
- Avoid `for` loops, ternary operators, and `switch` statements.
- Use descriptive `snake_case` names for functions and variables.
- Return from void functions with `return ;` and from non-void functions with `return (value);`.
- Keep module headers focused on declarations and place implementations in corresponding `.cpp` files.
- When adding classes, declare constructors and destructors explicitly and track errors through the `_error_code` pattern shared across the codebase.

Refer to `AGENTS.md` at the repository root for the authoritative checklist. New contributors should review the file before proposing changes.

## Testing Expectations

- Every feature must include tests. Extend the existing suites under `tests/` or create new ones when necessary.
- Prefer deterministic, hermetic tests that operate on fixtures committed to the repository.
- When a feature involves documentation, add a documentation test that confirms the guide exists and references the critical concepts.

## Documentation Workflow

- Place user-facing documentation in `docs/` and samples under `samples/`.
- Cross-link design documents when introducing new guides so the navigation remains discoverable.
- Update manifests or inventories when adding samples, fixtures, or runtime modules so downstream tooling remains synchronized.

Following these guidelines keeps the codebase consistent and reduces friction for future contributions.
