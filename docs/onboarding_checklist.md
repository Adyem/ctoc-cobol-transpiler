# Developer Onboarding Checklist

This checklist helps new contributors install dependencies, set up the repository, and validate the toolchain before modifying the transpiler.

## Repository Setup
- [ ] Install a C++17-capable compiler toolchain and GNU Make.
- [ ] Install the COBOL toolchain (for example, the `gnucobol` package) so `cobc` is available for sample execution tests.
- [ ] Clone the repository with submodules: `git clone --recurse-submodules`.
- [ ] If the repository is already cloned, run `make initialize` to sync the `libft` submodule.
- [ ] Verify the libft build scripts exist at `libft/Makefile`.

## Local Build
- [ ] Generate optimized binaries with `make all` and debug builds with `make debug`.
- [ ] Confirm artifacts are created in the project root (e.g., `ctoc_cobol_transpiler`).
- [ ] Use `make clean` between configuration changes to remove stale objects.

## Test Execution
- [ ] Build the automated test runner with `make tests`.
- [ ] Execute `make test` to run the full suite (requires initialized `libft`).
- [ ] Confirm `cobc` is installed by the toolchain step above or run `make install_cobc` if the binary is missing before executing COBOL-backed tests.
- [ ] Investigate and resolve any failing tests before committing changes.

## Documentation & Workflow
- [ ] Read `docs/contributing.md` for coding standards and review policies.
- [ ] Skim `docs/runtime_api_reference.md` to understand available runtime helpers.
- [ ] Update relevant guides when adding new features or modifying workflows.

Keep this checklist updated as the project evolves so newcomers can get productive quickly.
