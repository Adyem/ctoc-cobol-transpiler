# Developer Onboarding Checklist

This checklist helps new contributors install dependencies, set up the repository, and validate the toolchain before modifying the transpiler.

## Repository Setup
- [ ] Install a C++17-capable compiler toolchain and GNU Make.
- [x] Install the COBOL toolchain (for example, the `gnucobol` package) so `cobc` is available for sample execution tests.
- [ ] Clone the repository with submodules: `git clone --recurse-submodules`.
- [x] If the repository is already cloned, run `make initialize` to sync the `libft` submodule.
- [x] Verify the libft build scripts exist at `libft/Makefile`.
- [x] Consult `docs/platform_bootstrap.md` for platform-specific dependency installation steps on macOS, Linux, or Windows Subsystem for Linux.

## Local Build
- [x] Generate optimized binaries with `make all` and debug builds with `make debug`.
- [x] Confirm artifacts are created in the project root (e.g., `ctoc_cobol_transpiler`).
- [x] Use `make clean` between configuration changes to remove stale objects (requires initialized `libft`).

## Test Execution
- [x] Build the automated test runner with `make tests`.
- [x] Execute `make test` to run the full suite (requires initialized `libft`).
- [x] Confirm `cobc` is installed by the toolchain step above or run `make install_cobc` if the binary is missing before executing COBOL-backed tests.
- [ ] Enable forward CBL-C→COBOL tests by exporting `CTOC_ENABLE_FORWARD_TRANSLATION=1` once file/record lowering support is available; otherwise the suite will skip those scenarios.
- [x] Investigate and resolve any failing tests before committing changes (current suite passes with `make test`).

## Documentation & Workflow
- [ ] Read `docs/contributing.md` for coding standards and review policies.
- [ ] Skim `docs/runtime_api_reference.md` to understand available runtime helpers.
- [x] Review `docs/getting_started.md` for an end-to-end tour of the toolchain and recent feature additions.
- [ ] Update relevant guides when adding new features or modifying workflows.

Keep this checklist updated as the project evolves so newcomers can get productive quickly.
