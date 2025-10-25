# Development environment setup

This guide documents repeatable steps for configuring the COBOL transpiler development environment on Linux, macOS, and Windows Subsystem for Linux (WSL).

## Shared prerequisites

- A C++17 toolchain (GCC 11+ or Clang 14+).
- CMake 3.20 or newer.
- Python 3.8 or newer for helper scripts.
- `ninja` is recommended for faster incremental builds, though the project also supports Make.
- `pipx` or `pip` (optional) for installing `pre-commit` when using the provided hooks.

### Repository checkout

1. Clone the repository and initialize submodules:
   ```bash
   git clone https://github.com/example/ctoc-cobol-transpiler.git
   cd ctoc-cobol-transpiler
   git submodule update --init --recursive
   ```
2. Install the git blame ignore configuration (optional):
   ```bash
   git config blame.ignoreRevsFile .git-blame-ignore-revs
   ```
3. Install the optional `pre-commit` hooks:
   ```bash
   pipx install pre-commit  # or: pip install --user pre-commit
   pre-commit install
   ```

## Linux

1. Install toolchain dependencies using your distribution's package manager. For Debian-based systems:
   ```bash
   sudo apt update
   sudo apt install build-essential clang cmake ninja-build python3 python3-pip
   ```
2. Configure and build:
   ```bash
   cmake -S . -B build/linux -G Ninja
   cmake --build build/linux
   ```
3. Run the regression suite:
   ```bash
   cmake --build build/linux --target test
   ```

## macOS

1. Install Xcode command-line tools:
   ```bash
   xcode-select --install
   ```
2. Install Homebrew if not already present, then the dependencies:
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   brew install cmake ninja python pre-commit
   ```
3. Configure and build with Ninja:
   ```bash
   cmake -S . -B build/macos -G Ninja
   cmake --build build/macos
   ```
4. Execute the tests:
   ```bash
   cmake --build build/macos --target test
   ```

## Windows Subsystem for Linux (WSL)

1. Enable WSL and install Ubuntu following the official Microsoft documentation.
2. Within the Ubuntu shell, install dependencies:
   ```bash
   sudo apt update
   sudo apt install build-essential clang cmake ninja-build python3 python3-pip
   ```
3. Clone the repository inside the WSL filesystem (e.g., within `~/code`) to avoid cross-filesystem performance issues.
4. Configure and build:
   ```bash
   cmake -S . -B build/wsl -G Ninja
   cmake --build build/wsl
   ```
5. Run the regression suite:
   ```bash
   cmake --build build/wsl --target test
   ```

## Additional tips

- Invoke `make format` or the `pre-commit` hook before sending patches to ensure formatting consistency.
- Use `python3 scripts/update_codeowners.py` after editing `scripts/codeowners_manifest.json` to regenerate `.github/CODEOWNERS`.
- The provided build directories (`build/linux`, `build/macos`, `build/wsl`) are suggestions; feel free to adapt them to your workflow.
