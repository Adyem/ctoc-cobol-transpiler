# Platform Bootstrapping Guide

This guide walks through preparing a working development environment for the CTOC COBOL Transpiler on macOS, Linux, and Windows Subsystem for Linux (WSL). Follow the steps for your operating system before running the build or test workflows highlighted in the rest of the documentation.

## Shared Expectations

All platforms require the following components:

- A modern C++ toolchain capable of compiling C++17 code (`clang` or `g++`).
- GNU Make for driving the provided build scripts.
- GnuCOBOL so the test suites can exercise COBOL artifacts with `cobc`.
- Git with submodule support to fetch `libft` alongside this repository.

After installing the prerequisites, clone the repository with submodules and run the bootstrap target once per machine:

```bash
git clone --recurse-submodules https://example.com/ctoc-cobol-transpiler.git
cd ctoc-cobol-transpiler
make initialize
# optional sanity check once builds are ready
make test
```

`make initialize` builds the bundled `libft` dependency and verifies the submodule is ready for subsequent compilation steps.

Running `make test` immediately after initialization confirms the host toolchain can compile the harmonized standard-library helpers that now share trailing status codes. If the suite fails while linking COBOL fixtures, revisit the package installation steps below before continuing development.

## macOS (Homebrew)

1. Install [Homebrew](https://brew.sh/) if it is not already available on the system.
2. Install the toolchain and supporting utilities:
   ```bash
   brew update
   brew install llvm make gnucobol cmake git
   ```
3. Ensure the Homebrew LLVM binaries precede the system toolchain so `clang++` resolves to the C++17-capable compiler:
   ```bash
   echo 'export PATH="/opt/homebrew/opt/llvm/bin:$PATH"' >> ~/.zprofile
   echo 'export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"' >> ~/.zprofile
   echo 'export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"' >> ~/.zprofile
   source ~/.zprofile
   ```
4. Confirm the installed tools report expected versions:
   ```bash
   clang++ --version
   cobc --version
   make --version
   ```
5. Run the shared bootstrap commands listed above to fetch the repository and initialize `libft`.

## Linux (Debian/Ubuntu)

1. Refresh the package index and upgrade existing packages:
   ```bash
   sudo apt update
   sudo apt upgrade
   ```
2. Install the required toolchain, build helpers, and COBOL runtime:
   ```bash
   sudo apt install build-essential clang gnucobol make cmake git pkg-config
   ```
3. Verify the tool versions:
   ```bash
   clang++ --version
   cobc --version
   make --version
   ```
4. Clone the repository with submodules and run `make initialize` as shown in the shared expectations section.

## Windows Subsystem for Linux (WSL)

1. Enable WSL and install an Ubuntu distribution if one is not already present:
   ```powershell
   wsl --install -d Ubuntu
   ```
   Restart the machine when prompted and create a UNIX user for the distribution.
2. Inside the WSL shell, update packages and install the required toolchain (the commands mirror the Linux section):
   ```bash
   sudo apt update
   sudo apt upgrade
   sudo apt install build-essential clang gnucobol make cmake git pkg-config
   ```
3. If the COBOL packages are unavailable in your distribution channel, install GnuCOBOL from source by following the [official instructions](https://gnucobol.sourceforge.io/), then rerun `sudo ldconfig` inside WSL to register the libraries.
4. Validate the toolchain from the WSL environment:
   ```bash
   clang++ --version
   cobc --version
   make --version
   ```
5. Clone the repository using `git clone --recurse-submodules` and run `make initialize` within WSL to build `libft` using the Linux instructions above.

## Troubleshooting Tips

- If `clang++` still resolves to the system compiler on macOS after updating the PATH, verify the `PATH` export lines were appended to the correct shell profile (`~/.zprofile` for Zsh, `~/.bash_profile` for Bash) and open a new terminal.
- When `cobc` is not found, confirm the GnuCOBOL installation succeeded and that its binary directory is on your PATH. Most package managers handle this automatically, but manual builds may require updating `PATH`.
- For WSL installations, ensure Windows file sharing is disabled for the repository directory or use a location inside the WSL filesystem (e.g., under `~/projects`) to avoid performance penalties when building.

Once the prerequisites are satisfied, proceed with the standard workflows in `docs/getting_started.md` and the onboarding checklist.
