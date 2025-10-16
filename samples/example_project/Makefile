REPO_ROOT := $(abspath ../..)
ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
else
EXE_EXT :=
endif

TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)
COBC ?= cobc

SETUP_SENTINEL := .environment_ready
PROGRAM_NAME := hello_make_demo
CBLC_SRC := $(abspath $(PROGRAM_NAME).cblc)
COB_OUTPUT := $(PROGRAM_NAME).cob
EXECUTABLE := $(PROGRAM_NAME)$(EXE_EXT)

.PHONY: all clean run transpile compile compiler ensure_environment ensure_cobc

all: transpile $(EXECUTABLE)

transpile: $(COB_OUTPUT)

compile: $(EXECUTABLE)

run: $(EXECUTABLE)
	$(EXECUTABLE)

$(EXECUTABLE): $(COB_OUTPUT) | ensure_cobc
	$(COBC) -x -o $@ $<

$(COB_OUTPUT): $(CBLC_SRC) $(TRANSPILER)
	$(TRANSPILER) --direction cblc-to-cobol \
		--input $(CBLC_SRC) \
		--output $@ \
		--diagnostics silent
	@if [ ! -f $@ ]; then \
		printf 'The transpiler did not emit %s. Check the command output above for diagnostics.\n' "$@"; \
		exit 1; \
	fi

compiler: $(TRANSPILER)

ensure_environment: $(SETUP_SENTINEL)

$(SETUP_SENTINEL):
	$(MAKE) -C $(REPO_ROOT) initialize
	$(MAKE) -C $(REPO_ROOT) install_cobc
	touch $(SETUP_SENTINEL)

ensure_cobc:
	@if ! command -v $(COBC) >/dev/null 2>&1; then \
		printf '\033[1;31m[cobc missing]\033[0m Attempting to install with "make install_cobc"...\n'; \
		$(MAKE) -C $(REPO_ROOT) install_cobc; \
		if ! command -v $(COBC) >/dev/null 2>&1; then \
			printf 'Unable to locate "$(COBC)" even after running install_cobc.\n'; \
			exit 1; \
		fi; \
	fi

$(TRANSPILER): | $(SETUP_SENTINEL)
	$(MAKE) -C $(REPO_ROOT) all

clean:
	rm -f $(COB_OUTPUT) $(EXECUTABLE) $(SETUP_SENTINEL)
