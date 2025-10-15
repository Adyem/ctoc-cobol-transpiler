REPO_ROOT := $(abspath ../..)
ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
else
EXE_EXT :=
endif

TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)
COBC ?= cobc

BUILD_DIR := build
SETUP_SENTINEL := $(BUILD_DIR)/.environment_ready
PROGRAM_NAME := hello_make_demo
CBLC_SRC := $(REPO_ROOT)/samples/cblc/example_project/$(PROGRAM_NAME).cblc
COB_OUTPUT := $(BUILD_DIR)/$(PROGRAM_NAME).cob
EXECUTABLE := $(BUILD_DIR)/$(PROGRAM_NAME)$(EXE_EXT)

.PHONY: all clean run compiler ensure_environment ensure_cobc

all: ensure_environment $(EXECUTABLE)

run: ensure_environment $(EXECUTABLE)
	$(EXECUTABLE)

$(EXECUTABLE): $(COB_OUTPUT) | ensure_cobc
	$(COBC) -x -o $@ $<

$(COB_OUTPUT): $(CBLC_SRC) compiler | ensure_environment $(BUILD_DIR)
	$(TRANSPILER) --direction cblc-to-cobol \
		--input $(CBLC_SRC) \
		--output $@ \
		--diagnostics silent
	@if [ ! -f $@ ]; then \
		printf 'The transpiler did not emit %s. Check the command output above for diagnostics.\n' "$@"; \
		exit 1; \
	fi

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

compiler: ensure_environment $(TRANSPILER)

ensure_environment: $(SETUP_SENTINEL)

$(SETUP_SENTINEL): | $(BUILD_DIR)
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

$(TRANSPILER): $(SETUP_SENTINEL)
	$(MAKE) -C $(REPO_ROOT) all

clean:
	rm -rf $(BUILD_DIR)
