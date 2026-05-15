REPO_ROOT := $(abspath ../..)
ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
else
EXE_EXT :=
endif

TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)
COBC ?= cobc
GOINFRE_LOCAL_PREFIX ?= /goinfre/$(USER)/local

COBC_ON_PATH := $(shell command -v $(COBC) 2>/dev/null)
GOINFRE_COBC := $(GOINFRE_LOCAL_PREFIX)/bin/cobc
GOINFRE_COBC_AVAILABLE := $(shell test -x "$(GOINFRE_COBC)" && printf yes)

ifeq ($(COBC_ON_PATH),)
ifeq ($(GOINFRE_COBC_AVAILABLE),yes)
COBC := $(GOINFRE_COBC)
export PATH := $(GOINFRE_LOCAL_PREFIX)/bin:$(PATH)
export LD_LIBRARY_PATH := $(GOINFRE_LOCAL_PREFIX)/lib:$(GOINFRE_LOCAL_PREFIX)/lib64$(if $(LD_LIBRARY_PATH),:$(LD_LIBRARY_PATH))
export CPATH := $(GOINFRE_LOCAL_PREFIX)/include$(if $(CPATH),:$(CPATH))
export LIBRARY_PATH := $(GOINFRE_LOCAL_PREFIX)/lib:$(GOINFRE_LOCAL_PREFIX)/lib64$(if $(LIBRARY_PATH),:$(LIBRARY_PATH))
endif
endif

SETUP_SENTINEL := .environment_ready
PROGRAM_NAME := hello_make_demo
CBLC_SRC := $(abspath $(PROGRAM_NAME).cblc)
COB_DIR := cobol
COB_OUTPUT := $(COB_DIR)/$(PROGRAM_NAME).cob
EXECUTABLE := $(PROGRAM_NAME)$(EXE_EXT)

.PHONY: all clean run transpile compile compiler ensure_environment ensure_cobc

all: transpile $(EXECUTABLE)

transpile: $(COB_OUTPUT)

compile: $(EXECUTABLE)

run: $(EXECUTABLE)
	./$(EXECUTABLE)

$(EXECUTABLE): $(COB_OUTPUT) | ensure_cobc
	$(COBC) -x -o $@ $<

$(COB_OUTPUT): $(CBLC_SRC) $(TRANSPILER)
	mkdir -p $(COB_DIR)
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
	$(MAKE) -C $(REPO_ROOT) all
	$(MAKE) -C $(REPO_ROOT) install_cobc
	touch $(SETUP_SENTINEL)

ensure_cobc:
	@if ! command -v $(COBC) >/dev/null 2>&1; then \
		printf 'Unable to locate "$(COBC)" on PATH or at "$(GOINFRE_COBC)".\n' >&2; \
		printf 'Run "make -C $(REPO_ROOT) install_cobc_goinfre" to install it locally.\n' >&2; \
		exit 1; \
	fi

$(TRANSPILER): | $(SETUP_SENTINEL)
	$(MAKE) -C $(REPO_ROOT) all

clean:
	rm -f $(COB_OUTPUT) $(EXECUTABLE) $(SETUP_SENTINEL)
	rmdir $(COB_DIR) 2>/dev/null || true
