REPO_ROOT := $(abspath ../..)
ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
MODULE_EXT := .dll
else
EXE_EXT :=
MODULE_EXT := .so
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

MAIN_PROGRAM := message_showcase
CBLC_MODULES := message_showcase_main message_showcase_constants message_showcase_text message_showcase_ledger message_showcase_memory
CBLC_SOURCES := $(addsuffix .cblc,$(CBLC_MODULES))
COB_DIR := cobol
GENERATED_COB := $(addprefix $(COB_DIR)/,$(addsuffix .cob,$(CBLC_MODULES)))
EXECUTABLES := $(MAIN_PROGRAM)$(EXE_EXT)

STD_DIR := stdlib
STD_SENTINEL := $(STD_DIR)/.standard_library_built
STD_HELPERS := CBLC-ABS CBLC-STRCAT CBLC-STRCPY CBLC-STRLEN
STD_SOURCES := $(addprefix $(STD_DIR)/,$(addsuffix .cob,$(STD_HELPERS)))
STD_MODULES := $(addprefix $(STD_DIR)/,$(addsuffix $(MODULE_EXT),$(STD_HELPERS)))
SETUP_SENTINEL := .environment_ready
ACTUAL_OUTPUT := actual_output.log

RUN_SHOWCASE = ./$(MAIN_PROGRAM)$(EXE_EXT); \
printf '%s\n' "Standard library functions bundled with the showcase:"; \
printf '%s\n' $(foreach helper,$(STD_HELPERS),$(STD_DIR)/$(helper).cob $(STD_DIR)/$(helper)$(MODULE_EXT))

.PHONY: all clean run verify transpile compile stdlib ensure_environment ensure_cobc

all: ensure_environment stdlib transpile compile

transpile: $(GENERATED_COB)

stdlib: $(STD_SENTINEL) $(STD_MODULES)

compile: $(EXECUTABLES) $(STD_MODULES)

run: $(EXECUTABLES) $(STD_MODULES)
	@set -e; \
	export COB_LIBRARY_PATH=$(abspath $(STD_DIR)); \
	$(RUN_SHOWCASE)

verify: $(EXECUTABLES) $(STD_MODULES)
	@set -e; \
	export COB_LIBRARY_PATH=$(abspath $(STD_DIR)); \
	{ $(RUN_SHOWCASE); } | sed 's/[[:space:]]*$$//' | tee $(ACTUAL_OUTPUT)
	@diff -u EXPECTED_OUTPUT.txt $(ACTUAL_OUTPUT)

$(MAIN_PROGRAM)$(EXE_EXT): $(GENERATED_COB) | ensure_cobc
	$(COBC) -x -free -o $@ $(GENERATED_COB)

$(GENERATED_COB): $(CBLC_SOURCES) $(TRANSPILER)
	mkdir -p $(COB_DIR)
	$(TRANSPILER) --direction cblc-to-cobol \
		$(foreach source,$(CBLC_SOURCES),--input $(abspath $(source)) ) \
		$(foreach output,$(GENERATED_COB),--output $(abspath $(output)) ) \
		--diagnostics silent
	@set -e; \
	for output in $(GENERATED_COB); do \
		if [ ! -f "$$output" ]; then \
			printf 'The transpiler did not emit %s. Check the command output above for diagnostics.\n' "$$output"; \
			exit 1; \
		fi; \
	done

$(STD_SENTINEL): $(TRANSPILER)
	mkdir -p $(STD_DIR)
	CTOC_SKIP_STANDARD_LIBRARY_VALIDATION=1 $(TRANSPILER) --direction standard-library \
	        --output-dir $(abspath $(STD_DIR)) \
	        --diagnostics silent
	touch $@

$(STD_DIR)/%$(MODULE_EXT): $(STD_SENTINEL) | ensure_cobc
	@if [ ! -f $(STD_DIR)/$*.cob ]; then \
	        printf 'The standard library source %s is missing.\n' "$(STD_DIR)/$*.cob"; \
	        exit 1; \
	fi
	$(COBC) -m -free -o $@ $(STD_DIR)/$*.cob

ensure_environment: $(SETUP_SENTINEL)

$(SETUP_SENTINEL):
	$(MAKE) -C $(REPO_ROOT) all
	$(MAKE) -C $(REPO_ROOT) install_cobc
	touch $@

ensure_cobc:
	@if ! command -v $(COBC) >/dev/null 2>&1; then \
		printf 'Unable to locate "$(COBC)" on PATH or at "$(GOINFRE_COBC)".\n' >&2; \
		printf 'Run "make -C $(REPO_ROOT) install_cobc_goinfre" to install it locally.\n' >&2; \
		exit 1; \
	fi

$(TRANSPILER): | $(SETUP_SENTINEL)
	$(MAKE) -C $(REPO_ROOT) all

clean:
	rm -f $(GENERATED_COB) $(EXECUTABLES) $(SETUP_SENTINEL) $(STD_SENTINEL) $(ACTUAL_OUTPUT)
	rm -f $(STD_MODULES)
	rmdir $(COB_DIR) 2>/dev/null || true
	rm -rf $(STD_DIR)
