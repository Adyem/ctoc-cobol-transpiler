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

CBLC_PROGRAMS := message_showcase message_showcase_banner message_showcase_metrics
CBLC_SOURCES := $(addsuffix .cblc,$(CBLC_PROGRAMS))
GENERATED_COB := $(addsuffix .cob,$(CBLC_PROGRAMS))
EXECUTABLES := $(addsuffix $(EXE_EXT),$(CBLC_PROGRAMS))

STD_DIR := stdlib
STD_SENTINEL := $(STD_DIR)/.standard_library_built
STD_HELPERS := CBLC-ABS CBLC-STRCAT CBLC-STRCPY CBLC-STRLEN
STD_SOURCES := $(addprefix $(STD_DIR)/,$(addsuffix .cob,$(STD_HELPERS)))
STD_MODULES := $(addprefix $(STD_DIR)/,$(addsuffix $(MODULE_EXT),$(STD_HELPERS)))
SETUP_SENTINEL := .environment_ready
ACTUAL_OUTPUT := actual_output.log

RUN_SHOWCASE = ./message_showcase$(EXE_EXT); \
printf '\n'; \
./message_showcase_banner$(EXE_EXT); \
printf '\n'; \
./message_showcase_metrics$(EXE_EXT); \
printf '\n'; \
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

%$(EXE_EXT): %.cob | ensure_cobc
	$(COBC) -x -free -o $@ $<

%.cob: %.cblc $(TRANSPILER)
	$(TRANSPILER) --direction cblc-to-cobol \
		--input $(abspath $<) \
		--output $(abspath $@) \
		--diagnostics silent
	@if [ ! -f $@ ]; then \
		printf 'The transpiler did not emit %s. Check the command output above for diagnostics.\n' "$@"; \
		exit 1; \
	fi

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
	$(MAKE) -C $(REPO_ROOT) initialize
	$(MAKE) -C $(REPO_ROOT) install_cobc
	touch $@

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
	rm -f $(GENERATED_COB) $(EXECUTABLES) $(SETUP_SENTINEL) $(STD_SENTINEL) $(ACTUAL_OUTPUT)
	rm -f $(STD_MODULES)
	rm -rf $(STD_DIR)
