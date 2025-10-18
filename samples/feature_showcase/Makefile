REPO_ROOT := $(abspath ../..)
ifeq ($(OS),Windows_NT)
EXE_EXT := .exe
else
EXE_EXT :=
endif

TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)
COBC ?= cobc

CBLC_PROGRAMS := message_showcase message_showcase_banner message_showcase_metrics
COBOL_PROGRAMS := message_showcase_io
CBLC_SOURCES := $(addsuffix .cblc,$(CBLC_PROGRAMS))
GENERATED_COB := $(addsuffix .cob,$(CBLC_PROGRAMS))
STATIC_COB := $(addsuffix .cob,$(COBOL_PROGRAMS))
ALL_PROGRAMS := $(CBLC_PROGRAMS) $(COBOL_PROGRAMS)
EXECUTABLES := $(addsuffix $(EXE_EXT),$(ALL_PROGRAMS))
STD_DIR := stdlib
STD_SENTINEL := $(STD_DIR)/.standard_library_built
IO_INPUT := showcase_transactions.dat
IO_OUTPUT := showcase_success.txt showcase_failure.txt
IO_SENTINEL := .io_seeded

SETUP_SENTINEL := .environment_ready

.PHONY: all clean run transpile compile compiler ensure_environment ensure_cobc stdlib io-fixtures

all: stdlib transpile compile

transpile: $(GENERATED_COB)

stdlib: $(STD_SENTINEL)

compile: $(EXECUTABLES)

run: io-fixtures $(EXECUTABLES)
	./message_showcase$(EXE_EXT)
	./message_showcase_banner$(EXE_EXT)
	./message_showcase_metrics$(EXE_EXT)
	./message_showcase_io$(EXE_EXT)

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

io-fixtures: $(IO_SENTINEL)

$(IO_SENTINEL):
	printf 'A 00027\nR 00015\nA 00012\n' > $(IO_INPUT)
	touch $(IO_SENTINEL)

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
	rm -f $(GENERATED_COB) $(EXECUTABLES) $(SETUP_SENTINEL) $(STD_SENTINEL) $(IO_SENTINEL) $(IO_INPUT) $(IO_OUTPUT)
	rm -rf $(STD_DIR)
