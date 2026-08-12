REPO_ROOT := $(abspath ../..)
COBC ?= cobc
GOINFRE_LOCAL_PREFIX ?= /goinfre/$(USER)/local
include ../Makefile.platform
TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)
COBOL_NORMALIZER := $(REPO_ROOT)/scripts/cobol_fixed_to_free.py

MAIN_PROGRAM := message_showcase
CBLC_MODULES := message_showcase_main message_showcase_constants message_showcase_text message_showcase_ledger message_showcase_memory message_showcase_refs
CBLC_SOURCES := $(addsuffix .cblc,$(CBLC_MODULES))
COB_DIR := cobol
GENERATED_COB := $(addprefix $(COB_DIR)/,$(addsuffix .cob,$(CBLC_MODULES)))
TRANSPILE_SENTINEL := $(COB_DIR)/.transpiled
EXECUTABLES := $(MAIN_PROGRAM)$(EXE_EXT)

STD_DIR := stdlib
STD_SENTINEL := $(STD_DIR)/.standard_library_built
STD_HELPERS := CBLC-ABS CBLC-STRCAT CBLC-STRCAT-STRING CBLC-STRCPY CBLC-STRLEN CBLC-STRCMP-STRING
STD_SOURCES := $(addprefix $(STD_DIR)/,$(addsuffix .cob,$(STD_HELPERS)))
STD_MODULES := $(addprefix $(STD_DIR)/,$(addsuffix $(MODULE_EXT),$(STD_HELPERS)))
STD_CATALOG := $(foreach helper,$(STD_HELPERS),$(STD_DIR)/$(helper).cob $(STD_DIR)/$(helper)$(MODULE_EXT))
SETUP_SENTINEL := .environment_ready
ACTUAL_OUTPUT := actual_output.log
EXPECTED_PLATFORM_OUTPUT := .expected_output

.PHONY: all clean run verify transpile compile stdlib ensure_environment ensure_cobc

all: stdlib transpile compile

transpile: $(TRANSPILE_SENTINEL)

stdlib: $(STD_SENTINEL) $(STD_MODULES)

compile: $(EXECUTABLES) $(STD_MODULES)

run: $(EXECUTABLES) $(STD_MODULES)
	$(call SAMPLE_RUN_WITH_FILES,$(abspath $(STD_DIR)),$(MAIN_PROGRAM)$(EXE_EXT),"Standard library functions bundled with the showcase:" $(STD_CATALOG))

verify: $(EXECUTABLES) $(STD_MODULES) $(EXPECTED_PLATFORM_OUTPUT)
	{ $(call SAMPLE_RUN_WITH_FILES,$(abspath $(STD_DIR)),$(MAIN_PROGRAM)$(EXE_EXT),"Standard library functions bundled with the showcase:" $(STD_CATALOG)); } > $(ACTUAL_OUTPUT)
	$(call SAMPLE_REQUIRE_FILE,$(ACTUAL_OUTPUT))
	$(call SAMPLE_COMPARE_OUTPUT,$(EXPECTED_PLATFORM_OUTPUT),$(ACTUAL_OUTPUT))

$(EXPECTED_PLATFORM_OUTPUT): EXPECTED_OUTPUT.txt
	$(call SAMPLE_PREPARE_EXPECTED,$<,$@,$(MODULE_EXT))

$(MAIN_PROGRAM)$(EXE_EXT): $(GENERATED_COB) $(STD_SOURCES) | ensure_cobc
	$(COBC) -x -free -fstatic-call -o $@ $(GENERATED_COB) $(STD_SOURCES)

$(TRANSPILE_SENTINEL): $(CBLC_SOURCES) $(TRANSPILER) $(COBOL_NORMALIZER)
	$(call SAMPLE_MKDIR,$(COB_DIR))
	"$(TRANSPILER)" --direction cblc-to-cobol \
		$(foreach source,$(CBLC_SOURCES),--input $(abspath $(source)) ) \
		$(foreach output,$(GENERATED_COB),--output $(abspath $(output)) ) \
		--diagnostics silent
	$(foreach output,$(GENERATED_COB),$(call SAMPLE_REQUIRE_FILE,$(output)))
	$(PYTHON) "$(COBOL_NORMALIZER)" $(GENERATED_COB)
	$(call SAMPLE_TOUCH,$@)

$(GENERATED_COB): $(TRANSPILE_SENTINEL)
	$(call SAMPLE_REQUIRE_FILE,$@)

$(STD_SENTINEL): $(TRANSPILER)
	$(call SAMPLE_MKDIR,$(STD_DIR))
	$(SAMPLE_SKIP_VALIDATION) "$(TRANSPILER)" --direction standard-library \
	        --output-dir $(abspath $(STD_DIR)) \
	        --diagnostics silent
	$(call SAMPLE_TOUCH,$@)

$(STD_DIR)/%$(MODULE_EXT): $(STD_SENTINEL) | ensure_cobc
	$(call SAMPLE_REQUIRE_FILE,$(STD_DIR)/$*.cob)
	$(COBC) -m -free -o $@ $(STD_DIR)/$*.cob

ensure_environment: $(SETUP_SENTINEL)

$(SETUP_SENTINEL):
	$(MAKE) -C $(REPO_ROOT) all
ifeq ($(OS),Windows_NT)
	@$(SAMPLE_COBC_AVAILABLE) || (echo Unable to locate "$(COBC)" on PATH. & exit /b 1)
else
	$(MAKE) -C $(REPO_ROOT) install_cobc
endif
	$(call SAMPLE_TOUCH,$@)

$(TRANSPILER):
	$(MAKE) -C $(REPO_ROOT) all

clean:
	-$(call SAMPLE_REMOVE_FILES,$(GENERATED_COB) $(TRANSPILE_SENTINEL) $(EXECUTABLES) $(SETUP_SENTINEL) $(STD_SENTINEL) $(ACTUAL_OUTPUT) $(EXPECTED_PLATFORM_OUTPUT) $(STD_MODULES))
	-$(call SAMPLE_REMOVE_DIR,$(COB_DIR))
	-$(call SAMPLE_REMOVE_DIR,$(STD_DIR))
