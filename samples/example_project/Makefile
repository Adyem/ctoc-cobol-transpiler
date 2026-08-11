REPO_ROOT := $(abspath ../..)
COBC ?= cobc
GOINFRE_LOCAL_PREFIX ?= /goinfre/$(USER)/local
include ../Makefile.platform
TRANSPILER := $(REPO_ROOT)/ctoc_cobol_transpiler$(EXE_EXT)

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
	$(SAMPLE_RUN)$(EXECUTABLE)

$(EXECUTABLE): $(COB_OUTPUT) | ensure_cobc
	$(COBC) -x -o $@ $<

$(COB_OUTPUT): $(CBLC_SRC) $(TRANSPILER)
	$(call SAMPLE_MKDIR,$(COB_DIR))
	"$(TRANSPILER)" --direction cblc-to-cobol \
		--input $(CBLC_SRC) \
		--output $@ \
		--diagnostics silent
	$(call SAMPLE_REQUIRE_FILE,$@)

compiler: $(TRANSPILER)

ensure_environment: $(SETUP_SENTINEL)

$(TRANSPILER):
	$(MAKE) -C $(REPO_ROOT) all

$(SETUP_SENTINEL):
	$(MAKE) -C $(REPO_ROOT) all
ifeq ($(OS),Windows_NT)
	@$(SAMPLE_COBC_AVAILABLE) || (echo Unable to locate "$(COBC)" on PATH. & exit /b 1)
else
	$(MAKE) -C $(REPO_ROOT) install_cobc
endif
	$(call SAMPLE_TOUCH,$(SETUP_SENTINEL))

clean:
	-$(call SAMPLE_REMOVE_FILES,$(COB_OUTPUT) $(EXECUTABLE) $(SETUP_SENTINEL))
	-$(call SAMPLE_REMOVE_DIR,$(COB_DIR))
