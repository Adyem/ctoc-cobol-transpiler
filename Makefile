SHELL := /bin/bash

ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
else
    EXE_EXT :=
endif

NAME        = ctoc_cobol_transpiler$(EXE_EXT)
NAME_DEBUG  = ctoc_cobol_transpiler_debug$(EXE_EXT)
TEST_NAME   = automated_tests$(EXE_EXT)
LSP_NAME    = cblc_lsp$(EXE_EXT)

ifdef FORWARD_TRANSLATION
export CTOC_ENABLE_FORWARD_TRANSLATION := $(FORWARD_TRANSLATION)
endif
PYTHON          ?= python3
LINT_SCRIPT      = scripts/lint_sources.py
FUZZ_SCRIPT      = scripts/fuzz_transpiler.py
COVERAGE_SCRIPT  = scripts/coverage_report.py
FUZZ_ITERATIONS ?= 50
FUZZ_MODE ?= all
FUZZ_ARGS ?=

SRC         = \
    main.cpp \
    src/runtime/runtime_scalar.cpp \
    src/runtime/runtime_string.cpp \
    src/runtime/runtime_collation.cpp \
    src/runtime/runtime_encoding.cpp \
    src/runtime/runtime_record.cpp \
    src/runtime/runtime_file.cpp \
    src/runtime/runtime_memory.cpp \
    src/runtime/runtime_csv.cpp \
    src/runtime/runtime_sort.cpp \
    src/frontend/lexer.cpp \
    src/frontend/lexer_token.cpp \
    src/frontend/ast.cpp \
    src/frontend/parser.cpp \
    src/transpiler/transpiler_frontend.cpp \
    src/transpiler/transpiler_diagnostics.cpp \
    src/transpiler/transpiler_context.cpp \
    src/transpiler/transpiler_pipeline.cpp \
    src/transpiler/transpiler_parallel.cpp \
    src/transpiler/transpiler_cblc.cpp \
    src/transpiler/transpiler_c_backend.cpp \
    src/transpiler/transpiler_cli.cpp \
    src/formatter/cblc_formatter.cpp \
    src/transpiler/transpiler_logging.cpp \
    src/transpiler/transpiler_incremental_cache.cpp \
    src/transpiler/transpiler_runtime_helpers.cpp \
    src/transpiler/transpiler_validation.cpp \
    src/transpiler/transpiler_ast_visualizer.cpp \
    src/transpiler/transpiler_copybook_graph.cpp \
    src/transpiler/transpiler_semantic_dump.cpp \
    src/transpiler/semantics/transpiler_semantics.cpp \
    src/transpiler/semantics/transpiler_semantics_errors.cpp \
    src/transpiler/semantics/transpiler_semantics_helpers.cpp \
    src/transpiler/semantics/transpiler_semantics_scope.cpp \
    src/transpiler/semantics/transpiler_semantics_classifiers.cpp \
    src/transpiler/semantics/transpiler_semantics_conditions.cpp \
    src/transpiler/semantics/transpiler_semantics_assignments.cpp \
    src/transpiler/semantics/transpiler_semantics_statements.cpp \
    src/transpiler/transpiler_codegen.cpp \
    src/transpiler/transpiler_cobol_types.cpp \
    src/transpiler/transpiler_cobol_procedure.cpp \
    src/transpiler/transpiler_cobol_reverse.cpp \
    src/standard_library/transpiler_standard_library.cpp \
    src/standard_library/transpiler_standard_library_state.cpp \
    src/standard_library/transpiler_standard_library_strlen.cpp \
    src/standard_library/transpiler_standard_library_strlen_string.cpp \
    src/standard_library/transpiler_standard_library_strnlen.cpp \
    src/standard_library/transpiler_standard_library_strnlen_string.cpp \
    src/standard_library/transpiler_standard_library_strcmp.cpp \
    src/standard_library/transpiler_standard_library_strcmp_string.cpp \
    src/standard_library/transpiler_standard_library_strcpy.cpp \
    src/standard_library/transpiler_standard_library_strcpy_string.cpp \
    src/standard_library/transpiler_standard_library_strncpy.cpp \
    src/standard_library/transpiler_standard_library_strncpy_string.cpp \
    src/standard_library/transpiler_standard_library_memcmp.cpp \
    src/standard_library/transpiler_standard_library_memcmp_string.cpp \
    src/standard_library/transpiler_standard_library_strcat.cpp \
    src/standard_library/transpiler_standard_library_strcat_string.cpp \
    src/standard_library/transpiler_standard_library_strtod.cpp \
    src/standard_library/transpiler_standard_library_strtod_string.cpp \
    src/standard_library/transpiler_standard_library_abs.cpp \
    src/standard_library/transpiler_standard_library_fabs.cpp \
    src/standard_library/transpiler_standard_library_floor.cpp \
    src/standard_library/transpiler_standard_library_ceil.cpp \
    src/standard_library/transpiler_standard_library_exp.cpp \
    src/standard_library/transpiler_standard_library_log.cpp \
    src/standard_library/transpiler_standard_library_sin.cpp \
    src/standard_library/transpiler_standard_library_cos.cpp \
    src/standard_library/transpiler_standard_library_tan.cpp \
    src/standard_library/transpiler_standard_library_rounded.cpp \
    src/standard_library/transpiler_standard_library_banker_round.cpp \
    src/standard_library/transpiler_standard_library_date_yyyymmdd.cpp \
    src/standard_library/transpiler_standard_library_date_duration.cpp \
    src/standard_library/transpiler_standard_library_atoi.cpp \
    src/standard_library/transpiler_standard_library_atoi_string.cpp \
    src/standard_library/transpiler_standard_library_atol.cpp \
    src/standard_library/transpiler_standard_library_atol_string.cpp \
    src/standard_library/transpiler_standard_library_atoll.cpp \
    src/standard_library/transpiler_standard_library_atoll_string.cpp \
    src/standard_library/transpiler_standard_library_powerof.cpp \
    src/standard_library/transpiler_standard_library_sqrt.cpp \
    src/standard_library/transpiler_standard_library_min.cpp \
    src/standard_library/transpiler_standard_library_max.cpp \
    src/standard_library/transpiler_standard_library_toupper.cpp \
    src/standard_library/transpiler_standard_library_toupper_string.cpp \
    src/standard_library/transpiler_standard_library_tolower.cpp \
    src/standard_library/transpiler_standard_library_tolower_string.cpp \
    src/standard_library/transpiler_standard_library_isdigit.cpp \
    src/standard_library/transpiler_standard_library_isalpha.cpp

CC          = g++

OPT_LEVEL ?= 0

ifeq ($(OPT_LEVEL),0)
    OPT_FLAGS = -O0 -g
else ifeq ($(OPT_LEVEL),1)
    OPT_FLAGS = -O1 -flto -s -ffunction-sections -fdata-sections -Wl,--gc-sections
else ifeq ($(OPT_LEVEL),2)
    OPT_FLAGS = -O2 -flto -s -ffunction-sections -fdata-sections -Wl,--gc-sections
else ifeq ($(OPT_LEVEL),3)
    OPT_FLAGS = -O3 -flto -s -ffunction-sections -fdata-sections -Wl,--gc-sections
else
    $(error Unsupported OPT_LEVEL=$(OPT_LEVEL))
endif

COMPILE_FLAGS = -Wall -Werror -Wextra -std=c++17 -Wmissing-declarations \
                -Wold-style-cast -Wshadow -Wconversion -Wformat=2 -Wundef \
                -Wfloat-equal -Wconversion -Wodr -Wuseless-cast \
                -Wzero-as-null-pointer-constant -Wmaybe-uninitialized \
                -I. $(OPT_FLAGS)
DEPFLAGS = -MMD -MP

CFLAGS = $(COMPILE_FLAGS)

REPRODUCIBLE ?= 1
SOURCE_DATE_EPOCH ?= 1700000000

ifneq ($(REPRODUCIBLE),0)
    export SOURCE_DATE_EPOCH
    COMPILE_FLAGS += -ffile-prefix-map=$(CURDIR)=. -fmacro-prefix-map=$(CURDIR)=. -fdebug-prefix-map=$(CURDIR)=. -frandom-seed=ctoc
endif

ifeq ($(OS),Windows_NT)
    MKDIR   = mkdir
    RMDIR   = rmdir /S /Q
    RM      = del /F /Q
else
    MKDIR   = mkdir -p
    RMDIR   = rm -rf
    RM      = rm -f
endif

BUILD_LOG_DIR = ./build_logs

OBJ_DIR         = ./objs
OBJ_DIR_DEBUG   = ./objs_debug
OBJ_DIR_TEST    = ./objs_tests

ENABLE_LTO  ?= 0
ENABLE_PGO  ?= 0
COVERAGE    ?= 0
COVERAGE_LINE_THRESHOLD ?= 60
COVERAGE_BRANCH_THRESHOLD ?= 65
export ENABLE_LTO ENABLE_PGO COVERAGE

ifeq ($(ENABLE_LTO),1)
    COMPILE_FLAGS   += -flto
    LDFLAGS  += -flto
endif

ifeq ($(ENABLE_PGO),1)
    COMPILE_FLAGS  += -fprofile-generate
endif

ifeq ($(COVERAGE),1)
    COMPILE_FLAGS  += --coverage
    LDFLAGS        += --coverage
endif

ifeq ($(DEBUG),1)
    CFLAGS    += -DDEBUG=1
    OBJ_DIR    = $(OBJ_DIR_DEBUG)
    TARGET     = $(NAME_DEBUG)
else
    TARGET     = $(NAME)
endif

export COMPILE_FLAGS

ifeq ($(OS),Windows_NT)
    LDFLAGS     =
else
    LDFLAGS     = -lreadline
endif

OBJS        = $(SRC:%.cpp=$(OBJ_DIR)/%.o)

OBJS_NO_MAIN = $(filter-out $(OBJ_DIR)/main.o,$(OBJS))
LSP_SRC     = src/lsp/cblc_lsp.cpp
LSP_OBJ     = $(LSP_SRC:%.cpp=$(OBJ_DIR)/%.o)

.SILENT:

TOTAL_OBJS          := $(words $(OBJS))

TEST_SRC    = imported/libft_test_runner.cpp \
              tests/test_main.cpp \
              tests/test_support.cpp \
              tests/ast_tests.cpp \
              tests/ast_visualizer_tests.cpp \
              tests/copybook_graph_tests.cpp \
              tests/lexer_tests.cpp \
              tests/parser_tests.cpp \
             tests/parser_positive_program_tests.cpp \
             tests/parser_positive_control_flow_tests.cpp \
             tests/parser_positive_literal_character_tests.cpp \
             tests/parser_positive_literal_assignment_tests.cpp \
             tests/parser_positive_literal_arithmetic_tests.cpp \
             tests/parser_negative_statement_tests.cpp \
             tests/parser_negative_structure_tests.cpp \
             tests/parser_negative_data_tests.cpp \
             tests/semantics_tests.cpp \
             tests/semantics_assignment_tests.cpp \
             tests/semantics_arithmetic_tests.cpp \
            tests/semantics_condition_equality_tests.cpp \
            tests/semantics_condition_relational_tests.cpp \
            tests/semantics_condition_tests.cpp \
             tests/semantics_control_flow_tests.cpp \
             tests/semantics_usage_tests.cpp \
            tests/semantics_test_support.cpp \
             tests/runtime_int_tests.cpp \
              tests/runtime_char_tests.cpp \
             tests/runtime_string_tests.cpp \
             tests/runtime_csv_tests.cpp \
             tests/runtime_collation_tests.cpp \
             tests/runtime_encoding_tests.cpp \
             tests/runtime_memory_tests.cpp \
             tests/runtime_sort_tests.cpp \
             tests/standard_library_tests.cpp \
             tests/standard_library/standard_library_registry_tests.cpp \
             tests/standard_library/standard_library_abs_tests.cpp \
             tests/standard_library/standard_library_atoi_tests.cpp \
             tests/standard_library/standard_library_atol_tests.cpp \
             tests/standard_library/standard_library_atoll_tests.cpp \
             tests/standard_library/standard_library_ceil_tests.cpp \
             tests/standard_library/standard_library_cos_tests.cpp \
             tests/standard_library/standard_library_exp_tests.cpp \
             tests/standard_library/standard_library_fabs_tests.cpp \
             tests/standard_library/standard_library_floor_tests.cpp \
             tests/standard_library/standard_library_rounded_tests.cpp \
             tests/standard_library/standard_library_banker_round_tests.cpp \
             tests/standard_library/standard_library_date_parse_tests.cpp \
             tests/standard_library/standard_library_date_duration_tests.cpp \
             tests/standard_library/standard_library_isalpha_tests.cpp \
             tests/standard_library/standard_library_isdigit_tests.cpp \
             tests/standard_library/standard_library_log_tests.cpp \
             tests/standard_library/standard_library_memcmp_tests.cpp \
             tests/standard_library/standard_library_powerof_tests.cpp \
             tests/standard_library/standard_library_sin_tests.cpp \
             tests/standard_library/standard_library_sqrt_tests.cpp \
             tests/standard_library/standard_library_strcat_tests.cpp \
             tests/standard_library/standard_library_strcmp_tests.cpp \
             tests/standard_library/standard_library_strcpy_tests.cpp \
             tests/standard_library/standard_library_strlen_tests.cpp \
             tests/standard_library/standard_library_strncpy_tests.cpp \
             tests/standard_library/standard_library_strnlen_tests.cpp \
             tests/standard_library/standard_library_strtod_tests.cpp \
             tests/standard_library/standard_library_tan_tests.cpp \
             tests/standard_library/standard_library_tolower_tests.cpp \
             tests/standard_library/standard_library_toupper_tests.cpp \
              tests/runtime_audit_tests.cpp \
              tests/runtime_record_tests.cpp \
             tests/runtime_file_tests.cpp \
             tests/parallel_generation_tests.cpp \
              tests/pipeline_tests.cpp \
              tests/validation_tests.cpp \
             tests/transpiler_context_tests.cpp \
             tests/transpiler_context_registration_tests.cpp \
             tests/transpiler_context_copybook_tests.cpp \
             tests/transpiler_context_visibility_tests.cpp \
             tests/cblc_translation_unit_export_tests.cpp \
             tests/transpiler_context_import_tests.cpp \
             tests/transpiler_context_call_metadata_tests.cpp \
             tests/transpiler_context_file_tests.cpp \
             tests/transpiler_context_source_map_tests.cpp \
             tests/transpiler_context_comment_basic_tests.cpp \
             tests/transpiler_context_comment_capacity_tests.cpp \
             tests/transpiler_context_semantic_dump_tests.cpp \
              tests/sample_inventory_tests.cpp \
              tests/golden_file_tests.cpp \
              tests/grammar_doc_tests.cpp \
              tests/cobol_doc_tests.cpp \
              tests/cobol_execution_tests.cpp \
              tests/cblc_doc_tests.cpp \
              tests/cli_doc_tests.cpp \
              tests/contributing_doc_tests.cpp \
              tests/onboarding_doc_tests.cpp \
              tests/runtime_doc_tests.cpp \
              tests/cli_parse_success_tests.cpp \
              tests/cli_parse_failure_tests.cpp \
              tests/cli_standard_library_tests.cpp \
              tests/cli_tests.cpp \
              tests/ci_tests.cpp \
              tests/codegen_tests.cpp \
              tests/round_trip_tests.cpp \
              tests/property_tests.cpp \
              tests/cobol_type_tests.cpp \
              tests/compiler/compiler_test_support.cpp \
              tests/compiler/c/build_tests.cpp \
              tests/compiler/c/exit_tests.cpp \
              tests/compiler/c/multi_module_tests.cpp \
              tests/compiler/c/tests.cpp \
              tests/compiler/differential_tests.cpp \
              tests/compiler/incremental_cache_tests.cpp \
              tests/compiler/cobol/copy_file_tests.cpp \
              tests/compiler/cobol/filter_prefix_tests.cpp \
              tests/compiler/cobol/return_numeric_tests.cpp \
              tests/compiler/cobol/multi_module_tests.cpp \
              tests/compiler/cobol/return_boolean_tests.cpp \
              tests/compiler/cobol/return_character_tests.cpp \
              tests/compiler/cobol/record_writer_tests.cpp \
              tests/compiler/cobol/integration_showcase_tests.cpp \
              tests/compiler/cobol/message_showcase_tests.cpp \
              tests/compiler/cobol/record_summary_tests.cpp \
              tests/compiler/cobol/reverse_control_flow_tests.cpp \
              tests/compiler/cobol/reverse_normalization_tests.cpp \
              tests/compiler/cobol/reverse_cli_tests.cpp \
              tests/compiler/cobol/round_trip_pipeline_helpers.cpp \
              tests/compiler/cobol/round_trip_pipeline_cblc_to_cobol_tests.cpp \
              tests/compiler/cobol/round_trip_pipeline_cobol_round_trip_tests.cpp \
              tests/compiler/cobol/round_trip_pipeline_tests.cpp \
              tests/compiler/cobol/tests.cpp \
              tests/compiler/tests.cpp \
              tests/logging_tests.cpp \
              tests/cobol_reverse_tests.cpp \
              tests/cblc_formatter_tests.cpp \
              tests/stress_tests.cpp

TEST_OBJS   = $(TEST_SRC:%.cpp=$(OBJ_DIR_TEST)/%.o)

TOTAL_TEST_OBJS     := $(words $(TEST_OBJS))
DEPS        = $(OBJS:.o=.d) $(LSP_OBJ:.o=.d) $(TEST_OBJS:.o=.d)

all: dirs transpiler lsp

transpiler: $(TARGET)

lsp: $(LSP_NAME)

tests: dirs $(TEST_NAME)

dirs:
	@-$(MKDIR) $(OBJ_DIR)
	@-$(MKDIR) $(OBJ_DIR_DEBUG)
	@-$(MKDIR) $(OBJ_DIR_TEST)

$(BUILD_LOG_DIR):
	@-$(MKDIR) $(BUILD_LOG_DIR)

install_cobc:
	@if ! command -v cobc >/dev/null 2>&1; then \
		apt-get update && apt-get install -y gnucobol; \
	else \
		printf 'cobc already installed.\n'; \
	fi


tests_with_cobc: install_cobc
	$(MAKE) tests
	$(MAKE) test

debug:
	$(MAKE) all DEBUG=1

$(TARGET): $(OBJS)
	@printf '\033[1;36m[CTOC BUILD] Linking %s\033[0m\n' "$@"
	@$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS)

$(LSP_NAME): $(LSP_OBJ) $(OBJS_NO_MAIN)
	@printf '\033[1;36m[CTOC BUILD] Linking %s\033[0m\n' "$@"
	@$(CC) $(CFLAGS) $(LSP_OBJ) $(OBJS_NO_MAIN) -o $@ $(LDFLAGS)

$(TEST_NAME): $(TEST_OBJS) $(OBJS_NO_MAIN) $(TARGET)
	@printf '\033[1;36m[CTOC BUILD] Linking %s\033[0m\n' "$@"
	@$(CC) $(CFLAGS) $(TEST_OBJS) $(OBJS_NO_MAIN) -o $@ $(LDFLAGS)
$(OBJ_DIR)/%.o: %.cpp
	@-$(MKDIR) $(dir $@)
	@$(CC) $(CFLAGS) $(DEPFLAGS) -c $< -o $@
	@if [ $(TOTAL_OBJS) -gt 0 ]; then \
		built=$$(find $(OBJ_DIR) -type f -name '*.o' | wc -l); \
		printf '\033[1;36m[CTOC PROGRESS] %s (%d/%d)\033[0m\n' "$<" $$built $(TOTAL_OBJS); \
	fi

$(OBJ_DIR_TEST)/%.o: %.cpp
	@-$(MKDIR) $(dir $@)
	@$(CC) $(CFLAGS) $(DEPFLAGS) -c $< -o $@
	@if [ $(TOTAL_TEST_OBJS) -gt 0 ]; then \
		built=$$(find $(OBJ_DIR_TEST) -type f -name '*.o' | wc -l); \
		printf '\033[1;36m[CTOC TEST PROGRESS] %s (%d/%d)\033[0m\n' "$<" $$built $(TOTAL_TEST_OBJS); \
	fi

-include $(DEPS)

clean:
	-$(RM) $(OBJ_DIR)/*.o $(OBJ_DIR_DEBUG)/*.o
	-$(RM) $(OBJ_DIR_TEST)/*.o $(OBJ_DIR_TEST)/tests/*.o
	-$(RM) $(DEPS)
	-$(RM) test_example_compiler.c test_example_compiler.bin test_example_compiler.txt
	-$(RM) test_example_invalid_compiler.c test_example_invalid_compiler.bin test_example_invalid_compiler.log
	-$(RM) test_runtime_file.txt
	-$(RMDIR) $(BUILD_LOG_DIR)

fclean: clean
	-$(RM) $(NAME) $(NAME_DEBUG) $(TEST_NAME) $(LSP_NAME)
	-$(RMDIR) $(OBJ_DIR) $(OBJ_DIR_DEBUG) $(OBJ_DIR_TEST) data

re: fclean all

test: $(TEST_NAME)
	./$(TEST_NAME)

both: all debug

re_both: re both

lint:
	$(PYTHON) $(LINT_SCRIPT)

fuzz: $(TARGET)
	$(PYTHON) $(FUZZ_SCRIPT) --iterations $(FUZZ_ITERATIONS) --mode $(FUZZ_MODE) $(FUZZ_ARGS)

coverage:
	@-$(RMDIR) $(OBJ_DIR)
	@-$(RMDIR) $(OBJ_DIR_DEBUG)
	@-$(RMDIR) $(OBJ_DIR_TEST)
	@-$(RM) $(TARGET)
	@-$(RM) $(NAME_DEBUG)
	@-$(RM) $(TEST_NAME)
	$(MAKE) tests OPT_LEVEL=0 COVERAGE=1
	$(MAKE) test COVERAGE=1
	$(PYTHON) $(COVERAGE_SCRIPT) --object-dir $(OBJ_DIR) --threshold-lines $(COVERAGE_LINE_THRESHOLD) --threshold-branches $(COVERAGE_BRANCH_THRESHOLD)

ci-coverage:
	$(MAKE) coverage

ci-build:
	$(MAKE) fclean
	$(MAKE) all OPT_LEVEL=2
	$(MAKE) debug

ci-test:
	$(MAKE) test

ci-lint: lint

ci:
	$(MAKE) ci-build
	$(MAKE) ci-test
	$(MAKE) ci-lint
	$(MAKE) ci-coverage

.PHONY: all dirs clean fclean re debug both re_both tests test lint fuzz coverage ci-build ci-test ci-lint ci-coverage ci
