SHELL := /bin/bash

ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
else
    EXE_EXT :=
endif

NAME        = ctoc_cobol_transpiler$(EXE_EXT)
NAME_DEBUG  = ctoc_cobol_transpiler_debug$(EXE_EXT)
TEST_NAME   = automated_tests$(EXE_EXT)
PYTHON      ?= python3
LINT_SCRIPT  = scripts/lint_sources.py

SRC         = main.cpp runtime_scalar.cpp runtime_string.cpp runtime_collation.cpp runtime_encoding.cpp runtime_record.cpp runtime_file.cpp runtime_memory.cpp runtime_csv.cpp runtime_sort.cpp lexer.cpp lexer_token.cpp ast.cpp parser.cpp transpiler_diagnostics.cpp transpiler_context.cpp transpiler_pipeline.cpp transpiler_cblc.cpp transpiler_cli.cpp cblc_formatter.cpp transpiler_logging.cpp transpiler_validation.cpp transpiler_semantics.cpp transpiler_semantics_errors.cpp transpiler_semantics_helpers.cpp transpiler_semantics_scope.cpp transpiler_semantics_classifiers.cpp transpiler_semantics_conditions.cpp transpiler_semantics_assignments.cpp transpiler_semantics_statements.cpp transpiler_codegen.cpp transpiler_cobol_types.cpp transpiler_cobol_procedure.cpp transpiler_cobol_reverse.cpp transpiler_standard_library.cpp transpiler_standard_library_state.cpp transpiler_standard_library_strlen.cpp transpiler_standard_library_strlen_string.cpp transpiler_standard_library_strnlen.cpp transpiler_standard_library_strnlen_string.cpp transpiler_standard_library_strcmp.cpp transpiler_standard_library_strcmp_string.cpp transpiler_standard_library_strcpy.cpp transpiler_standard_library_strcpy_string.cpp transpiler_standard_library_strncpy.cpp transpiler_standard_library_strncpy_string.cpp transpiler_standard_library_memcmp.cpp transpiler_standard_library_memcmp_string.cpp transpiler_standard_library_strcat.cpp transpiler_standard_library_strcat_string.cpp transpiler_standard_library_strtod.cpp transpiler_standard_library_strtod_string.cpp transpiler_standard_library_abs.cpp transpiler_standard_library_fabs.cpp transpiler_standard_library_floor.cpp transpiler_standard_library_ceil.cpp transpiler_standard_library_exp.cpp transpiler_standard_library_log.cpp transpiler_standard_library_sin.cpp transpiler_standard_library_cos.cpp transpiler_standard_library_tan.cpp transpiler_standard_library_rounded.cpp transpiler_standard_library_banker_round.cpp transpiler_standard_library_date_yyyymmdd.cpp transpiler_standard_library_date_duration.cpp transpiler_standard_library_atoi.cpp transpiler_standard_library_atoi_string.cpp transpiler_standard_library_atol.cpp transpiler_standard_library_atol_string.cpp transpiler_standard_library_atoll.cpp transpiler_standard_library_atoll_string.cpp transpiler_standard_library_powerof.cpp transpiler_standard_library_sqrt.cpp transpiler_standard_library_min.cpp transpiler_standard_library_max.cpp transpiler_standard_library_toupper.cpp transpiler_standard_library_toupper_string.cpp transpiler_standard_library_tolower.cpp transpiler_standard_library_tolower_string.cpp transpiler_standard_library_isdigit.cpp transpiler_standard_library_isalpha.cpp

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

CFLAGS = $(COMPILE_FLAGS)

ifeq ($(OS),Windows_NT)
    MKDIR   = mkdir
    RMDIR   = rmdir /S /Q
    RM      = del /F /Q
else
    MKDIR   = mkdir -p
    RMDIR   = rm -rf
    RM      = rm -f
endif

LIBFT_DIR   = ./libft
SUBMODULE_SENTINEL = $(LIBFT_DIR)/Makefile
BUILD_LOG_DIR = ./build_logs
LIBFT_BUILD_LOG = $(BUILD_LOG_DIR)/libft_build.log

OBJ_DIR         = ./objs
OBJ_DIR_DEBUG   = ./objs_debug
OBJ_DIR_TEST    = ./objs_tests

ENABLE_LTO  ?= 0
ENABLE_PGO  ?= 0
export ENABLE_LTO ENABLE_PGO

ifeq ($(ENABLE_LTO),1)
    COMPILE_FLAGS   += -flto
    LDFLAGS  += -flto
endif

ifeq ($(ENABLE_PGO),1)
    COMPILE_FLAGS  += -fprofile-generate
endif

ifeq ($(DEBUG),1)
    CFLAGS    += -DDEBUG=1
    OBJ_DIR    = $(OBJ_DIR_DEBUG)
    TARGET     = $(NAME_DEBUG)
    LIBFT      = $(LIBFT_DIR)/Full_Libft_debug.a
else
    TARGET     = $(NAME)
    LIBFT      = $(LIBFT_DIR)/Full_Libft.a
endif

export COMPILE_FLAGS

ifeq ($(OS),Windows_NT)
    LDFLAGS     = $(LIBFT)
else
    LDFLAGS     = $(LIBFT) -lreadline
endif

OBJS        = $(SRC:%.cpp=$(OBJ_DIR)/%.o)

OBJS_NO_MAIN = $(filter-out $(OBJ_DIR)/main.o,$(OBJS))

.SILENT:

TOTAL_OBJS          := $(words $(OBJS))

TEST_SRC    = tests/test_main.cpp \
              tests/test_support.cpp \
              tests/ast_tests.cpp \
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
              tests/pipeline_tests.cpp \
              tests/validation_tests.cpp \
              tests/transpiler_context_tests.cpp \
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
              tests/cli_tests.cpp \
              tests/ci_tests.cpp \
              tests/codegen_tests.cpp \
              tests/round_trip_tests.cpp \
              tests/cobol_type_tests.cpp \
              tests/compiler/compiler_test_support.cpp \
              tests/compiler/c/build_tests.cpp \
              tests/compiler/c/exit_tests.cpp \
              tests/compiler/c/tests.cpp \
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
              tests/cblc_formatter_tests.cpp

TEST_OBJS   = $(TEST_SRC:%.cpp=$(OBJ_DIR_TEST)/%.o)

TOTAL_TEST_OBJS     := $(words $(TEST_OBJS))

all: ensure_libft dirs $(TARGET)

tests: ensure_libft dirs $(TEST_NAME)

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
	$(MAKE) initialize
	$(MAKE) tests
	$(MAKE) test

debug:
	$(MAKE) all DEBUG=1

$(TARGET): $(LIBFT) $(OBJS)
	@printf '\033[1;36m[CTOC BUILD] Linking %s\033[0m\n' "$@"
	@$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS)

$(TEST_NAME): $(LIBFT) $(TEST_OBJS) $(OBJS_NO_MAIN)
	@printf '\033[1;36m[CTOC BUILD] Linking %s\033[0m\n' "$@"
	@$(CC) $(CFLAGS) $(TEST_OBJS) $(OBJS_NO_MAIN) -o $@ $(LDFLAGS)


LIBFT_BUILD_GOAL := $(if $(DEBUG),debug,all)

$(LIBFT): ensure_libft | $(BUILD_LOG_DIR)
	@need_build=0; \
	if $(MAKE) -C $(LIBFT_DIR) -q $(LIBFT_BUILD_GOAL); then \
	:; \
	else \
	status=$$?; \
	if [ $$status -eq 1 ]; then \
	need_build=1; \
	else \
	exit $$status; \
	fi; \
	fi; \
        if [ $$need_build -eq 1 ] || [ ! -f $@ ]; then \
        printf '\033[1;36m[LIBFT BUILD] Updating %s (log: %s)\033[0m\n' "$@" "$(LIBFT_BUILD_LOG)"; \
        { $(MAKE) -C $(LIBFT_DIR) $(LIBFT_BUILD_GOAL); } 2>&1 | tee $(LIBFT_BUILD_LOG); \
        status=$${PIPESTATUS[0]}; \
        if [ $$status -ne 0 ]; then \
        printf 'libft build failed. Showing log:\n'; \
        cat $(LIBFT_BUILD_LOG); \
        exit $$status; \
        fi; \
        else \
        printf '\033[1;36m[LIBFT CHECK] %s is up to date\033[0m\n' "$@"; \
        fi

initialize:
	git submodule update --init --recursive --force

ensure_libft:
	@if [ ! -f $(SUBMODULE_SENTINEL) ]; then \
		printf '\033[1;36m[LIBFT SETUP] Initializing git submodule\033[0m\n'; \
		$(MAKE) initialize; \
		if [ ! -f $(SUBMODULE_SENTINEL) ]; then \
			printf 'Failed to initialize libft submodule. Please check git submodule configuration.\n'; \
			exit 1; \
		fi; \
	fi

$(OBJ_DIR)/%.o: %.cpp
	@-$(MKDIR) $(dir $@)
	@$(CC) $(CFLAGS) -c $< -o $@
	@if [ $(TOTAL_OBJS) -gt 0 ]; then \
		built=$$(find $(OBJ_DIR) -type f -name '*.o' | wc -l); \
		printf '\033[1;36m[CTOC PROGRESS] %s (%d/%d)\033[0m\n' "$<" $$built $(TOTAL_OBJS); \
	fi

$(OBJ_DIR_TEST)/%.o: %.cpp
	@-$(MKDIR) $(dir $@)
	@$(CC) $(CFLAGS) -c $< -o $@
	@if [ $(TOTAL_TEST_OBJS) -gt 0 ]; then \
		built=$$(find $(OBJ_DIR_TEST) -type f -name '*.o' | wc -l); \
		printf '\033[1;36m[CTOC TEST PROGRESS] %s (%d/%d)\033[0m\n' "$<" $$built $(TOTAL_TEST_OBJS); \
	fi

clean:
	-$(RM) $(OBJ_DIR)/*.o $(OBJ_DIR_DEBUG)/*.o
	-$(RM) $(OBJ_DIR_TEST)/*.o $(OBJ_DIR_TEST)/tests/*.o
	-$(RM) test_example_compiler.c test_example_compiler.bin test_example_compiler.txt
	-$(RM) test_example_invalid_compiler.c test_example_invalid_compiler.bin test_example_invalid_compiler.log
	-$(RM) test_runtime_file.txt
	-$(RM) $(LIBFT_BUILD_LOG)
	-$(RMDIR) $(BUILD_LOG_DIR)
	@if [ -f $(SUBMODULE_SENTINEL) ]; then \
		$(MAKE) -C $(LIBFT_DIR) fclean; \
	else \
		printf 'Skipping libft clean because the submodule is not initialized. Run "make initialize" to set it up.\n'; \
	fi

fclean: clean
	-$(RM) $(NAME) $(NAME_DEBUG) $(TEST_NAME)
	-$(RMDIR) $(OBJ_DIR) $(OBJ_DIR_DEBUG) $(OBJ_DIR_TEST) data

re: fclean all

test: $(TEST_NAME)
	./$(TEST_NAME)

both: all debug

re_both: re both

lint:
	$(PYTHON) $(LINT_SCRIPT)

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

.PHONY: all dirs clean fclean re debug both re_both tests test initialize ensure_libft lint ci-build ci-test ci-lint ci
