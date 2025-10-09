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

SRC         = main.cpp runtime_scalar.cpp runtime_string.cpp runtime_record.cpp runtime_file.cpp lexer.cpp lexer_token.cpp ast.cpp parser.cpp transpiler_diagnostics.cpp transpiler_context.cpp transpiler_pipeline.cpp transpiler_cli.cpp transpiler_logging.cpp transpiler_semantics.cpp transpiler_codegen.cpp transpiler_cobol_types.cpp transpiler_cobol_procedure.cpp transpiler_cobol_reverse.cpp transpiler_standard_library.cpp transpiler_standard_library_strlen.cpp transpiler_standard_library_strnlen.cpp transpiler_standard_library_strcmp.cpp transpiler_standard_library_strcpy.cpp transpiler_standard_library_strncpy.cpp transpiler_standard_library_memcmp.cpp transpiler_standard_library_strcat.cpp transpiler_standard_library_strtod.cpp transpiler_standard_library_abs.cpp transpiler_standard_library_fabs.cpp transpiler_standard_library_floor.cpp transpiler_standard_library_ceil.cpp transpiler_standard_library_exp.cpp transpiler_standard_library_log.cpp transpiler_standard_library_atoi.cpp transpiler_standard_library_atol.cpp transpiler_standard_library_atoll.cpp transpiler_standard_library_powerof.cpp transpiler_standard_library_sqrt.cpp transpiler_standard_library_toupper.cpp transpiler_standard_library_tolower.cpp transpiler_standard_library_isdigit.cpp transpiler_standard_library_isalpha.cpp

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

TEST_SRC    = tests/test_main.cpp \
              tests/test_support.cpp \
              tests/ast_tests.cpp \
              tests/lexer_tests.cpp \
              tests/parser_tests.cpp \
             tests/parser_positive_program_tests.cpp \
             tests/parser_positive_control_flow_tests.cpp \
             tests/parser_positive_literal_tests.cpp \
             tests/parser_negative_statement_tests.cpp \
             tests/parser_negative_structure_tests.cpp \
             tests/parser_negative_data_tests.cpp \
              tests/semantics_tests.cpp \
              tests/runtime_int_tests.cpp \
              tests/runtime_char_tests.cpp \
              tests/runtime_string_tests.cpp \
              tests/standard_library_tests.cpp \
              tests/runtime_audit_tests.cpp \
              tests/runtime_record_tests.cpp \
              tests/runtime_file_tests.cpp \
              tests/pipeline_tests.cpp \
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
              tests/compiler/cobol/record_summary_tests.cpp \
              tests/compiler/cobol/reverse_control_flow_tests.cpp \
              tests/compiler/cobol/reverse_normalization_tests.cpp \
              tests/compiler/cobol/reverse_cli_tests.cpp \
              tests/compiler/cobol/tests.cpp \
              tests/compiler/tests.cpp \
              tests/logging_tests.cpp \
              tests/cobol_reverse_tests.cpp

TEST_OBJS   = $(TEST_SRC:%.cpp=$(OBJ_DIR_TEST)/%.o)

all: ensure_libft dirs $(TARGET)

tests: ensure_libft dirs $(TEST_NAME)

dirs:
	-$(MKDIR) $(OBJ_DIR)
	-$(MKDIR) $(OBJ_DIR_DEBUG)
	-$(MKDIR) $(OBJ_DIR_TEST)

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
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS)

$(TEST_NAME): $(LIBFT) $(TEST_OBJS) $(OBJS_NO_MAIN)
	$(CC) $(CFLAGS) $(TEST_OBJS) $(OBJS_NO_MAIN) -o $@ $(LDFLAGS)


$(LIBFT): ensure_libft
	$(MAKE) -C $(LIBFT_DIR) $(if $(DEBUG), debug)

initialize:
	git submodule update --init --recursive

ensure_libft:
	@if [ ! -f $(SUBMODULE_SENTINEL) ]; then \
		printf 'The libft submodule is not initialized. Please run "make initialize" before building.\n'; \
		exit 1; \
	fi

$(OBJ_DIR)/%.o: %.cpp
	-$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ_DIR_TEST)/%.o: %.cpp
	-$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-$(RM) $(OBJ_DIR)/*.o $(OBJ_DIR_DEBUG)/*.o
	-$(RM) $(OBJ_DIR_TEST)/*.o $(OBJ_DIR_TEST)/tests/*.o
	-$(RM) test_example_compiler.c test_example_compiler.bin test_example_compiler.txt
	-$(RM) test_example_invalid_compiler.c test_example_invalid_compiler.bin test_example_invalid_compiler.log
	-$(RM) test_runtime_file.txt
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
