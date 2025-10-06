ifeq ($(OS),Windows_NT)
    EXE_EXT := .exe
else
    EXE_EXT :=
endif

NAME        = ctoc_cobol_transpiler$(EXE_EXT)
NAME_DEBUG  = ctoc_cobol_transpiler_debug$(EXE_EXT)
TEST_NAME   = automated_tests$(EXE_EXT)

SRC         = main.cpp runtime_scalar.cpp transpiler_diagnostics.cpp transpiler_context.cpp transpiler_pipeline.cpp

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

COMPILE_FLAGS = -Wall -Werror -Wextra -std=c++17 -Wmissing-declarations                 -Wold-style-cast -Wshadow -Wconversion -Wformat=2 -Wundef                 -Wfloat-equal -Wconversion -Wodr -Wuseless-cast                 -Wzero-as-null-pointer-constant -Wmaybe-uninitialized $(OPT_FLAGS)

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

TEST_SRC    = tests/runtime_scalar_tests.cpp

TEST_OBJS   = $(TEST_SRC:%.cpp=$(OBJ_DIR_TEST)/%.o)

all: ensure_libft dirs $(TARGET)

tests: ensure_libft dirs $(TEST_NAME)

dirs:
	-$(MKDIR) $(OBJ_DIR)
	-$(MKDIR) $(OBJ_DIR_DEBUG)
	-$(MKDIR) $(OBJ_DIR_TEST)

debug:
	$(MAKE) all DEBUG=1

$(TARGET): $(LIBFT) $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@ $(LDFLAGS)

$(TEST_NAME): $(LIBFT) $(TEST_OBJS)
	$(CC) $(CFLAGS) $(TEST_OBJS) -o $@ $(LDFLAGS)

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
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJ_DIR_TEST)/%.o: %.cpp
	-$(MKDIR) $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-$(RM) $(OBJ_DIR)/*.o $(OBJ_DIR_DEBUG)/*.o
	-$(RM) $(OBJ_DIR_TEST)/*.o $(OBJ_DIR_TEST)/tests/*.o
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

.PHONY: all dirs clean fclean re debug both re_both tests test initialize ensure_libft
