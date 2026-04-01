#include "test_runner.hpp"

#include <csignal>
#include <cstdio>
#include <cstring>
#include <execinfo.h>
#include <unistd.h>

typedef struct s_imported_test_case
{
    t_imported_test_func func;
    const char *description;
    const char *module;
}   t_imported_test_case;

static void imported_test_signal_handler(int signal_number)
{
    void *frames[64];
    int frame_count;

    frame_count = backtrace(frames, 64);
    backtrace_symbols_fd(frames, frame_count, STDERR_FILENO);
    signal(signal_number, SIG_DFL);
    raise(signal_number);
    return ;
}

static int *imported_test_get_count(void)
{
    static int test_count = 0;

    return (&test_count);
}

static int imported_test_get_capacity(void)
{
    return (4096);
}

static const char *imported_test_success_label(void)
{
    return ("\033[32mSuccess\033[0m");
}

static const char *imported_test_failure_label(void)
{
    return ("\033[31mFailure\033[0m");
}

static t_imported_test_case *imported_test_get_cases(void)
{
    static t_imported_test_case tests[4096];

    return (tests);
}

static void imported_test_swap_cases(t_imported_test_case *left, t_imported_test_case *right)
{
    t_imported_test_case temp;

    temp = *left;
    *left = *right;
    *right = temp;
    return ;
}

static void imported_test_sort(void)
{
    t_imported_test_case *tests;
    int *test_count;
    int outer_index;
    int inner_index;

    tests = imported_test_get_cases();
    test_count = imported_test_get_count();
    outer_index = 0;
    while (outer_index < *test_count)
    {
        inner_index = outer_index + 1;
        while (inner_index < *test_count)
        {
            if (std::strcmp(tests[inner_index].module, tests[outer_index].module) < 0)
                imported_test_swap_cases(&tests[outer_index], &tests[inner_index]);
            inner_index += 1;
        }
        outer_index += 1;
    }
    return ;
}

int imported_test_register(t_imported_test_func func, const char *description, const char *module)
{
    t_imported_test_case *tests;
    int *test_count;

    tests = imported_test_get_cases();
    test_count = imported_test_get_count();
    if (*test_count >= imported_test_get_capacity())
        return (1);
    tests[*test_count].func = func;
    tests[*test_count].description = description;
    tests[*test_count].module = module;
    *test_count += 1;
    return (0);
}

void imported_test_fail(const char *expression, const char *file, int line)
{
    FILE *log_file;

    log_file = std::fopen("imported_test_failures.log", "a");
    if (log_file)
    {
        std::fprintf(log_file, "%s:%d: %s\n", file, line, expression);
        std::fclose(log_file);
    }
    return ;
}

int imported_test_run_registered(void)
{
    FILE *log_file;
    t_imported_test_case *tests;
    int *test_count;
    int total_tests;
    int output_is_terminal;
    int index;
    int passed;

    log_file = std::fopen("imported_test_failures.log", "w");
    if (log_file)
        std::fclose(log_file);
    signal(SIGSEGV, imported_test_signal_handler);
    signal(SIGABRT, imported_test_signal_handler);
    imported_test_sort();
    tests = imported_test_get_cases();
    test_count = imported_test_get_count();
    total_tests = *test_count;
    output_is_terminal = isatty(STDOUT_FILENO);
    index = 0;
    passed = 0;
    while (index < total_tests)
    {
        if (output_is_terminal)
            std::printf("Running test %d \"%s\"", index + 1, tests[index].description);
        else
            std::printf("Running test %d \"%s\"\n", index + 1, tests[index].description);
        std::fflush(stdout);
        if (tests[index].func())
        {
            if (output_is_terminal)
                std::printf("\r\033[K");
            std::printf("%s %d %s\n", imported_test_success_label(),
                index + 1, tests[index].description);
            std::fflush(stdout);
            passed += 1;
        }
        else
        {
            if (output_is_terminal)
                std::printf("\r\033[K");
            std::printf("%s %d %s\n", imported_test_failure_label(),
                index + 1, tests[index].description);
            std::fflush(stdout);
        }
        index += 1;
    }
    std::printf("%d/%d tests passed\n", passed, total_tests);
    std::fflush(stdout);
    if (passed != total_tests)
        return (1);
    return (0);
}
