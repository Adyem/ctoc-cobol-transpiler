#ifndef COMPATIBILITY_PRINTF_COMPAT_HPP
#define COMPATIBILITY_PRINTF_COMPAT_HPP

#include <cstdarg>
#include <cstdio>

#if !defined(_WIN32)
#include <unistd.h>
#endif

inline int pf_printf_fd(int fd, const char *format, ...)
{
    int result;
    va_list args;

    va_start(args, format);
#if !defined(_WIN32)
    result = vdprintf(fd, format, args);
#else
    if (fd == 2)
        result = std::vfprintf(stderr, format, args);
    else
        result = std::vfprintf(stdout, format, args);
#endif
    va_end(args);
    return (result);
}

#endif
