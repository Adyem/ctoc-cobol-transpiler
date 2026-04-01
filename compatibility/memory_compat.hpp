#ifndef COMPATIBILITY_MEMORY_COMPAT_HPP
#define COMPATIBILITY_MEMORY_COMPAT_HPP

#include <cstdlib>

inline void *cma_calloc(size_t count, size_t size)
{
    return (std::calloc(count, size));
}

inline void cma_free(void *pointer)
{
    std::free(pointer);
}

#endif
