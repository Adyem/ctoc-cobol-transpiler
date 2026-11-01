#ifndef COMPATIBILITY_MEMORY_COMPAT_HPP
#define COMPATIBILITY_MEMORY_COMPAT_HPP

#include <cstddef>
#include <cstdlib>
#include <limits>

namespace cma_memory_compat_detail
{
    struct t_allocation_test_state
    {
        size_t fail_on_attempt;
        size_t attempt_count;
        size_t failure_count;
    };

    inline t_allocation_test_state &allocation_test_state(void)
    {
        static thread_local t_allocation_test_state state = {0, 0, 0};

        return (state);
    }
}

/* Fail exactly one allocation attempt in the current thread. This keeps
 * allocation-failure campaigns deterministic even when tests run in
 * parallel; the next allocation after the injected failure proceeds normally.
 */
inline void cma_allocation_test_fail_on_attempt(size_t attempt)
{
    cma_memory_compat_detail::t_allocation_test_state &state =
        cma_memory_compat_detail::allocation_test_state();

    state.fail_on_attempt = attempt;
    state.attempt_count = 0;
}

inline size_t cma_allocation_failure_count(void)
{
    return (cma_memory_compat_detail::allocation_test_state().failure_count);
}

inline void cma_allocation_test_disable(void)
{
    cma_memory_compat_detail::t_allocation_test_state &state =
        cma_memory_compat_detail::allocation_test_state();

    state.fail_on_attempt = 0;
    state.attempt_count = 0;
}

inline void *cma_calloc(size_t count, size_t size)
{
    if (size != 0 && count > std::numeric_limits<size_t>::max() / size)
    {
        cma_memory_compat_detail::allocation_test_state().failure_count += 1;
        return (NULL);
    }
    cma_memory_compat_detail::t_allocation_test_state &state =
        cma_memory_compat_detail::allocation_test_state();

    if (state.fail_on_attempt != 0)
    {
        state.attempt_count += 1;
        if (state.attempt_count == state.fail_on_attempt)
        {
            cma_allocation_test_disable();
            state.failure_count += 1;
            return (NULL);
        }
    }
    void *allocation;

    allocation = std::calloc(count, size);
    if (!allocation && count != 0 && size != 0)
        state.failure_count += 1;
    return (allocation);
}

inline void cma_free(void *pointer)
{
    std::free(pointer);
}

#endif
