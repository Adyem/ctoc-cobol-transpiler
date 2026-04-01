#ifndef COMPATIBILITY_LIBFT_COMPAT_HPP
#define COMPATIBILITY_LIBFT_COMPAT_HPP

#include <cerrno>
#include <cctype>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#define FT_SUCCESS 0
#define FT_FAILURE 1

#define FT_INT_MAX INT_MAX
#define FT_INT_MIN INT_MIN

inline void ft_bzero(void *buffer, size_t length)
{
    (void)std::memset(buffer, 0, length);
}

inline size_t ft_strlcpy(char *destination, const char *source, size_t destination_size)
{
    size_t source_length;
    size_t copy_length;

    source_length = (source ? std::strlen(source) : 0);
    if (!destination || destination_size == 0)
        return (source_length);
    if (!source)
    {
        destination[0] = '\0';
        return (0);
    }
    copy_length = source_length;
    if (copy_length >= destination_size)
        copy_length = destination_size - 1;
    if (copy_length > 0)
        (void)std::memcpy(destination, source, copy_length);
    destination[copy_length] = '\0';
    return (source_length);
}

inline size_t ft_strlcat(char *destination, const char *source, size_t destination_size)
{
    size_t destination_length;
    size_t source_length;
    size_t copy_length;

    if (!destination && destination_size == 0)
        return (source ? std::strlen(source) : 0);
    destination_length = 0;
    while (destination && destination_length < destination_size && destination[destination_length] != '\0')
        destination_length += 1;
    source_length = (source ? std::strlen(source) : 0);
    if (destination_length == destination_size)
        return (destination_size + source_length);
    if (!source)
        return (destination_length);
    copy_length = source_length;
    if (destination_length + copy_length >= destination_size)
        copy_length = destination_size - destination_length - 1;
    if (copy_length > 0)
        (void)std::memcpy(destination + destination_length, source, copy_length);
    destination[destination_length + copy_length] = '\0';
    return (destination_length + source_length);
}

inline int ft_validate_int(const char *text)
{
    char *end;
    long long value;

    if (!text || text[0] == '\0')
        return (FT_FAILURE);
    errno = 0;
    value = std::strtoll(text, &end, 10);
    if (errno == ERANGE)
        return (FT_FAILURE);
    if (!end || *end != '\0')
        return (FT_FAILURE);
    if (value < static_cast<long long>(INT_MIN))
        return (FT_FAILURE);
    if (value > static_cast<long long>(INT_MAX))
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

inline char *ft_strnstr(char *haystack, const char *needle, size_t length)
{
    size_t needle_length;
    size_t index;

    if (!haystack || !needle)
        return (NULL);
    if (needle[0] == '\0')
        return (haystack);
    needle_length = std::strlen(needle);
    if (needle_length > length)
        return (NULL);
    index = 0;
    while (haystack[index] != '\0' && index + needle_length <= length)
    {
        if (std::memcmp(haystack + index, needle, needle_length) == 0)
            return (haystack + index);
        index += 1;
    }
    return (NULL);
}

inline const char *ft_strnstr(const char *haystack, const char *needle, size_t length)
{
    return (ft_strnstr(const_cast<char *>(haystack), needle, length));
}

#endif
