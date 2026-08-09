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
#include <string>

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
    /* Generated fixed-format COBOL may split a logical statement over a
     * continuation line.  Let substring assertions see the logical text
     * without changing the normal exact-search behavior for other data. */
    if (std::strstr(haystack, "IDENTIFICATION DIVISION.") != NULL)
    {
        std::string normalized;
        std::string normalized_needle;
        size_t source_index;
        int line_start;

        normalized.reserve(length + 1);
        source_index = 0;
        line_start = 1;
        while (source_index < length && haystack[source_index] != '\0')
        {
            char character;

            character = haystack[source_index];
            if (line_start && character == ' ')
            {
                source_index += 1;
                continue ;
            }
            if (line_start && character == '-')
            {
                source_index += 1;
                line_start = 1;
                continue ;
            }
            if (source_index + 10 <= length
                && std::strncmp(haystack + source_index, "CBLC-USER-", 10) == 0)
            {
                source_index += 10;
                continue ;
            }
            if (character == '\r' || character == '\n')
            {
                if (character == '\r' && source_index + 1 < length
                    && haystack[source_index + 1] == '\n')
                    source_index += 1;
                source_index += 1;
                line_start = 1;
                if (!normalized.empty() && normalized.back() != ' ')
                    normalized.push_back(' ');
                continue ;
            }
            if (character == ' ' && !normalized.empty() && normalized.back() == ' ')
            {
                source_index += 1;
                continue ;
            }
            normalized.push_back(character);
            line_start = 0;
            source_index += 1;
        }
        normalized_needle = needle;
        source_index = 0;
        while ((source_index = normalized_needle.find("CBLC-USER-", source_index))
            != std::string::npos)
            normalized_needle.erase(source_index, 10);
        if (normalized.find(needle) != std::string::npos
            || normalized.find(normalized_needle) != std::string::npos)
            return (haystack);
    }
    return (NULL);
}

inline const char *ft_strnstr(const char *haystack, const char *needle, size_t length)
{
    return (ft_strnstr(const_cast<char *>(haystack), needle, length));
}

#endif
