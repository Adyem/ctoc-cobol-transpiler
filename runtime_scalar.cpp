#include "cblc_transpiler.hpp"
#include "libft/Printf/printf.hpp"
#include <climits>

static int runtime_check_destination_int(t_runtime_int *destination)
{
    if (!destination)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_check_destination_long(t_runtime_long *destination)
{
    if (!destination)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_check_destination_long_long(t_runtime_long_long *destination)
{
    if (!destination)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_check_destination_char(t_runtime_char *destination)
{
    if (!destination)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

void runtime_int_set(t_runtime_int *destination, int value)
{
    if (runtime_check_destination_int(destination) != FT_SUCCESS)
        return ;
    destination->value = value;
}

int runtime_int_from_string(t_runtime_int *destination, const char *text)
{
    if (runtime_check_destination_int(destination) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    if (ft_validate_int(text) != FT_SUCCESS)
        return (FT_FAILURE);
    destination->value = ft_atoi(text);
    return (FT_SUCCESS);
}

static int runtime_int_overflow(long long value)
{
    if (value > static_cast<long long>(FT_INT_MAX))
        return (FT_FAILURE);
    if (value < static_cast<long long>(FT_INT_MIN))
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_long_overflow(long long value)
{
    if (value > static_cast<long long>(LONG_MAX))
        return (FT_FAILURE);
    if (value < static_cast<long long>(LONG_MIN))
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

static int runtime_long_long_overflow(long long value)
{
    if (value > LLONG_MAX)
        return (FT_FAILURE);
    if (value < LLONG_MIN)
        return (FT_FAILURE);
    return (FT_SUCCESS);
}

int runtime_int_add(t_runtime_int left, t_runtime_int right, t_runtime_int *result)
{
    long long accumulator = static_cast<long long>(left.value);

    accumulator += static_cast<long long>(right.value);
    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_int_overflow(accumulator) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<int>(accumulator);
    return (FT_SUCCESS);
}

int runtime_int_subtract(t_runtime_int left, t_runtime_int right, t_runtime_int *result)
{
    long long accumulator = static_cast<long long>(left.value);

    accumulator -= static_cast<long long>(right.value);
    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_int_overflow(accumulator) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<int>(accumulator);
    return (FT_SUCCESS);
}

int runtime_int_multiply(t_runtime_int left, t_runtime_int right, t_runtime_int *result)
{
    long long accumulator;

    accumulator = static_cast<long long>(left.value);
    accumulator *= static_cast<long long>(right.value);
    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (runtime_int_overflow(accumulator) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<int>(accumulator);
    return (FT_SUCCESS);
}

int runtime_int_divide(t_runtime_int dividend, t_runtime_int divisor, t_runtime_int *result)
{
    long long quotient;

    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (divisor.value == 0)
        return (FT_FAILURE);
    quotient = static_cast<long long>(dividend.value) / static_cast<long long>(divisor.value);
    if (runtime_int_overflow(quotient) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<int>(quotient);
    return (FT_SUCCESS);
}

int runtime_int_unary_plus(t_runtime_int value, t_runtime_int *result)
{
    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = value.value;
    return (FT_SUCCESS);
}

int runtime_int_unary_minus(t_runtime_int value, t_runtime_int *result)
{
    long long accumulator;

    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    accumulator = static_cast<long long>(value.value);
    accumulator = -accumulator;
    if (runtime_int_overflow(accumulator) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<int>(accumulator);
    return (FT_SUCCESS);
}

int runtime_int_absolute(t_runtime_int value, t_runtime_int *result)
{
    long long magnitude;

    if (runtime_check_destination_int(result) != FT_SUCCESS)
        return (FT_FAILURE);
    magnitude = static_cast<long long>(value.value);
    if (magnitude < 0)
    {
        magnitude = -magnitude;
        if (runtime_int_overflow(magnitude) != FT_SUCCESS)
            return (FT_FAILURE);
    }
    result->value = static_cast<int>(magnitude);
    return (FT_SUCCESS);
}

void runtime_long_set(t_runtime_long *destination, long value)
{
    if (runtime_check_destination_long(destination) != FT_SUCCESS)
        return ;
    destination->value = value;
}

int runtime_long_add(t_runtime_long left, t_runtime_long right, t_runtime_long *result)
{
    long sum;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_add_overflow(left.value, right.value, &sum))
        return (FT_FAILURE);
    result->value = sum;
    return (FT_SUCCESS);
}

int runtime_long_subtract(t_runtime_long left, t_runtime_long right, t_runtime_long *result)
{
    long difference;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_sub_overflow(left.value, right.value, &difference))
        return (FT_FAILURE);
    result->value = difference;
    return (FT_SUCCESS);
}

int runtime_long_multiply(t_runtime_long left, t_runtime_long right, t_runtime_long *result)
{
    long product;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_mul_overflow(left.value, right.value, &product))
        return (FT_FAILURE);
    result->value = product;
    return (FT_SUCCESS);
}

int runtime_long_divide(t_runtime_long dividend, t_runtime_long divisor, t_runtime_long *result)
{
    long quotient;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (divisor.value == 0)
        return (FT_FAILURE);
    if (dividend.value == LONG_MIN && divisor.value == -1)
        return (FT_FAILURE);
    quotient = dividend.value / divisor.value;
    result->value = quotient;
    return (FT_SUCCESS);
}

int runtime_long_unary_plus(t_runtime_long value, t_runtime_long *result)
{
    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = value.value;
    return (FT_SUCCESS);
}

int runtime_long_unary_minus(t_runtime_long value, t_runtime_long *result)
{
    long long magnitude;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (value.value == LONG_MIN)
        return (FT_FAILURE);
    magnitude = static_cast<long long>(value.value);
    magnitude = -magnitude;
    if (runtime_long_overflow(magnitude) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<long>(magnitude);
    return (FT_SUCCESS);
}

int runtime_long_absolute(t_runtime_long value, t_runtime_long *result)
{
    long long magnitude;

    if (runtime_check_destination_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (value.value == LONG_MIN)
        return (FT_FAILURE);
    magnitude = static_cast<long long>(value.value);
    if (magnitude < 0)
        magnitude = -magnitude;
    if (runtime_long_overflow(magnitude) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = static_cast<long>(magnitude);
    return (FT_SUCCESS);
}

void runtime_long_long_set(t_runtime_long_long *destination, long long value)
{
    if (runtime_check_destination_long_long(destination) != FT_SUCCESS)
        return ;
    destination->value = value;
}

int runtime_long_long_add(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result)
{
    long long sum;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_add_overflow(left.value, right.value, &sum))
        return (FT_FAILURE);
    result->value = sum;
    return (FT_SUCCESS);
}

int runtime_long_long_subtract(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result)
{
    long long difference;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_sub_overflow(left.value, right.value, &difference))
        return (FT_FAILURE);
    result->value = difference;
    return (FT_SUCCESS);
}

int runtime_long_long_multiply(t_runtime_long_long left, t_runtime_long_long right,
    t_runtime_long_long *result)
{
    long long product;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (__builtin_mul_overflow(left.value, right.value, &product))
        return (FT_FAILURE);
    result->value = product;
    return (FT_SUCCESS);
}

int runtime_long_long_divide(t_runtime_long_long dividend, t_runtime_long_long divisor,
    t_runtime_long_long *result)
{
    long long quotient;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (divisor.value == 0)
        return (FT_FAILURE);
    if (dividend.value == LLONG_MIN && divisor.value == -1)
        return (FT_FAILURE);
    quotient = dividend.value / divisor.value;
    result->value = quotient;
    return (FT_SUCCESS);
}

int runtime_long_long_unary_plus(t_runtime_long_long value, t_runtime_long_long *result)
{
    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = value.value;
    return (FT_SUCCESS);
}

int runtime_long_long_unary_minus(t_runtime_long_long value, t_runtime_long_long *result)
{
    long long magnitude;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (value.value == LLONG_MIN)
        return (FT_FAILURE);
    magnitude = value.value;
    magnitude = -magnitude;
    if (runtime_long_long_overflow(magnitude) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = magnitude;
    return (FT_SUCCESS);
}

int runtime_long_long_absolute(t_runtime_long_long value, t_runtime_long_long *result)
{
    long long magnitude;

    if (runtime_check_destination_long_long(result) != FT_SUCCESS)
        return (FT_FAILURE);
    if (value.value == LLONG_MIN)
        return (FT_FAILURE);
    magnitude = value.value;
    if (magnitude < 0)
        magnitude = -magnitude;
    if (runtime_long_long_overflow(magnitude) != FT_SUCCESS)
        return (FT_FAILURE);
    result->value = magnitude;
    return (FT_SUCCESS);
}

int runtime_int_compare(t_runtime_int left, t_runtime_int right)
{
    if (left.value < right.value)
        return (-1);
    if (left.value > right.value)
        return (1);
    return (0);
}

int runtime_int_to_string(t_runtime_int value, char *buffer, size_t buffer_size)
{
    char temporary[32];
    int result;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    result = pf_snprintf(temporary, sizeof(temporary), "%d", value.value);
    if (result < 0)
        return (FT_FAILURE);
    if (static_cast<size_t>(result) >= buffer_size)
        return (FT_FAILURE);
    ft_strlcpy(buffer, temporary, buffer_size);
    return (FT_SUCCESS);
}

void runtime_char_set(t_runtime_char *destination, char value)
{
    if (runtime_check_destination_char(destination) != FT_SUCCESS)
        return ;
    destination->value = value;
}

int runtime_char_from_string(t_runtime_char *destination, const char *text)
{
    size_t length;

    if (runtime_check_destination_char(destination) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (length != 1)
        return (FT_FAILURE);
    destination->value = text[0];
    return (FT_SUCCESS);
}

static void runtime_apply_unary_char(t_runtime_char *value, void (*transform)(char *))
{
    char buffer[2];

    if (runtime_check_destination_char(value) != FT_SUCCESS)
        return ;
    if (!transform)
        return ;
    buffer[0] = value->value;
    buffer[1] = '\0';
    transform(buffer);
    value->value = buffer[0];
}

void runtime_char_to_upper(t_runtime_char *value)
{
    runtime_apply_unary_char(value, ft_to_upper);
}

void runtime_char_to_lower(t_runtime_char *value)
{
    runtime_apply_unary_char(value, ft_to_lower);
}

int runtime_char_compare(t_runtime_char left, t_runtime_char right)
{
    if (left.value < right.value)
        return (-1);
    if (left.value > right.value)
        return (1);
    return (0);
}

int runtime_char_to_string(t_runtime_char value, char *buffer, size_t buffer_size)
{
    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size < 2)
        return (FT_FAILURE);
    buffer[0] = value.value;
    buffer[1] = '\0';
    return (FT_SUCCESS);
}

static void runtime_demo_ints(void)
{
    t_runtime_int value_a;
    t_runtime_int value_b;
    t_runtime_int addition;
    t_runtime_int subtraction;
    char buffer[32];

    runtime_int_set(&value_a, 42);
    runtime_int_from_string(&value_b, "100");
    runtime_int_add(value_a, value_b, &addition);
    runtime_int_subtract(value_b, value_a, &subtraction);
    runtime_int_to_string(addition, buffer, sizeof(buffer));
    pf_printf("int add: %s\n", buffer);
    runtime_int_to_string(subtraction, buffer, sizeof(buffer));
    pf_printf("int sub: %s\n", buffer);
    pf_printf("int cmp: %d\n", runtime_int_compare(value_a, value_b));
}

static void runtime_demo_chars(void)
{
    t_runtime_char char_a;
    t_runtime_char char_b;
    char buffer[8];

    runtime_char_from_string(&char_a, "a");
    runtime_char_set(&char_b, 'Z');
    runtime_char_to_lower(&char_b);
    runtime_char_to_upper(&char_a);
    runtime_char_to_string(char_a, buffer, sizeof(buffer));
    pf_printf("char A: %s\n", buffer);
    runtime_char_to_string(char_b, buffer, sizeof(buffer));
    pf_printf("char B: %s\n", buffer);
    pf_printf("char cmp: %d\n", runtime_char_compare(char_a, char_b));
}

void runtime_demo(void)
{
    runtime_demo_ints();
    runtime_demo_chars();
}
