#include "runtime_scalar.hpp"
#include "libft/Printf/printf.hpp"

static int runtime_check_destination_int(t_runtime_int *destination)
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
    int result;

    if (!buffer)
        return (FT_FAILURE);
    if (buffer_size == 0)
        return (FT_FAILURE);
    result = pf_snprintf(buffer, buffer_size, "%d", value.value);
    if (result < 0)
        return (FT_FAILURE);
    if (static_cast<size_t>(result) >= buffer_size)
        return (FT_FAILURE);
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
    int length;

    if (runtime_check_destination_char(destination) != FT_SUCCESS)
        return (FT_FAILURE);
    if (!text)
        return (FT_FAILURE);
    length = ft_strlen(text);
    if (length <= 0)
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
