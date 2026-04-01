#include "test_support.hpp"

int main(void)
{
    int status;

    status = imported_test_run_registered();
    if (status != 0)
        return (1);
    return (0);
}
