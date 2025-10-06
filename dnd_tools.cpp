#include "dnd_tools.hpp"

#include <iostream>

std::string build_greeting(const std::string &name)
{
    return "Welcome to D&D Tools, " + name + "!";
}

void run_demo()
{
    std::cout << build_greeting("Adventurer") << '\n';
}
