//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "function.hh"

namespace ax {

using namespace std;

Function::operator string()
{
    return "λ:"s + name;
}
}