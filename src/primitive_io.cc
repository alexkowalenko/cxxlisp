//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include "exceptions.hh"

namespace ax {

//
// I/O functions
//

Expr throw_error(List& args)
{
    throw EvalException(to_string(args[0]));
}
}