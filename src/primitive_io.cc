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

Expr quit(List& args)
{
    if (args.size() > 0) {
        if (is_a<Int>(args[0])) {
            long ret = any_cast<Int>(args[0]);
            throw ExceptionQuit(ret);
        }
    }
    throw ExceptionQuit(0);
}
}