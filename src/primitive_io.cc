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

Expr* throw_error(Expr* args)
{
    throw EvalException(to_string(args->car));
}

Expr* quit(Expr* args)
{
    if (!is_false(args)) {
        if (is_a<Type::integer>(args)) {
            long ret = args->integer;
            throw ExceptionQuit(ret);
        }
    }
    throw ExceptionQuit(0);
}
}