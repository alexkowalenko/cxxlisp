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

Expr* print(const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    Stream* output;
    if (args->size() == 1) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(arg1(args)) && arg1(args)->stream->is_output()) {
            output = arg1(args)->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }

    if (name == "prin1" || name == "print") {
        if (name == "print") {
            get<ostream*>(output->str)->put('\n').flush();
        }
        auto str = to_string(args->car);
        get<ostream*>(output->str)->write(str.c_str(), str.size());
    } else {
        auto str = to_pstring(args->car);
        get<ostream*>(output->str)->write(str.c_str(), str.size());
    }
    return args->car;
}

Expr* terpri(const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    Stream* output;
    if (args->size() == 0) {
        output = get_reference(name, std_out, a)->stream;
    } else {
        if (is_a<Type::stream>(args->car) && args->car->stream->is_output()) {
            output = args->car->stream;
        } else {
            throw EvalException(name + ": argument is not an output stream");
        }
    }
    get<ostream*>(output->str)->put('\n').flush();
    return sF;
}
}