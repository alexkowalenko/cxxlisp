//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "args.hh"

#include <boost/format.hpp>
#include <sstream>

namespace ax {

template <Type t>
optional<string> checkType(const string& name, const Expr* args, const string& tname)
{
    if (size_list(args) == 1) {
        if (!is_a<t>(args->car)) {
            return name + " argument needs to be a " + tname;
        }
    } else {
        while (args) {
            if (!(is_a<t>(args->car))) {
                return name + " arguments needs to be a " + tname;
            }
            args = args->cdr;
        }
    }
    return {};
}

optional<string> checkTypeNumeric(const string& name, const Expr* args, const string& tname)
{
    if (is_false(args)) {
        return {};
    } else if (size_list(args) == 1) {
        if (!(is_a<Type::integer>(args->car) /*|| is_a<Float>(args->car) */)) {
            return name + " argument needs to be a " + tname;
        }
    } else {
        while (args) {
            if (!(is_a<Type::integer>(args->car) /* || is_a<Float>(x) */)) {
                return name + " arguments needs to be a " + tname;
            }
            args = args->cdr;
        }
    }
    return {};
}

optional<string> checkArgs(const ArgConstraint& cons, const string& name, const Expr* args)
{
    auto size = size_list(args);
    switch (cons.constraint) {
    case ArgConstraintType::none:
        if (size != 0) {
            ostringstream os;
            os << boost::format("%1% expects no arguments") % name;
            return os.str();
        }
        break;
    case ArgConstraintType::eq:
        if (size != cons.num) {
            ostringstream os;
            if (cons.num == 1) {
                os << boost::format("%1% expecting an argument") % name;
            } else {
                os << boost::format("%1% expecting %2% arguments") % name % cons.num;
            }
            return os.str();
        }
        break;
    case ArgConstraintType::min:
        if (size < cons.num) {
            ostringstream os;
            os << boost::format("%1% expecting at least %2% arguments") % name % cons.num;
            return os.str();
        }
        break;
    case ArgConstraintType::max:
        if (size > cons.num) {
            ostringstream os;
            os << boost::format("%1% expecting at max %2% arguments") % name % cons.num;
            return os.str();
        }
        break;
    case ArgConstraintType::no_check:;
    }
    if (cons.argType == ArgType::numeric) {
        return checkTypeNumeric(name, args, "number");
    } else if (cons.argType == ArgType::integer) {
        return checkType<Type::integer>(name, args, "integer");
    } else if (cons.argType == ArgType::string) {
        return checkType<Type::string>(name, args, "string");
    } else if (cons.argType == ArgType::character) {
        return checkType<Type::character>(name, args, "character");
    }
    return {};
}
}