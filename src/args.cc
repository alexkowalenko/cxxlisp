//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "args.hh"

#include <boost/format.hpp>
#include <sstream>

namespace ax {
optional<string> checkArgs(const ArgConstraint& cons, const string& name, const List& args)
{
    switch (cons.constraint) {
    case ArgConstraintType::none:
        if (args.size() != 0) {
            ostringstream os;
            os << boost::format("%1% expects no arguments") % name;
            return os.str();
        }
        break;
    case ArgConstraintType::eq:
        if (args.size() != cons.num) {
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
        if (args.size() < cons.num) {
            ostringstream os;
            os << boost::format("%1% expecting at least %2% arguments") % name % cons.num;
            return os.str();
        }
        break;
    case ArgConstraintType::max:
        if (args.size() > cons.num) {
            ostringstream os;
            os << boost::format("%1% expecting at max %2% arguments") % name % cons.num;
            return os.str();
        }
        break;
    case ArgConstraintType::no_check:;
    }
    if (cons.argType == ArgType::numeric) {
        if (args.size() == 1) {
            if (!is_a<Int>(args[0])) {
                return name + " argument needs to be number";
            }
        } else {
            for (auto x : args) {
                if (!is_a<Int>(x)) {
                    return name + " arguments needs to be number";
                }
            }
        }
    }
    return {};
}
}