//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "evaluator.hh"

#include <boost/log/trivial.hpp>

#include "exceptions.hh"
#include "parser.hh"
#include "primitive.hh"

namespace ax {

List Evaluator::eval_list(const List& l)
{
    List result;
    for (auto e : l) {
        auto r = eval(e);
        result.push_back(r);
    }
    return result;
}

Expr Evaluator::eval(Expr& e)
{
    BOOST_LOG_TRIVIAL(trace) << "eval: " << to_string(e);

    // Eval basic types
    if (is_a<Bool>(e)) {
        return e;
    }
    if (is_a<Int>(e)) {
        return e;
    }
    if (is_a<Atom>(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    // Test for list
    if (!is_a<List>(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    // Expression is a list
    List e_list = any_cast<List>(e);
    if (e_list.size() == 0) {
        return sF;
    }

    auto e_car = e_list.front();
    if (is_a<Atom>(e_car)) {
        // Atom in function position
        Atom name = any_cast<Atom>(e_car);

        // quote
        if (name == quote_atom) {
            if (e_list.size() > 1) {
                return e_list[1];
            }
            return sF;
        }

        // Lookup primitive table
        if (auto prim = prim_table.find(name); prim != prim_table.end()) {
            auto result = List(e_list.begin() + 1, e_list.end());
            if (prim->second.preEval) {
                result = eval_list(result);
            }
            auto check = checkArgs(prim->second.cons, name, result);
            if (check) {
                throw EvalException(*check);
            }
            return prim->second.pf(name, result);
        }
    }

    throw EvalException("Can't evaluate "s + to_string(e));
}
}
