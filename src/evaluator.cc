//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "evaluator.hh"

#include "exceptions.hh"
#include "parser.hh"

namespace ax {

Expr Evaluator::eval(Expr& e)
{
    // Eval basic types
    if (is_a<Bool>(e)) {
        return e;
    } else if (is_a<Atom>(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    // Test for list
    if (!is_a<List>(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    // Expression is a list
    List e_list = as_a<List>(e);
    if (e_list.size() == 0) {
        return sF;
    }

    auto e_car = e_list[0];
    if (is_a<Atom>(e_car)) {
        // Atom in function position
        Atom name = as_a<Atom>(e_car);

        // quote
        if (name == quote_atom) {
            if (e_list.size() > 1) {
                return e_list[1];
            }
            return sF;
        }

        // atom 
        if (name == "atom") {
            if (e_list.size() > 1) {
                auto res = eval(e_list[1]);
                if (is_atomic(res) || is_false(res)) {
                    return sT;
                }
            }
            return sF;
        }

    }

    throw EvalException("Can't evaluate "s + to_string(e));
}
}
