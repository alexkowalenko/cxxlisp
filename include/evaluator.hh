//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EVALUATOR_HH
#define EVALUATOR_HH

#include "expr.hh"

namespace ax {

class Evaluator {

public:
    static Expr eval(Expr& e);

private:
    static List eval_list(const List& l);
};
}

#endif