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
    Expr eval(Expr& e);
};
}

#endif