//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EVALUATOR_HH
#define EVALUATOR_HH

#include "expr.hh"

#include "function.hh"
#include "options.hh"
#include "symboltable.hh"

namespace ax {

class Evaluator {

public:
    static Expr eval(Expr& e, SymbolTable& a);

    static Expr perform_list(List& l, SymbolTable& a);

    static Options opt;

private:
    static List eval_list(List& l, SymbolTable& a);
    static Expr perform_function(Function& f, List args, SymbolTable& a);
    static Expr backquote(Expr& s, SymbolTable& a);
};
}

#endif