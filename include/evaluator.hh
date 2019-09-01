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
    Evaluator(Options& o)
        : opt(o){};

    Expr* eval(Expr* const e, SymbolTable& a);
    //     Expr perform_list(List& l, SymbolTable& a);
    //     List eval_list(List& l, SymbolTable& a);

    // private:
    //     SymbolTable create_context(Function& f, List args, SymbolTable& a);
    //     Expr perform_function(Function& f, List args, SymbolTable& a);
    //     Expr backquote(Expr& s, SymbolTable& a);

    Options opt;
};
}

#endif