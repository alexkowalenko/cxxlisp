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
    Evaluator(Options& o, shared_ptr<SymbolTable> g)
        : globalTable(g)
        , opt(o){};

    Expr* eval(Expr* const e, shared_ptr<SymbolTable> a);
    Expr* perform_list(Expr* const e, shared_ptr<SymbolTable> a);
    Expr* eval_list(const Expr* e, shared_ptr<SymbolTable> a);

    shared_ptr<SymbolTable> globalTable;

private:
    //     SymbolTable create_context(Function& f, List args, SymbolTable& a);
    //     Expr perform_function(Function& f, List args, SymbolTable& a);
    //     Expr backquote(Expr& s, SymbolTable& a);

    Options opt;
};
}

#endif