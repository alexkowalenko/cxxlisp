//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef EVALUATOR_HH
#define EVALUATOR_HH

#include "expr.hh"

#include <set>

#include "function.hh"
#include "options.hh"
#include "symboltable.hh"

namespace ax {

class Evaluator {
public:
    Evaluator(Options& o, shared_ptr<SymbolTable> g, set<Atom>& tf)
        : globalTable(g)
        , trace_functions(tf)
        , opt(o){};

    Expr* eval(Expr* const e, shared_ptr<SymbolTable> a);
    Expr* perform_list(const Expr* e, shared_ptr<SymbolTable> a);
    Expr* eval_list(const Expr* e, shared_ptr<SymbolTable> a);

    bool has_function(const Atom& f);

    shared_ptr<SymbolTable> globalTable;
    set<Atom>& trace_functions;

private:
    shared_ptr<SymbolTable> create_context(Function* f, Expr* const args, shared_ptr<SymbolTable> a);
    Expr* perform_function(Function* f, Expr* const args, shared_ptr<SymbolTable> a);
    Expr* backquote(Expr* s, shared_ptr<SymbolTable> a);

    Options opt;
};
}

#endif