//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "expr.hh"

#include <set>

#include "function.hh"
#include "options.hh"
#include "symboltable.hh"

namespace ax {

class Evaluator {
  public:
    Evaluator(Options &o, SymbolTable &g, std::set<Atom> &tf)
        : globalTable(g), trace_functions(tf), opt(o){};

    void repl(std::istream &in, std::ostream &os);

    Expr eval(const Expr &e, SymbolTable &a);
    Expr perform_list(const Expr &e, SymbolTable &a);
    Expr eval_list(const Expr &e, SymbolTable &a);

    bool has_function(const Atom &f);

    SymbolTable    &globalTable;
    std::set<Atom> &trace_functions;
    Options         opt;

  private:
    SymbolTable create_context(Function &f, Expr &args, SymbolTable &a);
    Expr        perform_function(Function &f, const Expr &args, SymbolTable &a);
    Expr        backquote(Expr s, SymbolTable &a);
};

} // namespace ax