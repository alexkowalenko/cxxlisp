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
    Evaluator(Options &o, std::shared_ptr<SymbolTable> g, std::set<Atom> &tf)
        : globalTable(g), trace_functions(tf), opt(o){};

    void repl(std::istream &in, std::ostream &os);

    Expr eval(const Expr e, std::shared_ptr<SymbolTable> a);
    Expr perform_list(const Expr e, std::shared_ptr<SymbolTable> a);
    Expr eval_list(const Expr e, std::shared_ptr<SymbolTable> a);

    bool has_function(const Atom &f);

    std::shared_ptr<SymbolTable> globalTable;
    std::set<Atom>              &trace_functions;
    Options                      opt;

  private:
    std::shared_ptr<SymbolTable> create_context(Function *f, const Expr args,
                                                std::shared_ptr<SymbolTable> a);
    Expr perform_function(Function *f, const Expr args, std::shared_ptr<SymbolTable> a);
    Expr backquote(Expr s, std::shared_ptr<SymbolTable> a);
};

} // namespace ax