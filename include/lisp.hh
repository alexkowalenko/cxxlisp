//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <iostream>
#include <set>

#include "options.hh"
#include "symboltable.hh"

namespace ax {

class Lisp {
  public:
    Lisp(Options &opt);

    void init();
    void repl(std::istream &in, std::ostream &os);
    void terminate();

  private:
    Options &                    opt;
    std::shared_ptr<SymbolTable> symboltable;
    std::set<Atom>               trace_functions;
};

} // namespace ax
