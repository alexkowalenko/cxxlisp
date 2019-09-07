//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef LISP_HH
#define LISP_HH

#include <iostream>

#include "options.hh"
#include "symboltable.hh"

namespace ax {

using namespace std;

class Lisp {
public:
    Lisp(Options& opt);

    void init();
    void repl(istream& in, ostream& os);
    void terminate();

private:
    Options& opt;
    shared_ptr<SymbolTable> symboltable;
};
}

#endif