//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef PRIMITIVE_HH
#define PRIMITIVE_HH

#include <map>

#include "args.hh"
#include "expr.hh"

namespace ax {

using namespace std;
typedef Expr (*PrimFunct)(const string& name, List& args);

struct Primitive {
    string name;
    PrimFunct pf;
    ArgConstraint cons;
    bool preEval = false;
};

inline const bool preEvaluate = true;

extern map<string, Primitive> prim_table;

void init_prims();
}

#endif