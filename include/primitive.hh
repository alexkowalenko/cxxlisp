//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef PRIMITIVE_HH
#define PRIMITIVE_HH

#include <map>
#include <variant>

#include "args.hh"
#include "evaluator.hh"
#include "expr.hh"
#include "symboltable.hh"

namespace ax {

using namespace std;

using PrimBasicFunct = function<Expr(List& args)>;
using PrimSimpleFunct = function<Expr(const string& name, List& args)>;
using PrimFunct = function<Expr(const string& name, List& args, SymbolTable& a)>;
using PrimFullFunct = function<Expr(Evaluator& l, const string& name, List& args, SymbolTable& a)>;

struct Primitive {
    string name;
    variant<PrimBasicFunct, PrimSimpleFunct, PrimFunct, PrimFullFunct> pf;
    ArgConstraint cons;
    bool preEval = false;
};

inline const bool preEvaluate = true;

extern map<string, Primitive> prim_table;

void init_prims();
}

#endif