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

// Numbers

Expr numberp(List& args);

extern PrimBasicFunct zerop;
extern PrimBasicFunct plusp;
extern PrimBasicFunct minusp;

template <Int N>
Expr nump(List& args)
// Generates a templated function which mods compared to N.
{
    return abs(any_cast<Int>(args[0]) % 2) == N;
}

template <typename T>
PrimBasicFunct predicate(const function<bool(T, T)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](List& args) -> Expr {
        return f(any_cast<T>(args[0]), any_cast<T>(args[1]));
    };
}

extern PrimBasicFunct num_eq;
extern PrimBasicFunct num_neq;
extern PrimBasicFunct num_gt;
extern PrimBasicFunct num_ge;
extern PrimBasicFunct num_lt;
extern PrimBasicFunct num_le;

extern PrimBasicFunct num_add;
extern PrimBasicFunct num_sub;
extern PrimBasicFunct num_mult;
extern PrimBasicFunct num_div;
extern PrimBasicFunct num_mod;
extern PrimBasicFunct num_rem;

Expr num_sub_init(List& args);
PrimBasicFunct check_zeros(PrimBasicFunct f);

extern PrimBasicFunct num_power;

extern PrimBasicFunct num_max;
extern PrimBasicFunct num_min;

extern PrimBasicFunct num_abs;
extern PrimBasicFunct num_floor;
extern PrimBasicFunct num_ceil;
extern PrimBasicFunct num_round;
extern PrimBasicFunct num_trunc;

extern PrimBasicFunct num_log;
extern PrimBasicFunct num_exp;
extern PrimBasicFunct num_sin;
extern PrimBasicFunct num_cos;
extern PrimBasicFunct num_tan;
extern PrimBasicFunct num_asin;
extern PrimBasicFunct num_acos;
extern PrimBasicFunct num_atan;
extern PrimBasicFunct num_sqrt;

Expr incf(Evaluator& l, const string& name, List& args, SymbolTable& a);

// Functions

Expr defun(const string& name, List& args, SymbolTable& a);
Expr lambda(const string& name, List& args);
Expr funct(const string& name, List& args);
Expr functionp(const string&, List& args, SymbolTable& a);
Expr fboundp(const string&, List& args, SymbolTable& a);
Expr apply(Evaluator& l, const string& name, List& args, SymbolTable& a);
Expr funcall(Evaluator& l, const string& name, List& args, SymbolTable& a);
Expr mapcar(Evaluator& l, const string& name, List& args, SymbolTable& a);
Expr doFuncs(Evaluator& l, const string& name, List& args, SymbolTable& a);

// Strings

PrimBasicFunct funct_ci(PrimBasicFunct f, function<Expr(const Expr&)> trans);
Expr string_fnct(const string& name, List& args);

// I/O

Expr throw_error(List& args);
Expr quit(List& args);
}

#endif