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

void init_prims();

using PrimBasicFunct = function<Expr*(Expr* const args)>;
using PrimSimpleFunct = function<Expr*(const string& name, Expr* const args)>;
using PrimFunct = function<Expr*(const string& name, Expr* const args, shared_ptr<SymbolTable> a)>;
using PrimFullFunct = function<Expr*(Evaluator& l, const string& name, Expr* const args, shared_ptr<SymbolTable> a)>;

struct Primitive {
    string name;
    variant<PrimBasicFunct, PrimSimpleFunct, PrimFunct, PrimFullFunct> pf;
    ArgConstraint cons;
    bool preEval = false;
};

inline const bool preEvaluate = true;

extern map<string, Primitive> prim_table;

// Accessor functions for setf
using AccessorFunct = function<Expr*(Evaluator& l, Expr* args, Expr* val, shared_ptr<SymbolTable> a)>;

struct Accessor {
    string name;
    AccessorFunct af;
    ArgConstraint cons;
    bool preEval = false;
};
extern map<Atom, Accessor> setf_accessors;

// get a reference, in order to modify it.
Expr* get_reference(const string& name, Expr* ref, shared_ptr<SymbolTable> a);

// Numbers

Expr* numberp(Expr* args);

extern PrimBasicFunct zerop;
extern PrimBasicFunct plusp;
extern PrimBasicFunct minusp;

template <Int N>
Expr* nump(Expr* args)
// Generates a templated function which mods compared to N.
{
    return abs(args->car->integer % 2) == N ? sT : sF;
}

template <typename T>
PrimBasicFunct predicate_str(const function<bool(T, T)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](Expr* args) -> Expr* {
        return f(args->car->string, args->cdr->car->string) ? sT : sF;
    };
}

template <typename T>
PrimBasicFunct predicate_chr(const function<bool(T, T)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](Expr* args) -> Expr* {
        return f(args->car->chr, args->cdr->car->chr) ? sT : sF;
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

Expr* num_sub_init(Expr* args);
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

Expr* incf(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* float_f(Expr*);

template <Int d>
Expr* inc(const string& name, Expr* args)
{
    return mk_float(as_float(args->car) + d);
}

// Functions

Expr* defun(const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* lambda(const string& name, Expr* args);
Expr* funct(const string& name, Expr* args);
Expr* functionp(const string&, Expr* args, shared_ptr<SymbolTable> a);
Expr* fboundp(const string&, Expr* args, shared_ptr<SymbolTable> a);
Expr* apply(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* funcall(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* mapcar(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* doFuncs(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a);

// Strings

extern PrimBasicFunct str_eq;
extern PrimBasicFunct str_neq;
extern PrimBasicFunct str_gt;
extern PrimBasicFunct str_ge;
extern PrimBasicFunct str_lt;
extern PrimBasicFunct str_le;

PrimBasicFunct funct_ci(PrimBasicFunct f, function<Expr*(const Expr*)> trans);
Expr* string_fnct(const string& name, Expr* args);
Expr* to_lower_str(const Expr* s);

// Characters

extern PrimBasicFunct char_eq;
extern PrimBasicFunct char_neq;
extern PrimBasicFunct char_gt;
extern PrimBasicFunct char_ge;
extern PrimBasicFunct char_lt;
extern PrimBasicFunct char_le;

Expr* to_lower_char(const Expr* s);

// Sequences

Expr* length(Expr* args);
Expr* elt(Expr* args);
Expr* setelt(const string& name, Expr* args);
Expr* subseq(Expr* args);

Expr* setf_elt(Evaluator& l, Expr* args, Expr* r, shared_ptr<SymbolTable> a);
Expr* make_sequence(Expr* args);

// I/O

Expr* throw_error(Expr* args);
Expr* quit(Expr* args);

Expr* const std_out = mk_atom("*standard-output*");
Expr* const std_in = mk_atom("*standard-input*");
Expr* const std_err = mk_atom("*error-output*");

template <StreamType N>
Expr* stream_typep(Expr* args)
{
    return is_a<Type::stream>(args->car) && args->car->stream->stream_type == N
        ? sT
        : sF;
}

Expr* open(Expr* args);
Expr* close(Expr* args);

Expr* print(const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* terpri(const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* read(const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* read_char(const string& name, Expr* args, shared_ptr<SymbolTable> a);
Expr* format(const string& name, Expr* args, shared_ptr<SymbolTable> a);
}
#endif