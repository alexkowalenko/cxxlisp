//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <algorithm>
#include <cmath>
#include <numeric>

#include "exceptions.hh"

namespace ax {

Expr* numberp(Expr* args)
{
    return is_a<Type::integer>(args->car) || is_a<Type::floating>(args->car) ? sT : sF;
}

PrimBasicFunct num_predicate0(const function<bool(Float, Float)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](Expr* const args) {
        return f(as_float(args->car), 0.0) ? sT : sF;
    };
}

// these need to be static, as they are held by functions, when the functor here goes out of scope.
static function<bool(Int, Int)> eq = equal_to<Int>();
static function<bool(Int, Int)> neq = not_equal_to<Int>();
static function<bool(Int, Int)> gt = greater<Int>();
static function<bool(Int, Int)> ge = greater_equal<Int>();
static function<bool(Int, Int)> lt = less<Int>();
static function<bool(Int, Int)> le = less_equal<Int>();

static function<bool(Float, Float)> eqf = equal_to<Float>();
static function<bool(Float, Float)> neqf = not_equal_to<Float>();
static function<bool(Float, Float)> gtf = greater<Float>();
static function<bool(Float, Float)> gef = greater_equal<Float>();
static function<bool(Float, Float)> ltf = less<Float>();
static function<bool(Float, Float)> lef = less_equal<Float>();

PrimBasicFunct zerop = num_predicate0(eqf);
PrimBasicFunct plusp = num_predicate0(gtf);
PrimBasicFunct minusp = num_predicate0(ltf);

PrimBasicFunct predicate_num(function<bool(Float, Float)>& f)
{
    return [&](Expr* args) -> Expr* {
        return f(as_float(args->car), as_float(args->cdr->car)) ? sT : sF;
    };
}

PrimBasicFunct num_eq = predicate_num(eqf);
PrimBasicFunct num_neq = predicate_num(neqf);
PrimBasicFunct num_gt = predicate_num(gtf);
PrimBasicFunct num_ge = predicate_num(gef);
PrimBasicFunct num_lt = predicate_num(ltf);
PrimBasicFunct num_le = predicate_num(lef);

static function<Int(Int, Int)> add = plus<Int>();
static function<Int(Int, Int)> sub = minus<Int>();
static function<Int(Int, Int)> mult = multiplies<Int>();
static function<Int(Int, Int)> div = divides<Int>();
static function<Int(Int, Int)> mod = modulus<Int>();
static function<Int(Int, Int)> rem = modulus<Int>();

static function<Float(Float, Float)> addf = plus<Float>();
static function<Float(Float, Float)> subf = minus<Float>();
static function<Float(Float, Float)> multf = multiplies<Float>();
static function<Float(Float, Float)> divf = divides<Float>();
static function<Float(Float, Float)> modf = [](Float a, Float b) -> Float { return Float(fmod(a, b)); };
static function<Float(Float, Float)> remf = [](Float a, Float b) -> Float { return Float(remainder(a, b)); };

PrimBasicFunct numeric_operation_int(const function<Int(Int, Int)>& f, Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](Expr* args) -> Expr* {
        if (is_false(args)) {
            return mk_int(s);
        }
        auto accum = args->car->integer;
        args = args->cdr;
        while (args) {
            accum = f(accum, args->car->integer);
            args = args->cdr;
        }
        return mk_int(accum);
    };
}

bool is_all_integer(Expr* args)
{
    while (!is_false(args)) {
        if (!is_a<Type::integer>(args->car)) {
            return false;
        }
        args = args->cdr;
    }
    return true;
}

PrimBasicFunct numeric_operation_float(const function<Int(Int, Int)>& f,
    const function<Float(Float, Float)>& fr,
    Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](Expr* args) -> Expr* {
        if (is_false(args)) {
            return mk_int(s);
        }
        if (is_all_integer(args)) {
            auto accum = args->car->integer;
            args = args->cdr;
            while (args) {
                accum = f(accum, args->car->integer);
                args = args->cdr;
            }
            return mk_int(accum);
        }
        auto accum = as_float(args->car);
        args = args->cdr;
        while (args) {
            accum = fr(accum, as_float(args->car));
            args = args->cdr;
        }
        return mk_float(accum);
    };
}

PrimBasicFunct numeric_operation2_float(const function<Int(Int, Int)>& f,
    const function<Float(Float, Float)>& fr)
// Returns a function implementing the function f across 2 arguments.
{
    return [=](Expr* args) -> Expr* {
        if (is_all_integer(args)) {
            return mk_int(f(args->car->integer, args->cdr->car->integer));
        }
        return mk_float(f(as_float(args->car), as_float(args->cdr->car)));
    };
}

PrimBasicFunct num_add = numeric_operation_float(add, addf, 0);
PrimBasicFunct num_sub = numeric_operation_float(sub, subf, 0);
PrimBasicFunct num_mult = numeric_operation_float(mult, multf, 1);
PrimBasicFunct num_div = numeric_operation_float(div, divf, 1);
PrimBasicFunct num_mod = numeric_operation2_float(mod, modf);
PrimBasicFunct num_rem = numeric_operation2_float(rem, remf);

Expr* num_sub_init(Expr* args)
{
    if (args->size() == 1) {
        return mk_float(-as_float(args->car));
    }
    return num_sub(args);
}

PrimBasicFunct check_zeros(PrimBasicFunct f)
{
    return [=](Expr* args) -> Expr* {
        auto x = args;
        x = x->cdr; // start at second item
        while (x) {
            if (x->car->integer == 0) {
                throw NumericException("divide by zero");
            }
            x = x->cdr;
        }
        if (args->size() == 1) {
            args = mk_list(mk_int(1), args);
        }
        return f(args);
    };
}

PrimBasicFunct numeric_operation(const function<Float(Float, Float)>& f, Float s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](Expr* args) -> Expr* {
        if (is_false(args)) {
            return mk_int(s);
        }
        auto accum = as_float(args->car);
        args = args->cdr;
        while (args) {
            accum = f(accum, as_float(args->car));
            args = args->cdr;
        }
        return mk_float(accum);
    };
}

PrimBasicFunct num_power = numeric_operation(
    [](Float a, Float b) -> Float { return Float(pow(a, b)); },
    0);

PrimBasicFunct num_max = numeric_operation(
    [](Float a, Float b) -> Float { return max<Float>(a, b); },
    0);

PrimBasicFunct num_min = numeric_operation(
    [](Float a, Float b) -> Float { return min<Float>(a, b); },
    0);

PrimBasicFunct numeric_single(const function<Float(Float)>& f)
// Returns a function implementing the function f on one argument.
{
    return [=](Expr* args) -> Expr* {
        return mk_float(f(as_float(args->car)));
    };
}

PrimBasicFunct num_abs = numeric_single([](Float x) -> Float { return abs(x); });
PrimBasicFunct num_floor = numeric_single([](Float x) -> Float { return floor(x); });
PrimBasicFunct num_ceil = numeric_single([](Float x) -> Float { return ceil(x); });
PrimBasicFunct num_round = numeric_single([](Float x) -> Float { return round(x); });
PrimBasicFunct num_trunc = numeric_single([](Float x) -> Float { return trunc(x); });

PrimBasicFunct num_log = numeric_single([](Float x) -> Float { return Float(log(x)); });
PrimBasicFunct num_exp = numeric_single([](Float x) -> Float { return Float(exp(x)); });
PrimBasicFunct num_sin = numeric_single([](Float x) -> Float { return Float(sin(x)); });
PrimBasicFunct num_cos = numeric_single([](Float x) -> Float { return Float(cos(x)); });
PrimBasicFunct num_tan = numeric_single([](Float x) -> Float { return Float(tan(x)); });
PrimBasicFunct num_asin = numeric_single([](Float x) -> Float { return Float(asin(x)); });
PrimBasicFunct num_acos = numeric_single([](Float x) -> Float { return Float(acos(x)); });
PrimBasicFunct num_atan = numeric_single([](Float x) -> Float { return Float(atan(x)); });
PrimBasicFunct num_sqrt = numeric_single([](Float x) -> Float { return Float(sqrt(x)); });

Expr* incf(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (!is_a<Type::atom>(args->car)) {
        throw EvalException(name + ": argument needs to a reference");
    }
    auto incr = mk_int(1);
    if (args->size() > 1) {
        incr = l.eval(args->cdr->car, a);
        if (!is_number(incr)) {
            throw EvalException(name + ": increment is not number");
        }
    }
    if (auto val = a->find(args->car->atom)) {
        if (!is_number(*val)) {
            throw EvalException(name + ": value is not a number " + to_string(*val));
        }
        auto result = mk_float(as_float(*val) + (as_float(incr)) * (name == "incf" ? 1 : -1));
        a->set(args->car->atom, result);
        return result;
    } else {
        throw EvalException(name + ": undefined variable " + to_string(args->car));
    }
}
};
