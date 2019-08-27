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

/*
Expr numberp(List& args)
{
    return is_a<Int>(args[0]) || is_a<Float>(args[0]);
}

PrimBasicFunct num_predicate0(const function<bool(Float, Float)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](List& args) {
        return f(as_Float(args[0]), 0);
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

PrimBasicFunct predicate_num(function<bool(Int, Int)>& f, function<bool(Float, Float)>& fr)
{
    return [&](List& args) -> Expr {
        if (is_a<Int>(args[0]) && is_a<Int>(args[1])) {
            return f(any_cast<Int>(args[0]), any_cast<Int>(args[1]));
        }
        return fr(as_Float(args[0]), as_Float(args[1]));
    };
}

PrimBasicFunct num_eq = predicate_num(eq, eqf);
PrimBasicFunct num_neq = predicate_num(neq, neqf);
PrimBasicFunct num_gt = predicate_num(gt, gtf);
PrimBasicFunct num_ge = predicate_num(ge, gef);
PrimBasicFunct num_lt = predicate_num(lt, ltf);
PrimBasicFunct num_le = predicate_num(le, lef);

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

PrimBasicFunct numeric_operation(const function<Int(Int, Int)>& f, Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](List& args) -> Expr {
        if (args.empty()) {
            return s;
        }
        return accumulate(args.begin() + 1,
            args.end(),
            any_cast<Int>(*args.begin()),
            [=](Expr a, Expr b) -> Int {
                return f(any_cast<Int>(a), any_cast<Int>(b));
            });
    };
}

bool is_all_Int(const List& a)
{
    for (auto x : a) {
        if (!is_a<Int>(x)) {
            return false;
        }
    }
    return true;
}

PrimBasicFunct numeric_operation_float(const function<Int(Int, Int)>& f, const function<Float(Float, Float)>& fr, Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](List& args) -> Expr {
        if (args.empty()) {
            return s;
        }
        if (is_all_Int(args)) {
            return accumulate(args.begin() + 1,
                args.end(),
                any_cast<Int>(*args.begin()),
                [=](Expr a, Expr b) -> Int {
                    return f(any_cast<Int>(a), any_cast<Int>(b));
                });
        }
        return accumulate(args.begin() + 1,
            args.end(),
            as_Float(*args.begin()),
            [=](Expr a, Expr b) -> Float {
                return fr(as_Float(a), as_Float(b));
            });
    };
}

PrimBasicFunct num_add = numeric_operation_float(add, addf, 0);
PrimBasicFunct num_sub = numeric_operation_float(sub, subf, 0);
PrimBasicFunct num_mult = numeric_operation_float(mult, multf, 1);
PrimBasicFunct num_div = numeric_operation_float(div, divf, 1);
PrimBasicFunct num_mod = numeric_operation_float(mod, modf, 1);
PrimBasicFunct num_rem = numeric_operation_float(rem, remf, 1);

Expr num_sub_init(List& args)
{
    if (args.size() == 1) {
        if (is_a<Int>(args[0])) {
            return -any_cast<Int>(args[0]);
        }
        return -any_cast<Float>(args[0]);
    }
    return num_sub(args);
}

PrimBasicFunct check_zeros(PrimBasicFunct f)
{
    return [=](List& args) -> Expr {
        for_each(args.begin() + 1,
            args.end(),
            [](Expr e) { if (as_Float(e) == 0) {
                throw NumericException("divide by zero");
            } });
        if (args.size() == 1) {
            args.insert(args.begin(), Float{ 1 });
        }
        return f(args);
    };
}

PrimBasicFunct num_power = numeric_operation_float(
    [](Int a, Int b) -> Float { return Float(pow(as_Float(a), as_Float(b))); },
    [](Float a, Float b) -> Float { return Float(pow(a, b)); },
    0);

PrimBasicFunct num_max = numeric_operation_float(
    [](Int a, Int b) -> Int { return Int(max<Int>(a, b)); },
    [](Float a, Float b) -> Float { return Float(max<Float>(a, b)); },
    0);

PrimBasicFunct num_min = numeric_operation_float(
    [](Int a, Int b) -> Int { return Int(min<Int>(a, b)); },
    [](Float a, Float b) -> Float { return Float(min<Float>(a, b)); },
    0);

PrimBasicFunct numeric_single(const function<Float(Float)>& f)
// Returns a function implementing the function f on one argument.
{
    return [=](List& args) -> Expr {
        return f(as_Float(args[0]));
    };
}

PrimBasicFunct num_abs = numeric_single([](Float x) -> Float { return Float(abs(x)); });
PrimBasicFunct num_floor = numeric_single([](Float x) -> Float { return Float(floor(x)); });
PrimBasicFunct num_ceil = numeric_single([](Float x) -> Float { return Float(ceil(x)); });
PrimBasicFunct num_round = numeric_single([](Float x) -> Float { return Float(round(x)); });
PrimBasicFunct num_trunc = numeric_single([](Float x) -> Float { return Float(trunc(x)); });

PrimBasicFunct num_log = numeric_single([](Float x) -> Float { return Float(log(x)); });
PrimBasicFunct num_exp = numeric_single([](Float x) -> Float { return Float(exp(x)); });
PrimBasicFunct num_sin = numeric_single([](Float x) -> Float { return Float(sin(x)); });
PrimBasicFunct num_cos = numeric_single([](Float x) -> Float { return Float(cos(x)); });
PrimBasicFunct num_tan = numeric_single([](Float x) -> Float { return Float(tan(x)); });
PrimBasicFunct num_asin = numeric_single([](Float x) -> Float { return Float(asin(x)); });
PrimBasicFunct num_acos = numeric_single([](Float x) -> Float { return Float(acos(x)); });
PrimBasicFunct num_atan = numeric_single([](Float x) -> Float { return Float(atan(x)); });
PrimBasicFunct num_sqrt = numeric_single([](Float x) -> Float { return Float(sqrt(x)); });

Expr incf(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    if (!is_a<Atom>(args[0])) {
        throw EvalException(name + ": argument needs to be symbol");
    }
    Expr incr = Int{ 1 };
    if (args.size() > 1) {
        incr = l.eval(args[1], a);
        if (!is_Num(incr)) {
            throw EvalException(name + ": increment is not number");
        }
    }
    if (auto val = a.find(any_cast<Atom>(args[0]))) {
        if (!is_Num(*val)) {
            throw EvalException(name + ": value is not a number " + to_string(*val));
        }
        Expr result = Float(as_Float(*val) + (as_Float(incr)) * (name == "incf" ? 1 : -1));
        a.set(any_cast<Atom>(args[0]), result);
        return result;
    } else {
        throw EvalException(name + ": undefined variable " + to_string(args[0]));
    }
}
*/
};
