//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <cmath>
#include <numeric>

#include "exceptions.hh"

namespace ax {

PrimBasicFunct num_predicate0(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](List& args) {
        return f(any_cast<Int>(args[0]), 0);
    };
}

static function<bool(Int, Int)> eq = equal_to<Int>();
static function<bool(Int, Int)> neq = not_equal_to<Int>();
static function<bool(Int, Int)> gt = greater<Int>();
static function<bool(Int, Int)> ge = greater_equal<Int>();
static function<bool(Int, Int)> lt = less<Int>();
static function<bool(Int, Int)> le = less_equal<Int>();

PrimBasicFunct zerop = num_predicate0(eq);
PrimBasicFunct plusp = num_predicate0(gt);
PrimBasicFunct minusp = num_predicate0(lt);

PrimBasicFunct num_eq = predicate<Int>(eq);
PrimBasicFunct num_neq = predicate<Int>(neq);
PrimBasicFunct num_gt = predicate<Int>(gt);
PrimBasicFunct num_ge = predicate<Int>(ge);
PrimBasicFunct num_lt = predicate<Int>(lt);
PrimBasicFunct num_le = predicate<Int>(le);

static function<Int(Int, Int)> add = plus<Int>();
static function<Int(Int, Int)> sub = minus<Int>();
static function<Int(Int, Int)> mult = multiplies<Int>();
static function<Int(Int, Int)> div = divides<Int>();
static function<Int(Int, Int)> mod = modulus<Int>();

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

PrimBasicFunct num_add = numeric_operation(add, 0);
PrimBasicFunct num_sub = numeric_operation(sub, 0);
PrimBasicFunct num_mult = numeric_operation(mult, 1);
PrimBasicFunct num_div = numeric_operation(div, 1);
PrimBasicFunct num_mod = numeric_operation(mod, 0);

Expr num_sub_init(List& args)
{
    if (args.size() == 1) {
        return -any_cast<Int>(args[0]);
    }
    return num_sub(args);
}

PrimBasicFunct check_zeros(PrimBasicFunct f)
{
    return [=](List& args) -> Expr {
        for_each(args.begin() + 1,
            args.end(),
            [](Expr e) { if (any_cast<Int>(e) == 0) {
                throw NumericException("divide by zero");
            } });
        return f(args);
    };
}

PrimBasicFunct num_power = numeric_operation(
    [](Int a, Int b) -> Int { return Int(pow(a, b)); },
    0);

PrimBasicFunct num_max = numeric_operation(
    [](Int a, Int b) -> Int { return Int(max<Int>(a, b)); },
    0);
PrimBasicFunct num_min = numeric_operation(
    [](Int a, Int b) -> Int { return Int(min<Int>(a, b)); },
    0);

PrimBasicFunct numeric_single(const function<Int(Int)>& f)
// Returns a function implementing the function f on one argument.
{
    return [=](List& args) -> Expr {
        return f(any_cast<Int>(args[0]));
    };
}

PrimBasicFunct num_abs = numeric_single([](Int x) -> Int { return Int(abs(x)); });
PrimBasicFunct num_floor = numeric_single([](Int x) -> Int { return Int(floor(x)); });
PrimBasicFunct num_ceil = numeric_single([](Int x) -> Int { return Int(ceil(x)); });
PrimBasicFunct num_round = numeric_single([](Int x) -> Int { return Int(round(x)); });
PrimBasicFunct num_trunc = numeric_single([](Int x) -> Int { return Int(trunc(x)); });
};
