//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <cmath>
#include <vector>

#include <boost/log/trivial.hpp>

#include "evaluator.hh"
#include "exceptions.hh"

namespace ax {

map<string, Primitive> prim_table;

Expr atom(const string&, List& args)
{
    return is_atomic(args[0]) || is_false(args[0]);
}

Expr symbolp(const string&, List& args)
{
    auto a = args.front();
    if (is_atomic(a)
        && (is_a<Atom>(a) || is_a<Bool>(a))) {
        return sT;
    }
    if (is_a<List>(a) && any_cast<List>(a).size() == 0) {
        return sT;
    }
    return sF;
}

Expr null(const string&, List& args)
{
    return is_false(args.front());
}

Expr andor(const string& name, List& args)
{
    if (args.size() == 0) {
        return name == "and";
    }
    Expr last = sF;
    for (auto& x : args) {
        last = Evaluator::eval(x);
        if (name == "and") {
            // BOOST_LOG_TRIVIAL(trace) << "and: " << to_string(last) << " : " << is_false(last);
            if (is_false(last)) {
                return sF;
            }
        } else {
            // or
            if (!is_false(last)) {
                return last;
            }
        }
    }
    return name == "and" ? last : sF;
}

Expr carcdr(const string& name, List& args)
{
    auto res = args.front();
    if (is_a<List>(res)) {
        auto list = any_cast<List>(res);
        if (list.size() > 0) {
            if (name == "car" || name == "first") {
                return list.front();
            } else {
                // cdr
                if (list.size() > 1) {
                    return List(list.begin() + 1, list.end());
                } else {
                    return sF;
                }
            }
        }
    }
    return sF;
}

Expr consp(const string& name, List& args)
{
    auto res = args.front();
    return is_a<List>(res) && any_cast<List>(res).size() > 0;
}

Expr listp(const string& name, List& args)
{
    auto res = args.front();
    return is_a<List>(res) || is_sF(res);
}

Expr cons(const string& name, List& args)
{
    if (is_a<List>(args[1])) {
        auto res = List(1, args[0]);
        for (auto& x : any_cast<List>(args[1])) {
            res.push_back(x);
        }
        return res;
    } else if (is_sF(args[1])) {
        return List(1, args[0]);
    }
    return sF;
}

Expr list(const string& name, List& args)
{
    if (args.size() == 0) {
        return sF;
    }
    return args;
}

Expr rplaca(const string& name, List& args)
{
    if (is_a<List>(args[0])) {
        auto l = any_cast<List>(args[0]);
        l[0] = args[1];
        return l;
    }
    throw EvalException("rplaca first argument not a list");
}

Expr rplacd(const string& name, List& args)
{
    if (is_a<List>(args[0])) {
        if (is_a<List>(args[1])) {
            auto l = any_cast<List>(args[0]);
            l.resize(1);
            auto cdr = any_cast<List>(args[1]);
            for (auto& x : cdr) {
                l.push_back(x);
            }
            return l;
        }
    }
    throw EvalException("rplaca first argument not a list");
}

Expr numberp(const string& name, List& args)
{
    return is_a<Int>(args[0]);
}

PrimFunct numeric_predicate0(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](const string& name, List& args) {
        if (is_a<Int>(args[0])) {
            return f(any_cast<Int>(args[0]), 0);
        }
        throw EvalException(name + " argument needs to be number");
    };
}

static function<bool(Int, Int)> eq = equal_to<Int>();
static function<bool(Int, Int)> neq = not_equal_to<Int>();
static function<bool(Int, Int)> gt = greater<Int>();
static function<bool(Int, Int)> ge = greater_equal<Int>();
static function<bool(Int, Int)> lt = less<Int>();
static function<bool(Int, Int)> le = less_equal<Int>();

static PrimFunct zerop = numeric_predicate0(eq);
static PrimFunct plusp = numeric_predicate0(gt);
static PrimFunct minusp = numeric_predicate0(lt);

template <Int N>
Expr nump(const string& name, List& args)
// Generates a templated function which mods compared to N.
{
    if (is_a<Int>(args[0])) {
        return abs(any_cast<Int>(args[0]) % 2) == N;
    }
    throw EvalException(name + " argument needs to be number");
}

PrimFunct numeric_predicate(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](const string& name, List& args) {
        if (is_a<Int>(args[0]) && is_a<Int>(args[1])) {
            return f(any_cast<Int>(args[0]), any_cast<Int>(args[1]));
        }
        throw EvalException(name + " arguments needs to be number");
    };
}

static PrimFunct num_eq = numeric_predicate(eq);
static PrimFunct num_neq = numeric_predicate(neq);
static PrimFunct num_gt = numeric_predicate(gt);
static PrimFunct num_ge = numeric_predicate(ge);
static PrimFunct num_lt = numeric_predicate(lt);
static PrimFunct num_le = numeric_predicate(le);

static function<Int(Int, Int)> add = plus<Int>();
static function<Int(Int, Int)> sub = minus<Int>();
static function<Int(Int, Int)> mult = multiplies<Int>();
static function<Int(Int, Int)> div = divides<Int>();
static function<Int(Int, Int)> mod = modulus<Int>();

PrimFunct numeric_operation(const function<Int(Int, Int)>& f, Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](const string& name, List& args) {
        if (args.size() == 0) {
            return s;
        }
        auto iter = args.begin();
        if (!is_a<Int>(*iter)) {
            throw EvalException(name + " arguments needs to be number");
        }
        auto acc = any_cast<Int>(*iter);
        for (iter++; iter != args.end(); iter++)
            if (is_a<Int>(*iter)) {
                acc = f(acc, any_cast<Int>(*iter));
            } else {
                throw EvalException(name + " arguments needs to be number");
            }
        return acc;
    };
}

static PrimFunct num_add = numeric_operation(add, 0);
static PrimFunct num_sub = numeric_operation(sub, 0);
static PrimFunct num_mult = numeric_operation(mult, 1);
static PrimFunct num_div = numeric_operation(div, 1);
static PrimFunct num_mod = numeric_operation(mod, 0);

Expr num_sub_init(const string& name, List& args)
{
    if (args.size() == 1 && is_a<Int>(args[0])) {
        return -any_cast<Int>(args[0]);
    }
    return num_sub(name, args);
}

PrimFunct check_zeros(PrimFunct f)
{
    return [=](const string& name, List& args) {
        auto iter = args.begin();
        for (iter++; iter != args.end(); ++iter)
            if (is_a<Int>(*iter) && any_cast<Int>(*iter) == 0) {
                throw NumericException("divide by zero");
            }
        return f(name, args);
    };
}

static PrimFunct num_power = numeric_operation(
    [](Int a, Int b) { return Int(pow(a, b)); },
    0);

static PrimFunct num_max = numeric_operation(
    [](Int a, Int b) { return Int(max<Int>(a, b)); },
    0);
static PrimFunct num_min = numeric_operation(
    [](Int a, Int b) { return Int(min<Int>(a, b)); },
    0);

PrimFunct numeric_single(const function<Int(Int)>& f)
// Returns a function implementing the function f on one argument.
{
    return [=](const string& name, List& args) {
        if (is_a<Int>(args[0])) {
            return f(any_cast<Int>(args[0]));
        }
        throw EvalException(name + " argument needs to be number");
    };
}

static PrimFunct num_abs = numeric_single([](Int x) { return Int(abs(x)); });
static PrimFunct num_floor = numeric_single([](Int x) { return Int(floor(x)); });
static PrimFunct num_ceil = numeric_single([](Int x) { return Int(ceil(x)); });
static PrimFunct num_round = numeric_single([](Int x) { return Int(round(x)); });
static PrimFunct num_trunc = numeric_single([](Int x) { return Int(trunc(x)); });

void init_prims()
{
    vector<Primitive> defs{
        { "atom", &atom, one_arg, preEvaluate },
        { "symbolp", &symbolp, one_arg, preEvaluate },

        { "null", &null, one_arg, preEvaluate },
        { "not", &null, one_arg, preEvaluate },
        { "and", &andor, no_check },
        { "or", &andor, no_check },

        { "car", &carcdr, one_arg, preEvaluate },
        { "first", &carcdr, one_arg, preEvaluate },
        { "cdr", &carcdr, one_arg, preEvaluate },
        { "rest", &carcdr, one_arg, preEvaluate },

        { "consp", &consp, one_arg, preEvaluate },
        { "listp", &listp, one_arg, preEvaluate },

        { "cons", &cons, two_args, preEvaluate },
        { "list", &list, no_check, preEvaluate },

        { "rplaca", &rplaca, two_args, preEvaluate },
        { "rplacd", &rplacd, two_args, preEvaluate },

        // Number functions

        { "numberp", &numberp, one_arg, preEvaluate },
        { "integerp", &numberp, one_arg, preEvaluate },

        { "zerop", zerop, one_arg, preEvaluate },
        { "oddp", &nump<1>, one_arg, preEvaluate },
        { "evenp", &nump<0>, one_arg, preEvaluate },
        { "plusp", plusp, one_arg, preEvaluate },
        { "minusp", minusp, one_arg, preEvaluate },

        { "=", num_eq, two_args, preEvaluate },
        { "/=", num_neq, two_args, preEvaluate },
        { "<", num_lt, two_args, preEvaluate },
        { "<=", num_le, two_args, preEvaluate },
        { ">", num_gt, two_args, preEvaluate },
        { ">=", num_ge, two_args, preEvaluate },

        { "+", num_add, no_check, preEvaluate },
        { "-", &num_sub_init, no_check, preEvaluate },
        { "*", num_mult, no_check, preEvaluate },
        { "/", check_zeros(num_div), no_check, preEvaluate },
        { "mod", check_zeros(num_mod), two_args, preEvaluate },
        { "^", num_power, min_one, preEvaluate },
        { "expt", num_power, min_one, preEvaluate },
        { "max", num_max, min_one, preEvaluate },
        { "min", num_min, min_one, preEvaluate },

        { "abs", num_abs, one_arg, preEvaluate },
        { "floor", num_floor, one_arg, preEvaluate },
        { "ceiling", num_ceil, one_arg, preEvaluate },
        { "round", num_round, one_arg, preEvaluate },
        { "truncate", num_trunc, one_arg, preEvaluate },

    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }
}
}
