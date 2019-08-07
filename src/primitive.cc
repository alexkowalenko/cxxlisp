//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <vector>

#include <boost/log/trivial.hpp>

#include "evaluator.hh"
#include "exceptions.hh"

namespace ax {

map<string, Primitive> prim_table;

Expr atom(const string&, List& args)
{
    if (is_atomic(args[0]) || is_false(args[0])) {
        return sT;
    }
    return sF;
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
        if (name == "and") {
            return sT;
        }
        return sF;
    }
    Expr last = sF;
    for (auto& x : args) {
        last = Evaluator::eval(x);
        if (name == "and") {
            BOOST_LOG_TRIVIAL(trace) << "and: " << to_string(last) << " : " << is_false(last);
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
    if (name == "and") {
        return last;
    }
    return sF;
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
    if (is_a<List>(res) && any_cast<List>(res).size() > 0) {
        return sT;
    }
    return sF;
}

Expr listp(const string& name, List& args)
{
    auto res = args.front();
    if (is_a<List>(res) || is_sF(res)) {
        return sT;
    }
    return sF;
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
    if (is_a<Int>(args[0])) {
        return sT;
    }
    return sF;
}

PrimFunct numeric_predicate0(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](const string& name, List& args) {
        if (is_a<Int>(args[0])) {
            if (f(any_cast<Int>(args[0]), 0)) {
                return sT;
            }
            return sF;
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
        if (abs(any_cast<Int>(args[0]) % 2) == N) {
            return sT;
        }
        return sF;
    }
    throw EvalException(name + " argument needs to be number");
}

PrimFunct numeric_predicate(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](const string& name, List& args) {
        if (is_a<Int>(args[0]) && is_a<Int>(args[1])) {
            if (f(any_cast<Int>(args[0]), any_cast<Int>(args[1]))) {
                return sT;
            }
            return sF;
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

    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }
}
}
