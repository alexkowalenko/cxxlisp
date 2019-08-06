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
    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }
}
}
