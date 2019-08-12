//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "evaluator.hh"

#include <boost/log/trivial.hpp>

#include "exceptions.hh"
#include "function.hh"
#include "parser.hh"
#include "primitive.hh"

// 👾

namespace ax {

Expr Evaluator::perform_function(Function& f, List args, SymbolTable& a)
{
    List evalArgs;
    if (f.macro) {
        // Macro args are not evaluated
        evalArgs = args;
    } else {
        evalArgs = eval_list(args, a);
    }

    if (f.parameters.size() != args.size()) {
        throw EvalException(f.name + ": invalid number of arguments"s);
    }
    SymbolTable context(&a);
    for (int i = 0; i < evalArgs.size(); ++i) {
        context.put(any_cast<Atom>(f.parameters[i]), evalArgs[i]);
    }
    auto result = perform_list(f.body, context);
    if (f.macro) {
        // macros are post-evaluated
        result = eval(result, a);
    }
    return result;
}

Expr Evaluator::backquote(Expr& s, SymbolTable& a)
{
    if (is_atomic(s)) {
        return s;
    }
    if (is_a<List>(s)) {
        List result;
        List slist = any_cast<List>(s);
        for (auto iter = slist.begin(); iter != slist.end(); ++iter) {
            auto e = *iter;
            if (is_a<Atom>(e)
                && (any_cast<Atom>(e) == unquote_atom || any_cast<Atom>(e) == splice_unquote_atom)) {
                if (iter + 1 != slist.end()) {
                    auto res = eval(*(++iter), a);
                    if (any_cast<Atom>(e) == unquote_atom) {
                        result.push_back(res);
                    } else {
                        if (is_a<List>(res)) {
                            for (auto x : any_cast<List>(res)) {
                                result.push_back(x);
                            }
                        } else {
                            result.push_back(res);
                        }
                    }
                }
            } else if (is_a<List>(e)) {
                result.push_back(backquote(e, a));
            } else {
                result.push_back(e);
            }
        }
        return result;
    }
    return s;
}

List Evaluator::eval_list(List& l, SymbolTable& a)
{
    List result;
    for_each(l.begin(), l.end(), [&](Expr& e) { result.push_back(eval(e, a)); });
    return result;
}

Expr Evaluator::perform_list(List& l, SymbolTable& a)
{
    Expr result = sF;
    for_each(l.begin(), l.end(), [&](Expr& e) { result = eval(e, a); });
    return result;
}

Expr Evaluator::eval(Expr& e, SymbolTable& a)
{
    BOOST_LOG_TRIVIAL(debug) << "eval: " << to_string(e);

    // Eval basic types
    if (is_a<Bool>(e)) {
        return e;
    }
    if (is_a<Int>(e)) {
        return e;
    }
    if (is_a<Atom>(e)) {
        if (auto val = a.find(any_cast<Atom>(e))) {
            return *val;
        }
        throw EvalException("unbound variable: "s + to_string(e));
    }
    if (is_a<FunctionRef>(e)) {
        return e;
    }

    // Test for list
    if (!is_a<List>(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    // Expression is a list
    List e_list = any_cast<List>(e);
    if (e_list.empty()) {
        return sF;
    }

    auto e_car = e_list.front();
    if (is_a<Atom>(e_car)) {
        // Atom in function position
        Atom name = any_cast<Atom>(e_car);

        // quote
        if (name == quote_atom) {
            if (e_list.size() > 1) {
                return e_list[1];
            }
            return sF;
        }

        // backquote
        if (name == backquote_atom) {
            return backquote(e_list[1], a);
        }

        // Lookup symbol table for function
        if (auto f = a.find(name)) {
            if (!is_a<Function>(*f)) {
                throw EvalException("A non function in function location "s + to_string(*f));
            }
            Function fn = any_cast<Function>(*f);
            return perform_function(fn, List(e_list.begin() + 1, e_list.end()), a);
        }

        // Lookup primitive table
        if (auto prim = prim_table.find(name); prim != prim_table.end()) {
            auto result = List(e_list.begin() + 1, e_list.end());
            if (prim->second.preEval) {
                result = eval_list(result, a);
            }
            auto check = checkArgs(prim->second.cons, name, result);
            if (check) {
                throw EvalException(*check);
            }
            return prim->second.pf(name, result, a);
        }
    } else if (is_a<List>(e_car)) {
        // List in function position - eval and check for function object
        Expr flist = any_cast<List>(e_car);
        auto res = eval(flist, a);
        if (is_a<Function>(res)) {
            // perform function
            Function fn = any_cast<Function>(res);
            return perform_function(fn, List(e_list.begin() + 1, e_list.end()), a);
        }
    }

    throw EvalException("Can't evaluate "s + to_string(e));
}
}
