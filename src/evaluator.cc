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

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

template <class... Ts>
overloaded(Ts...)->overloaded<Ts...>;

const Keyword optional_atom = Keyword("&optional");
const Keyword rest_atom = Keyword("&rest");

Evaluator::Evaluator(Options& o)
    : opt(o){};

SymbolTable Evaluator::create_context(Function& f, List args, SymbolTable& a)
{
    List evalArgs;
    if (f.macro) {
        // Macro args are not evaluated
        evalArgs = args;
    } else {
        evalArgs = eval_list(args, a);
    }

    unsigned int arg_count = 0;
    bool optional = false;
    bool rest = false;
    SymbolTable context(&a);
    for (unsigned int i = 0; i < f.parameters.size(); ++i) {
        auto param = f.parameters[i];
        if (is_a<Keyword>(param) && any_cast<Keyword>(param) == optional_atom) {
            optional = true;
            continue;
        } else if (is_a<Keyword>(param) && any_cast<Keyword>(param) == rest_atom) {
            rest = true;
            continue;
        } else if (is_a<List>(param)) {
            if (optional) {
                // get symbol
                if (any_cast<List>(param).size() != 2) {
                    throw EvalException(f.name + ": default argument not 2 member list " + to_string(param));
                }
                auto pp = any_cast<List>(param)[0];
                if (!is_a<Atom>(pp)) {
                    throw EvalException(f.name + ": optional default argument is not an atom " + to_string(pp));
                }
                cout << "Args " << evalArgs.size() << " : " << i << endl;
                if (evalArgs.size() < i) {
                    context.put(any_cast<Atom>(pp), any_cast<List>(param)[1]);
                } else {
                    context.put(any_cast<Atom>(pp), evalArgs[arg_count]);
                }
            }
        } else if (is_a<Atom>(param)) {
            if (evalArgs.size() > arg_count) {
                if (rest) {
                    // Get all arguments and finish matching
                    List restList(evalArgs.begin() + arg_count, evalArgs.end());
                    context.put(any_cast<Atom>(f.parameters[i]), restList);
                    break;
                } else {
                    context.put(any_cast<Atom>(f.parameters[i]), evalArgs[arg_count]);
                }
            } else if (optional) {
                context.put(any_cast<Atom>(f.parameters[i]), sF);
            } else if (rest) {
                context.put(any_cast<Atom>(f.parameters[i]), sF);
            }
        }
        ++arg_count;
    }
    if (rest) {
        ; // don't worry about counting pararamters
    } else if (optional) {
        if (!(f.parameters.size() - 1 == evalArgs.size() || f.parameters.size() - 2 == evalArgs.size())) {
            throw EvalException(f.name + ": invalid number of arguments"s);
        }
    } else {
        if (f.parameters.size() != evalArgs.size()) {
            throw EvalException(f.name + ": invalid number of arguments"s);
        }
    }
    return context;
}

Expr Evaluator::perform_function(Function& f, List args, SymbolTable& a)
{
    SymbolTable context = create_context(f, args, a);
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
    if (opt.debug_expr) {
        BOOST_LOG_TRIVIAL(debug) << "eval: " << to_string(e);
    };

    // Eval basic types
    if (is_a<Bool>(e)) {
        return e;
    }
    if (is_a<Int>(e) || is_a<Float>(e)) {
        return e;
    }
    if (is_a<Atom>(e)) {
        if (auto val = a.find(any_cast<Atom>(e))) {
            return *val;
        }
        throw EvalException("unbound variable: "s + to_string(e));
    }
    if (is_a<String>(e) || is_a<Char>(e)) {
        return e;
    }
    if (is_a<FunctionRef>(e)) {
        return e;
    }
    if (is_a<Keyword>(e)) {
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
            List result(e_list.begin() + 1, e_list.end());
            if (prim->second.preEval) {
                result = eval_list(result, a);
            }
            auto check = checkArgs(prim->second.cons, name, result);
            if (check) {
                throw EvalException(*check);
            }
            return visit(overloaded{
                             [&](PrimBasicFunct pf) -> Expr { return pf(result); },
                             [&](PrimSimpleFunct pf) -> Expr { return pf(name, result); },
                             [&](PrimFunct pf) -> Expr { return pf(name, result, a); },
                             [&](PrimFullFunct pf) -> Expr { return pf(*this, name, result, a); },
                         },
                prim->second.pf);
        }
    } else if (is_a<Function>(e_car)) {
        // compiled function in function position - out put of apply.
        Function fn = any_cast<Function>(e_car);
        return perform_function(fn, List(e_list.begin() + 1, e_list.end()), a);
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
