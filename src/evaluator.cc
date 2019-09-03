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

const string optional_atom = string("&optional");
const string rest_atom = string("&rest");

shared_ptr<SymbolTable> Evaluator::create_context(Function* f, Expr* args, shared_ptr<SymbolTable> a)
{
    Expr* evalArgs;
    if (f->macro) {
        // Macro args are not evaluated
        evalArgs = args;
    } else {
        evalArgs = eval_list(args, a);
    }

    auto evalArgs_size = size_list(evalArgs);
    unsigned int arg_count = 0;
    bool optional = false;
    bool rest = false;
    shared_ptr<SymbolTable> context = make_shared<SymbolTable>(a.get());

    for (auto param = f->parameters; !is_false(param) && !is_false(evalArgs); param = param->cdr) {
        if (is_a<Type::keyword>(param->car) && param->car->keyword == optional_atom) {
            optional = true;

            continue;
        } else if (is_a<Type::keyword>(param) && param->car->keyword == rest_atom) {
            rest = true;
            continue;
        } else if (is_a<Type::list>(param->car)) {
            if (optional) {
                // get symbol
                if (size_list(param->car) != 2) {
                    throw EvalException(f->name + ": default argument not 2 member list " + to_string(param->car));
                }
                auto pp = param->car->car;
                if (!is_a<Type::atom>(pp)) {
                    throw EvalException(f->name + ": optional default argument is not an atom " + to_string(pp));
                }
                if (evalArgs_size < arg_count) {
                    context->put(pp->atom, param->cdr->car);
                } else {
                    context->put(pp->atom, evalArgs->car);
                }
            }
        } else if (is_a<Type::atom>(param->car)) {
            if (evalArgs_size > arg_count) {
                if (rest) {
                    // Get all arguments and finish matching
                    context->put(param->car->atom, evalArgs);
                    break;
                } else {
                    context->put(param->car->atom, evalArgs->car);
                }
            } else if (optional) {
                context->put(param->car->atom, sF);
            } else if (rest) {
                context->put(param->car->atom, sF);
            }
        }
        ++arg_count;
        evalArgs = evalArgs->cdr;
    }
    auto f_param_size = is_false(f->parameters) ? 0 : size_list(f->parameters);
    cout << to_dstring(f->parameters) << endl;
    cout << "param count " << f_param_size << " : " << evalArgs_size << endl;
    if (rest) {
        ; // don't worry about counting pararamters
    } else if (optional) {
        if (!(f_param_size - 1 == evalArgs_size || f_param_size - 2 == evalArgs_size)) {
            throw EvalException(f->name + ": invalid number of arguments"s);
        }
    } else {
        if (f_param_size != evalArgs_size) {
            throw EvalException(f->name + ": invalid number of arguments"s);
        }
    }
    return context;
}

Expr* Evaluator::perform_function(Function* f, Expr* args, shared_ptr<SymbolTable> a)
{
    auto context = create_context(f, args, a);
    auto result = perform_list(f->body, context);
    if (f->macro) {
        // macros are post-evaluated
        result = eval(result, a);
    }
    return result;
}

/*
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
*/

Expr* Evaluator::eval_list(const Expr* e, shared_ptr<SymbolTable> a)
{
    if (is_false(e)) {
        return sF;
    }
    Expr* result = mk_list();
    Expr* rl = result;
    while (e) {
        if (!rl->car) {
            rl->car = eval(e->car, a);
        } else {
            rl->cdr = mk_list(eval(e->car, a));
            rl = rl->cdr;
        }
        e = e->cdr;
    }
    // cout << "eval_list: " << to_dstring(result) << endl;
    return result;
}

Expr* Evaluator::perform_list(Expr* e, shared_ptr<SymbolTable> a)
{
    auto result = sF;
    while (e) {
        result = eval(e->car, a);
        e = e->cdr;
    }
    return result;
}

Expr* Evaluator::eval(Expr* const e, shared_ptr<SymbolTable> a)
{
    //if (opt.debug_expr) {
    BOOST_LOG_TRIVIAL(debug) << "eval: " << to_string(e);
    //};

    if (is_false(e)) { // stops nullptrs
        return sF;
    }
    // Eval basic types
    switch (e->type) {
    case Type::boolean:
        return e;
    case Type::integer:
        return e;
    case Type::atom:
        if (auto val = a->find(e->atom)) {
            return *val;
        }
        throw EvalException("unbound variable: "s + to_string(e));

    // if (is_a<String>(e) || is_a<Char>(e)) {
    //     return e;
    // }
    // if (is_a<FunctionRef>(e)) {
    //     return e;
    // }
    case Type::function:
        return e;
    case Type::keyword:
        return e;
    case Type::list:; // fallthrough
    }

    // Test for list
    if (!is_list(e)) {
        throw EvalException("Can't evaluate "s + to_string(e));
    }

    auto e_car = e->car;
    if (is_atom(e_car)) {
        // Atom in function position
        Atom name = e_car->atom;

        // quote
        if (name == quote_at->atom) {
            if (size_list(e) == 2) {
                return e->cdr->car;
            }
            throw EvalException("quote: requires one argument");
        }

        // backquote
        // if (name == backquote_atom) {
        //     return backquote(e_list[1], a);
        // }

        // Lookup symbol table for function
        if (auto f = a->find(name)) {
            if (!is_a<Type::function>(*f)) {
                throw EvalException("A non function in function location "s + to_string(*f));
            }
            auto fn = (*f)->function;
            return perform_function(fn, e->cdr, a);
        }

        // Lookup primitive table
        if (auto prim = prim_table.find(name); prim != prim_table.end()) {
            auto result = e->cdr;
            if (prim->second.preEval) {
                result = eval_list(result, a);
            }
            auto check = checkArgs(prim->second.cons, name, result);
            if (check) {
                throw EvalException(*check);
            }
            return visit(overloaded{
                             [&](PrimBasicFunct pf) -> Expr* { return pf(result); },
                             [&](PrimSimpleFunct pf) -> Expr* { return pf(name, result); },
                             [&](PrimFunct pf) -> Expr* { return pf(name, result, a); },
                             [&](PrimFullFunct pf) -> Expr* { return pf(*this, name, result, a); },
                         },
                prim->second.pf);
        }
    } else if (is_a<Type::function>(e_car)) {
        // compiled function in function position - output of apply.
        auto fn = e_car->function;
        return perform_function(fn, e->cdr, a);
    } else if (is_a<Type::list>(e_car)) {
        // List in function position - eval and check for function object
        auto res = eval(e_car, a);
        if (is_a<Type::function>(res)) {
            // perform function
            auto fn = res->function;
            return perform_function(fn, e->cdr, a);
        }
    }

    throw EvalException("Can't evaluate "s + to_string(e));
}
}
