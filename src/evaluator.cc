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

    // BOOST_LOG_TRIVIAL(debug) << "function args: " << to_string(evalArgs);

    auto evalArgs_size = evalArgs->size();
    unsigned int arg_count = 0;
    bool optional = false;
    bool rest = false;
    shared_ptr<SymbolTable> context = make_shared<SymbolTable>(a.get());
    auto f_param_size = is_false(f->parameters) ? 0 : f->parameters->size();

    for (auto param = f->parameters; !is_false(param); param = param->cdr) {
        if (is_a<Type::keyword>(param->car) && param->car->keyword == optional_atom) {
            optional = true;
            // cout << "opt args " << endl;
            continue;
        } else if (is_a<Type::keyword>(param->car) && param->car->keyword == rest_atom) {
            rest = true;
            // cout << "rest args " << endl;
            continue;
        } else if (is_a<Type::list>(param->car)) {
            if (optional) {
                // get symbol
                if (param->car->size() != 2) {
                    throw EvalException(f->name + ": default argument not 2 member list " + to_string(param->car));
                }
                auto atom = param->car->car;
                if (!is_a<Type::atom>(atom)) {
                    throw EvalException(f->name + ": optional default argument is not an atom " + to_string(atom));
                }
                if (evalArgs_size < f_param_size - 1) {
                    context->put(atom->atom, param->car->cdr->car);
                } else {
                    context->put(atom->atom, evalArgs->car);
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
        if (!is_false(evalArgs->cdr)) {
            evalArgs = evalArgs->cdr;
        }
    }

    // cout << "param count " << f_param_size << " : " << evalArgs_size << endl;
    if (rest) {
        ; // don't worry about counting pararamters
    } else if (optional) {
        f_param_size -= 1;
        if (!(f_param_size == evalArgs_size || f_param_size - 1 == evalArgs_size)) {
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
        // BOOST_LOG_TRIVIAL(debug) << "macro expand: " << to_string(result);
        result = eval(result, a);
    }
    return result;
}

Expr* Evaluator::backquote(Expr* s, shared_ptr<SymbolTable> a)
{
    if (is_atomic(s)) {
        return s;
    }
    if (is_a<Type::list>(s)) {
        auto top = mk_list();
        Expr* p = nullptr;
        for (auto cur = top; !is_false(s); s = s->cdr, cur = cur->cdr) {
            if (is_a<Type::atom>(s->car)) {
                if (s->car->atom == unquote_at->atom || s->car->atom == splice_unquote_at->atom) {
                    if (!is_false(s->cdr)) {
                        auto res = eval(s->cdr->car, a);
                        if (s->car->atom == unquote_at->atom) {
                            cur->car = res;
                            s = s->cdr; // advance
                        } else {
                            if (is_a<Type::list>(res)) {
                                // splice in list
                                while (!is_false(res)) {
                                    cur->car = res->car;
                                    cur->cdr = mk_list();
                                    cur = cur->cdr;
                                    res = res->cdr;
                                }
                            } else {
                                cur->car = res;
                            }
                            s = s->cdr; // advance
                        }
                    }
                } else {
                    cur->car = s->car;
                }
            } else if (is_a<Type::list>(s->car)) {
                cur->car = backquote(s->car, a);
            } else {
                cur->car = s->car;
            }
            cur->cdr = mk_list();
            p = cur;
        }
        p->cdr = nullptr;
        return top;
    }
    return s;
}

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
    if (opt.debug_expr) {
        BOOST_LOG_TRIVIAL(debug) << "eval: " << to_string(e);
    };

    if (is_false(e)) { // stops nullptrs
        return sF;
    }
    // Eval basic types
    switch (e->type) {
    case Type::boolean:
    case Type::integer:
    case Type::floating:
    case Type::character:
    case Type::string:
        return e;

    case Type::atom:
        if (auto val = a->find(e->atom)) {
            return *val;
        }
        throw EvalException("unbound variable: "s + to_string(e));

    case Type::function_ref:
    case Type::function:
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
            if (e->size() == 2) {
                return e->cdr->car;
            }
            throw EvalException("quote: requires one argument");
        }

        // backquote
        if (name == backquote_at->atom) {
            return backquote(e->cdr->car, a);
        }

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
