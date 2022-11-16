//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "evaluator.hh"

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <spdlog/spdlog.h>

#include "exceptions.hh"
#include "function.hh"
#include "linereaderRL.hh"
#include "linereaderReplxx.hh"
#include "linereaderStream.hh"
#include "parser.hh"
#include "primitive.hh"
#include "tracer.hh"

// 👾

namespace ax {

template <class... Ts> struct overloaded : Ts... {
    using Ts::operator()...;
};

template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

const std::string optional_atom{"&optional"};
const std::string rest_atom{"&rest"};
const std::string key_atom{"&key"};

void process_keyword(const std::string &name, Expr &params, const Expr &args, SymbolTable &a) {
    for (auto cur = params; !is_false(cur); cur = cur->cdr) {
        if (is_a<Type::atom>(cur->car)) {
            Atom param = cur->car->atom;
            if (auto res = get_keyword_value(args, mk_keyword(":" + param))) {
                a->put(param, (*res));
            } else {
                a->put(param, sF);
            }
        } else if (is_a<Type::list>(cur->car)) {
            if (cur->car->size() != 2) {
                throw EvalException(name + ": default argument not 2 member list " +
                                    to_string(cur->car));
            }
            if (!is_a<Type::atom>(cur->car->car)) {
                throw EvalException(name + ": default argument name not an atom " +
                                    to_string(cur->car->car));
            }
            Atom param = cur->car->car->atom;
            if (auto res = get_keyword_value(args, mk_keyword(":" + param))) {
                a->put(param, (*res));
            } else {
                a->put(param, cur->car->cdr->car);
            }
        }
    }
}

SymbolTable Evaluator::create_context(Function *f, Expr &args, SymbolTable &a) {
    // BOOST_LOG_TRIVIAL(debug) << "function args: " << to_string(evalArgs);

    size_t evalArgs_size{0};
    Expr   evalArgs{args};
    if (evalArgs) {
        evalArgs_size = evalArgs->size();
    };
    size_t      arg_count = 0;
    bool        optional = false;
    bool        rest = false;
    SymbolTable context = mk_symbol_table(a);
    auto        f_param_size = is_false(f->parameters) ? 0 : f->parameters->size();

    for (auto param = f->parameters; !is_false(param); param = param->cdr) {
        if (is_a<Type::keyword>(param->car) && param->car->keyword == optional_atom) {
            optional = true;
            // cout << "opt args " << endl;
            continue;
        } else if (is_a<Type::keyword>(param->car) && param->car->keyword == rest_atom) {
            rest = true;
            // cout << "rest args " << endl;
            continue;
        } else if (is_a<Type::keyword>(param->car) && param->car->keyword == key_atom) {
            // process all the remain arguments as keyword arguments
            process_keyword(f->name, param->cdr, evalArgs, context);
            return context;
        } else if (is_a<Type::list>(param->car)) {
            if (optional) {
                // get symbol
                if (param->car->size() != 2) {
                    throw EvalException(f->name + ": default argument not 2 member list " +
                                        to_string(param->car));
                }
                auto atom = param->car->car;
                if (!is_a<Type::atom>(atom)) {
                    throw EvalException(f->name + ": optional default argument is not an atom " +
                                        to_string(atom));
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
        if (!is_false(evalArgs)) {
            evalArgs = evalArgs->cdr;
        }
    }

    // cout << "param count " << f_param_size << " : " << evalArgs_size << endl;
    if (rest) {
        ; // don't worry about counting pararamters
    } else if (optional) {
        f_param_size -= 1;
        if (!(f_param_size == evalArgs_size || f_param_size - 1 == evalArgs_size)) {
            throw EvalException(f->name + ": invalid number of arguments");
        }
    } else {
        if (f_param_size != evalArgs_size) {
            throw EvalException(f->name + ": invalid number of arguments");
        }
    }
    return context;
}

Expr Evaluator::perform_function(Function *f, const Expr &args, SymbolTable &a) {
    Expr evalArgs;
    if (f->macro) {
        // Macro args are evaluated later
        evalArgs = args;
    } else {
        evalArgs = eval_list(args, a);
    }

    auto context = create_context(f, evalArgs, a);
    auto result = perform_list(f->body, context);

    if (f->macro) {
        // macros are post-evaluated
        // BOOST_LOG_TRIVIAL(debug) << "macro expand: " << to_string(result);
        result = eval(result, a);
    }
    return result;
}

Expr Evaluator::backquote(Expr s, SymbolTable &a) {
    if (is_atomic(s)) {
        return s;
    }
    if (is_a<Type::list>(s)) {
        auto top = mk_list();
        Expr p = nullptr;
        for (auto cur = top; !is_false(s); s = s->cdr, cur = cur->cdr) {
            if (is_a<Type::atom>(s->car) &&
                (s->car->atom == unquote_at->atom || s->car->atom == splice_unquote_at->atom)) {
                if (!is_false(s->cdr)) {
                    auto res = eval(s->cdr->car, a);
                    if (s->car->atom == unquote_at->atom) {
                        cur->car = res;
                    } else if (is_a<Type::list>(res)) {
                        // splice in list
                        Expr prev = nullptr;
                        for (; !is_false(res); cur = cur->cdr, res = res->cdr) {
                            cur->car = res->car;
                            cur->cdr = mk_list();
                            prev = cur;
                        }
                        if (prev) {
                            cur = prev;
                        }
                    } else {
                        cur->car = res;
                    }
                    s = s->cdr; // advance
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

// eval_list makes a copy of the list.
Expr Evaluator::eval_list(const Expr &expr, SymbolTable &a) {
    Expr e{expr};
    if (is_false(e)) {
        return sF;
    }
    Expr result = mk_list();
    Expr rl = result;
    for (; e; e = e->cdr) {
        if (!rl->car) {
            rl->car = eval(e->car, a);
        } else {
            rl->cdr = mk_list(eval(e->car, a));
            rl = rl->cdr;
        }
    }
    // cout << "eval_list: " << to_dstring(result) << endl;
    return result;
}

Expr Evaluator::perform_list(const Expr &expr, SymbolTable &a) {
    Expr e{expr};
    auto result = sF;
    for (; e; e = e->cdr) {
        result = eval(e->car, a);
    }
    return result;
}

Expr Evaluator::eval(const Expr &e, SymbolTable &a) {
    if (opt.debug_expr) {
        SPDLOG_DEBUG("eval: {}", to_string(e));
    };

    if (is_false(e)) { // stops nullptrs
        return sF;
    }
    // Eval basic types
    switch (e->type) {
    case Type::boolean:
    case Type::integer:
    case Type::floating:
    case Type::complex:
    case Type::character:
    case Type::string:
    case Type::stream:
    case Type::vector:
        return e;

    case Type::atom:
        if (auto val = a->find(e->atom)) {
            return *val;
        }
        throw EvalException("unbound variable: " + to_string(e));

    case Type::function_ref:
    case Type::function:
    case Type::keyword:
        return e;
    case Type::list:; // fallthrough
    }

    // Test for list
    if (!is_list(e)) {
        throw EvalException("Can't evaluate " + to_string(e));
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

        TracerGuard guard;
        // Check traced functions
        if (trace_functions.size() > 0) {
            if (trace_functions.find(name) != trace_functions.end()) {
                // make a tracer
                guard.add_trace(new Tracer(name, to_string(e->cdr)));
            }
        }

        // Lookup symbol table for function
        if (auto f = a->find(name)) {
            if (!is_a<Type::function>(*f)) {
                throw EvalException("A non function in function location " + to_string(*f));
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
                             [&](PrimBasicFunct pf) -> Expr { return pf(result); },
                             [&](PrimSimpleFunct pf) -> Expr { return pf(name, result); },
                             [&](PrimFunct pf) -> Expr { return pf(name, result, a); },
                             [&](PrimFullFunct pf) -> Expr { return pf(*this, name, result, a); },
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

    throw EvalException("Can't evaluate " + to_string(e));
}

bool Evaluator::has_function(const Atom &name) {
    if (auto f = globalTable->find(name)) {
        if (is_a<Type::function>(*f)) {
            return true;
        }
        return false;
    }
    if (auto p = prim_table.find(name); p != prim_table.end()) {
        return true;
    }
    return false;
}

void Evaluator::repl(std::istream &istr, std::ostream &ostr) {
    std::unique_ptr<LineReader> rl;
    if (opt.readline) {
        // rl = make_unique<LineReaderReadLine>();
        rl = std::make_unique<LineReaderReplxx>();
    } else {
        rl = std::make_unique<LineReaderStream>(istr);
    }
    Lexer  lex(*rl);
    Parser parser(lex);
    while (true) {
        ParserResult res;
        try {
            res = parser.parse();
            if (res.eof) {
                break;
            }
            if (opt.parse_only) {
                ostr << to_string(res.val) << std::endl;
                continue;
            }

            auto ex = eval(res.val, globalTable);
            ostr << to_string(ex) << std::endl;

        } catch (UnknownToken &e) {
            ostr << "Unknown token: " << e.tok << std::endl;
            continue;
        } catch (ParseException &e) {
            ostr << "Parse error: " << e.what() << std::endl;
            continue;
        } catch (EndBracketException &e) {
            ostr << "Parse error: Extra bracket found" << std::endl;
            continue;
        } catch (EvalException &e) {
            ostr << "Eval error: " << e.what() << std::endl;
        } catch (NumericException &e) {
            ostr << "Numeric exception: " << e.what() << std::endl;
        } catch (RuntimeException &e) {
            ostr << "Runtime exception: " << e.what() << std::endl;
        } catch (ExceptionQuit &e) {
            break;
        } catch (std::exception &e) {
            ostr << "Exception: " << e.what() << std::endl;
            continue;
        } catch (...) {
            ostr << "Unknown exception!" << std::endl;
            continue;
        }
        if (res.eof) {
            break;
        }
    }
}
} // namespace ax
