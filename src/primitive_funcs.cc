//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include "exceptions.hh"
#include "parser.hh"
#include "symboltable.hh"

namespace ax {

//
// Function functions
//

Function createFunction(const std::string &name, const Expr &args) {
    if (!is_a<Type::list>(args->car)) {
        throw EvalException(name + " needs a list of parameters");
    }
    for (auto p = args->car; !is_false(p); p = p->cdr) {
        if (!(is_a<Type::atom>(p->car) || is_a<Type::keyword>(p->car) ||
              is_a<Type::list>(p->car))) {
            throw EvalException(name + " parameter needs to be an atom :" + to_string(p->car));
        }
    }
    Function f = mk_function(name, args->car);
    if (args->size() > 1) {
        f->body = args->cdr;
    } else {
        f->body = sF;
    }
    return f;
}

Expr defun(const std::string &name, const Expr &args, SymbolTable &a) {
    if (!is_a<Type::atom>(args->car)) {
        throw EvalException(name + " function name needs to an atom");
    }
    auto fname = args->car->atom;
    auto f = createFunction(fname, args->cdr);
    if (name == "defmacro") {
        f->macro = true;
    }
    a->put(fname, mk_function(f));
    return args->car;
}

Expr lambda(const std::string &name, const Expr &args) {
    auto f = createFunction(name, args);
    if (name == "macro") {
        f->macro = true;
    }
    return mk_function(f);
}

Expr funct(const std::string &name, const Expr &args) {
    if (is_a<Type::atom>(args->car)) {
        return mk_function_ref(args->car->atom);
    }
    throw EvalException(name + " function name needs to an atom");
}

Expr find_funct(std::string &n, SymbolTable &a) {
    if (auto p = prim_table.find(n); p != prim_table.end()) {
        return sT;
    }
    if (auto fs = a->find(n)) {
        if (is_a<Type::function>(*fs)) {
            return sT;
        }
    }
    return sF;
}

Expr functionp(const std::string &, const Expr &args, SymbolTable &a) {
    if (is_a<Type::function>(args->car)) { // This is the difference
        return sT;
    } else if (is_a<Type::function_ref>(args->car)) {
        return find_funct(args->car->function_ref, a);
    }
    return sF;
}

// Like functionp but works on atoms
Expr fboundp(const std::string &, const Expr &args, SymbolTable &a) {
    if (!is_a<Type::atom>(args->car)) {
        return sF;
    } else {
        return find_funct(args->car->atom, a);
    }
    return sF;
}

Expr get_function(Evaluator &l, const std::string &name, const Expr &arg, SymbolTable &a) {
    auto fn = l.eval(arg, a);
    if (is_a<Type::function_ref>(fn)) {
        return mk_list(mk_atom(fn->function_ref));
    } else if (is_a<Type::function>(fn)) {
        return mk_list(fn);
    } else {
        throw EvalException(name + ": Not function ref or lambda expression: " + to_string(fn));
    }
    return sF;
}

Expr apply(Evaluator &l, const std::string &name, const Expr &args, SymbolTable &a) {
    auto ex = get_function(l, name, args->car, a);
    auto res = l.eval(args->cdr->car, a);
    if (is_a<Type::list>(res)) {
        // new to transform list into quoted elements
        auto new_res = mk_list();
        Expr prev = nullptr;
        auto nrt = new_res;
        for (auto rt = res; !is_false(rt); rt = rt->cdr, nrt = nrt->cdr) {
            nrt->car = mk_list({quote_at, rt->car});
            nrt->cdr = mk_list();
            prev = nrt;
        }
        prev->cdr = nullptr;
        ex->cdr = new_res;
    } else {
        ex->cdr = mk_list(res);
    }
    return l.eval(ex, a);
}

Expr funcall(Evaluator &l, const std::string &name, const Expr &args, SymbolTable &a) {
    auto ex = get_function(l, name, args->car, a);
    ex->cdr = args->cdr;
    return l.eval(ex, a);
}

Expr mapcar(Evaluator &l, const std::string &name, const Expr &args, SymbolTable &a) {
    auto ex = get_function(l, name, args->car, a);
    auto evalargs = l.eval_list(args->cdr, a);
    for (auto x = evalargs; !is_false(x); x = x->cdr) {
        if (!is_a<Type::list>(x->car)) {
            throw EvalException(name + ": expecting list arguments " + to_string(x->car));
        }
    }
    Expr resulttop = mk_list();
    auto result = resulttop;
    Expr prev = nullptr;
    for (unsigned int i = 0;; ++i) {
        auto topr = mk_list(ex->car);
        auto r = topr;
        for (auto elem = evalargs; !is_false(elem); elem = elem->cdr) {
            if (i >= elem->car->size()) {
                goto end;
            }
            Expr val;
            if (name == "mapcar") {
                val = elem->car->at(i);
            } else {
                val = elem->car->from(i);
            }
            r->cdr = mk_list(mk_list({quote_at, val}));
            r = r->cdr;
        }

        auto res = l.eval(topr, a);
        result->car = res;
        result->cdr = mk_list();
        prev = result;
        result = result->cdr;
    }
    if (prev) {
        prev->cdr = nullptr;
    }
end:
    return resulttop;
}

Expr do_times(Evaluator &l, const std::string &name, const Expr &args, SymbolTable &a) {
    if (!is_a<Type::list>(args->car)) {
        throw EvalException(name + ": has no parameter list " + to_string(args->car));
    }
    auto params = args->car;
    if (params->size() < 2) {
        throw EvalException(name + ": not enough vars in parameter list");
    }
    auto        variable = arg0(params);
    auto        limit = arg1(params);
    SymbolTable context = mk_symbol_table(a);
    Expr        result = nullptr;
    if (params->size() >= 3) {
        result = arg2(params);
        if (!is_a<Type::atom>(result)) {
            throw EvalException(name + ": result is not a symbol" + to_string(result));
        }
    }
    // Eval limit
    auto res_limit = l.eval(limit, a);
    Expr variable_value;
    Int  variable_max;
    if (name == "dotimes") {
        if (!is_a<Type::integer>(res_limit)) {
            throw EvalException(name + ": limit is not a integer " + to_string(res_limit));
        }
        variable_value = mk_int(0);
        variable_max = res_limit->integer;
    } else {
        // dolist
        if (!is_a<Type::list>(res_limit)) {
            throw EvalException(name + ": expecting a list " + to_string(res_limit));
        }
        if (is_false(res_limit)) {
            return sF;
        }
        variable_value = res_limit->car;
    }

    while (true) {
        context->put(variable->atom, variable_value);

        // perform body
        l.eval_list(args->cdr, context);

        // increment counters
        if (name == "dotimes") {
            if (variable_value->integer >= variable_max - 1) {
                goto end;
            }
            variable_value->integer++;
        } else {
            if (is_false(res_limit->cdr)) {
                goto end;
            }
            res_limit = res_limit->cdr;
            variable_value = res_limit->car;
        }
    }
end:
    if (!is_false(result)) {
        if (auto val = context->find(result->atom)) {
            return *val;
        }
    }
    return sF;
}

// (do (params) ((test) return) x1 x2 ...)
Expr do_func(Evaluator &l, const std::string &name, const Expr &args, SymbolTable &a) {
    if (!is_a<Type::list>(args->car)) {
        throw EvalException(name + ": has no parameter list " + to_string(args->car));
    }
    SymbolTable context = mk_symbol_table(a);
    for (auto params = args->car; !is_false(params); params = params->cdr) {
        if (!is_a<Type::list>(params->car)) {
            throw EvalException(name + ": parameter is not a list " + to_string(params->car));
        }
        if (params->car->size() < 2) {
            throw EvalException(name + ": not enough vars in parameter list");
        }
        auto variable = arg0(params->car);
        if (!is_atom(variable)) {
            throw EvalException(name + ": parameter is not a symbol " + to_string(variable));
        }
        Expr value = nullptr;
        if (name == "do") {
            value = l.eval(arg1(params->car), a);
        } else {
            value = l.eval(arg1(params->car), context);
        }
        context->put(variable->atom, value);
    }

    auto ret = arg1(args);
    if (!is_a<Type::list>(ret)) {
        throw EvalException(name + ": expecting a test " + to_string(arg1(args)));
    }
    auto test = arg0(ret);
    Expr ret_val = sF;
    if (ret->size() >= 2) {
        ret_val = arg1(ret);
    }
    for (auto test_val = l.eval(test, context); is_false(test_val);
         test_val = l.eval(test, context)) {
        l.perform_list(args->cdr->cdr, context);

        // update parameters
        for (auto params = args->car; !is_false(params); params = params->cdr) {
            if (!is_a<Type::list>(params->car)) {
                throw EvalException(name + ": parameter is not a list " + to_string(params->car));
            }
            if (params->car->size() == 3) {
                auto variable = arg0(params->car);
                auto value = l.eval(arg2(params->car), context);
                context->put(variable->atom, value);
            }
        }
    }
    return l.eval(ret_val, context);
}
} // namespace ax
