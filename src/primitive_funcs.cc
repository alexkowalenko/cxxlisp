//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include "exceptions.hh"
#include "parser.hh"

namespace ax {

//
// Function functions
//

Function* createFunction(const string& name, Expr* args)
{
    if (!is_a<Type::list>(args->car)) {
        throw EvalException(name + " needs a list of parameters");
    }
    auto p = args->car;
    while (!is_false(p)) {
        if (!(is_a<Type::atom>(p->car) || is_a<Type::keyword>(p->car) || is_a<Type::list>(p->car))) {
            throw EvalException(name + " parameter needs to be an atom :" + to_string(p->car));
        }
        p = p->cdr;
    }
    Function* f = new (GC) Function(name, args->car);
    if (size_list(args) > 1) {
        f->body = args->cdr;
    } else {
        f->body = sF;
    }
    return f;
}

Expr* defun(const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
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

Expr* lambda(const string& name, Expr* args)
{
    auto f = createFunction(name, args);
    if (name == "macro") {
        f->macro = true;
    }
    return mk_function(f);
}

Expr* funct(const string& name, Expr* args)
{
    if (is_a<Type::atom>(args->car)) {
        return mk_function_ref(args->car->atom);
    }
    throw EvalException(name + " function name needs to an atom");
}

Expr* find_funct(string& n, shared_ptr<SymbolTable> a)
{
    cout << n << endl;
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

Expr* functionp(const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    if (is_a<Type::function>(args->car)) { // This is the difference
        return sT;
    } else if (is_a<Type::function_ref>(args->car)) {
        return find_funct(args->car->function_ref, a);
    }
    return sF;
}

// Like functionp but works on atoms
Expr* fboundp(const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    if (!is_a<Type::atom>(args->car)) {
        return sF;
    } else {
        return find_funct(args->car->atom, a);
    }
    return sF;
}

Expr* get_function(Evaluator& l, const string& name, Expr* arg, shared_ptr<SymbolTable> a)
{
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

Expr* apply(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    auto ex = get_function(l, name, args->car, a);
    auto res = l.eval(args->cdr->car, a);
    if (is_a<Type::list>(res)) {
        // new to transform list into quoted elements
        auto new_res = mk_list();
        auto nrt = new_res;
        auto rt = res;
        Expr* prev = nullptr;
        while (!is_false(rt)) {
            nrt->car = mk_list({ quote_at, rt->car });
            nrt->cdr = mk_list();
            prev = nrt;
            nrt = nrt->cdr;
            rt = rt->cdr;
        }
        prev->cdr = nullptr;
        ex->cdr = new_res;
    } else {
        ex->cdr = res;
    }
    return l.eval(ex, a);
}

Expr* funcall(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    auto ex = get_function(l, name, args->car, a);
    ex->cdr = args->cdr;
    return l.eval(ex, a);
}

/*
Expr mapcar(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    List ex = get_function(l, name, args[0], a);
    List aargs(args.begin() + 1, args.end());
    auto evalargs = l.eval_list(aargs, a);
    for (auto x : evalargs) {
        if (!is_a<List>(x)) {
            throw EvalException(name + ": expecting list arguments " + to_string(x));
        }
    }
    List result;
    for (unsigned int i = 0;; ++i) {
        List r = ex;
        for (auto elem : evalargs) {
            if (i >= any_cast<List>(elem).size()) {
                goto end;
            }
            Expr val;
            if (name == "mapcar") {
                val = any_cast<List>(elem)[i];
            } else {
                List list = any_cast<List>(elem);
                val = List(list.begin() + i, list.end());
            }
            List q = { quote_atom, val };
            r.push_back(q);
        }
        Expr e{ r };
        auto res = l.eval(e, a);
        result.push_back(res);
    }
end:
    return result;
}

Expr doFuncs(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (!is_a<List>(args[0])) {
        throw EvalException(name + ": has no parameter list " + to_string(args[0]));
    }
    List params = any_cast<List>(args[0]);
    if (params.size() < 2) {
        throw EvalException(name + ": not enough vars in parameter list");
    }
    Expr variable = params[0];
    Expr limit = params[1];
    SymbolTable context(&a);
    Expr result;
    if (params.size() >= 3) {
        result = params[2];
        if (!is_a<Atom>(result)) {
            throw EvalException(name + ": result is not a symbol" + to_string(result));
        }
        context.put(any_cast<Atom>(result), sF);
    }
    // Eval limit
    Expr res_limit = l.eval(limit, a);
    Expr variable_value;
    int counter = 0;
    int variable_max;
    if (name == "dotimes") {
        if (!is_a<Int>(res_limit)) {
            throw EvalException(name + ": limit is not a integer " + to_string(res_limit));
        }
        variable_value = Int(0);
        variable_max = any_cast<Int>(res_limit);
    } else {
        // dolist
        if (!is_a<List>(res_limit)) {
            throw EvalException(name + ": expecting a list " + to_string(res_limit));
        }
        if (any_cast<List>(res_limit).empty()) {
            return sF;
        }
        variable_value = any_cast<List>(res_limit).front();
    }
    context.put(any_cast<Atom>(variable), variable_value);

    while (true) {
        if (name == "dotimes") {
            if (any_cast<Int>(variable_value) >= variable_max) {
                goto end;
            }
        }

        // perform body
        List body(args.begin() + 1, args.end());
        l.eval_list(body, context);

        // increment counters
        if (name == "dotimes") {
            variable_value = Int(any_cast<Int>(variable_value) + 1);
        } else {
            counter++;
            if (counter >= any_cast<List>(res_limit).size()) {
                goto end;
            }
            variable_value = any_cast<List>(res_limit)[counter];
        }
        context.put(any_cast<Atom>(variable), variable_value);
    }
end:
    if (result.has_value()) {
        if (auto val = context.find(any_cast<Atom>(result))) {
            return *val;
        }
    }
    return sF;
}
*/
}
