//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/log/trivial.hpp>

#include "evaluator.hh"
#include "exceptions.hh"
#include "function.hh"
#include "parser.hh"

namespace ax {

map<string, Primitive> prim_table;
map<Atom, AccessorFunct> setf_accessors;

Expr* atom(Expr* const args)
{
    return is_atomic(args->car) || is_false(args->car) ? sT : sF;
}

Expr* symbolp(Expr* const args)
{
    auto a = args->car;
    if (is_false(a)) {
        return sT;
    }
    if (is_atomic(a) && (is_atom(a) || is_bool(a))) {
        return sT;
    }
    return sF;
}

template <Type T>
Expr* typep(Expr* args)
{
    return is_a<T>(args->car) ? sT : sF;
}

Expr* type_of(Expr* args)
{
    if (is_false(args->car)) {
        return mk_atom(type_null);
    }
    string ret;
    switch (args->car->type) {
    case Type::atom:
        ret = type_atom;
        break;
    case Type::integer:
        ret = type_int;
        break;
    case Type::list:
        ret = type_list;
        break;
    case Type::floating:
        ret = type_float;
        break;
    case Type::string:
        ret = type_string;
        break;
    case Type::character:
        ret = type_char;
        break;
    case Type::function_ref:
        ret = type_funct;
        break;
    case Type::boolean:
        ret = type_bool;
        break;
    default:
        throw EvalException("Uknown type: " + to_string(args->car));
    }
    return mk_atom(ret);
}

Expr* null(Expr* const args)
{
    return is_false(args->car) ? sT : sF;
}

Expr* andor(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (is_false(args)) {
        return name == "and" ? sT : sF;
    }
    auto last = sF;
    for (; args; args = args->cdr) {
        last = l.eval(args->car, a);
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

Expr* carcdr(const string& name, Expr* args)
{
    auto res = args->car;
    if (is_list(res)) {
        if (name == "car" || name == "first") {
            return res->car;
        } else {
            // cdr
            return res->cdr != nullptr ? res->cdr : sF;
        }
    }
    return sF;
}

Expr* consp(Expr* args)
{
    return is_list(args->car) && args->car->car ? sT : sF;
}

Expr* listp(Expr* args)
{
    return is_list(args->car) || is_sF(args->car) ? sT : sF;
}

//   (cons 'a '(x y z)) --> (a x y z)
Expr* cons(Expr* args)
{
    auto result = mk_list(args->car);
    if (!is_false(args->cdr->car)) {
        result->cdr = args->cdr->car;
    }
    return result;
}

Expr* list(Expr* args)
{
    return args;
}

// (rplaca '(a b) 'x) -> (x b)
Expr* rplaca(Expr* args)
{
    if (is_list(args->car)) {
        args->car->car = args->cdr->car;
        return args->car;
    }
    throw EvalException("rplaca first argument not a list");
}

// (rplacd '(a b) 'x) -> (a . x)
Expr* rplacd(Expr* args)
{
    if (is_list(args->car)) {
        args->car->cdr = args->cdr->car;
        return args->car;
    }
    throw EvalException("rplacd first argument not a list");
}

// (append x y)
//
// Takes two lists x, y and appends into one list
//
//  (cond ((null. x) y)
//        ('t (cons (car x) (append. (cdr x) y)))))

Expr* s_append(Expr* x, Expr* y)
{
    if (is_false(x)) {
        return y;
    }
    Expr* res = mk_list(x->car, s_append(x->cdr, y));
    return res;
}

// (append '(a) '(b)) -> (a b)
Expr* append(Expr* args)
{
    if (args->size() == 1) {
        return args->car;
    }
    auto cur = mk_list();
    for (; !is_false(args); args = args->cdr) {
        if (is_list(args->car)) {
            cur = s_append(cur, args->car);
        } else {
            if (is_false(args->car)) {
                cur = s_append(cur, mk_list());
            } else {
                throw EvalException("append: is not a list: " + to_string(args->car));
            }
        }
    }
    return cur;
}

Expr* push(Evaluator& l, const string& name, Expr* const args, shared_ptr<SymbolTable> a)
{
    auto x = l.eval(args->car, a);
    auto val = get_reference("push", arg1(args), a);
    if (is_a<Type::list>(val)) {
        val = mk_list(x, val);
        a->set(arg1(args)->atom, val);
        return val;
    }
    throw EvalException("push: is not a list: " + to_string(arg1(args)));
}

Expr* pop(const string& name, Expr* const args, shared_ptr<SymbolTable> a)
{
    auto val = get_reference("pop", args->car, a);
    if (is_a<Type::list>(val)) {
        auto ret = val->car;
        val = val->cdr == nullptr ? sF : val->cdr;
        a->set(args->car->atom, val);
        return ret;
    }
    if (is_false(val)) {
        return sF;
    }
    throw EvalException("pop: is not a list: " + to_string(args->car));
}

//
// eq functions
//

Expr* eq_p(Expr* args)
{
    return expr_eq(args->car, args->cdr->car);
}

Expr* eql_p(Expr* args)
{
    return expr_eql(args->car, args->cdr->car);
}

Expr* equal_p(Expr* args)
{
    return expr_equal(args->car, args->cdr->car);
}

//
// variable functions
//

Expr* defvar(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (is_false(args)) {
        throw EvalException(name + " needs a name");
    }
    auto list_size = args->size();
    if (list_size > 3) {
        throw EvalException(name + " only takes maximum of 3 arguments");
    }
    auto n = args->car;
    if (!is_a<Type::atom>(n)) {
        throw EvalException(name + " requires a symbol as a first argument");
    }
    if (name == "defvar") {
        if (list_size > 1) {
            auto val = l.eval(args->cdr->car, a);
            a->put(n->atom, val);
        } else {
            a->put(n->atom, sF);
        }
    } else if (name == "defparameter") {
        if (list_size == 1) {
            throw EvalException(name + " needs a value");
        }
        auto val = l.eval(args->cdr->car, a);
        l.globalTable->put(n->atom, val);
    } else if (name == "defconstant") {
        if (list_size == 1) {
            throw EvalException(name + " needs a value");
        }
        if (auto x = a->find(n->atom)) {
            throw RuntimeException(name + " redefined const " + n->atom);
        }
        auto val = l.eval(args->cdr->car, a);
        l.globalTable->put(n->atom, val);
    }
    return n;
}

Expr* setq(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (!args) {
        return sF;
    }
    if (args->size() % 2 != 0) {
        throw EvalException(name + " requires an even number of variables");
    }
    Expr* val = sF;
    while (args) {
        auto n = args->car;
        val = l.eval(args->cdr->car, a);
        if (is_a<Type::atom>(n)) {
            if (!a->set(n->atom, val)) {
                a->put(n->atom, val);
            }
        } else if (is_a<Type::list>(n) && name == "setf") {
            if (n->size() < 1 && !is_a<Type::atom>(n->car)) {
                throw EvalException(name + " expecting accessor name");
            }
            if (auto setf = setf_accessors.find(n->car->atom); setf != setf_accessors.end()) {
                val = setf->second(l, n->cdr, val, a);
            } else {
                throw EvalException(name + " accessor not found: " + n->car->atom);
            }
        } else {
            throw EvalException(name + " requires a symbol as an argument");
        }
        args = args->cdr->cdr; // move forward 2 args
    }
    return val;
}

Expr* makunbound(const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    if (is_a<Type::atom>(args->car)) {
        a->remove(args->car->atom);
    }
    return args->car;
}

//
// Program control
//

Expr* ifFunc(Evaluator& l, const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    size_t size = 0;
    if (args) {
        size = args->size();
    }
    if (size < 2) {
        throw EvalException("if requires 2 or 3 arguments");
    }
    auto res = l.eval(args->car, a);
    if (is_false(res)) {
        if (size < 3) {
            return sF;
        }
        return l.eval(args->cdr->cdr->car, a);
    }
    //
    if (size < 2) {
        return sF;
    }
    return l.eval(args->cdr->car, a);
}

Expr* cond(Evaluator& l, const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    while (args) {
        if (is_a<Type::list>(args->car)) {
            auto first = l.eval(args->car->car, a);
            if (is_false(first)) {
                args = args->cdr;
                continue;
            }
            if (args->car->size() == 1) {
                return first;
            }
            return l.perform_list(args->car->cdr, a);
        }
        throw EvalException("cond: clause " + to_string(args->car) + "is not a list");
    }
    return sF;
}

Expr* progn(Evaluator& l, const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    return l.perform_list(args, a);
}

Expr* prog1(Evaluator& l, const string&, Expr* args, shared_ptr<SymbolTable> a)
{
    if (is_false(args)) {
        return sF;
    }
    auto result = l.eval(args->car, a);
    l.perform_list(args->cdr, a);
    return result;
}

Expr* let(Evaluator& l, const string& name, Expr* args, shared_ptr<SymbolTable> a)
{
    if (!is_a<Type::list>(args->car)) {
        throw EvalException(name + ": expecting a list of bindings");
    }
    shared_ptr<SymbolTable> context = make_shared<SymbolTable>(a.get());

    auto b = args->car;
    while (!is_false(b)) {
        if (!is_a<Type::list>(b->car)) {
            throw EvalException(name + ": expecting a binding " + to_string(b->car));
        }
        auto sym = b->car->car;
        if (!is_a<Type::atom>(sym)) {
            throw EvalException(name + ": expecting a symbol for the binding - " + to_string(sym));
        }
        Expr* val;
        if (name == "let") {
            val = l.eval(b->car->cdr->car, a);
        } else {
            // let*
            val = l.eval(b->car->cdr->car, context);
        }
        context->put(sym->atom, val);
        b = b->cdr;
    }
    return l.perform_list(args->cdr, context);
}

// get a reference, in order to modify it.
Expr* get_reference(const string& name, Expr* ref, shared_ptr<SymbolTable> a)
{
    if (!is_a<Type::atom>(ref)) {
        throw EvalException(name + ": argument needs to a reference");
    }
    if (auto val = a->find(ref->atom)) {
        return *val;
    } else {
        throw EvalException(name + ": undefined variable " + ref->atom);
    }
}

void init_prims()
{
    vector<Primitive> defs{
        { "atom", &atom, one_arg, preEvaluate },
        { "symbolp", &symbolp, one_arg, preEvaluate },
        { "keywordp", &typep<Type::keyword>, one_arg, preEvaluate },
        { "type-of", &type_of, one_arg, preEvaluate },

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

        { "append", &append, no_check, preEvaluate },

        { "push", &push, two_args },
        { "pop", &pop, one_arg },

        // eq

        { "eq", &eq_p, two_args, preEvaluate },
        { "eql", &eql_p, two_args, preEvaluate },
        { "equal", &equal_p, two_args, preEvaluate },

        // // variables

        { "defvar", &defvar, no_check },
        { "defconstant", &defvar, no_check },
        { "defparameter", &defvar, no_check },
        { "setq", &setq, no_check },
        { "setf", &setq, no_check },
        { "makunbound", makunbound, one_arg, preEvaluate },

        // // Program control

        { "if", &ifFunc, no_check },
        { "cond", &cond, no_check },

        { "progn", &progn, no_check },
        { "prog1", &prog1, no_check },

        { "let", &let, min_two },
        { "let*", &let, min_two },

        // // Function
        { "defun", &defun, min_two },
        { "lambda", &lambda, min_one },
        { "defmacro", &defun, min_one },
        { "macro", &lambda, min_one },
        { "functionp", &functionp, min_one, preEvaluate },
        { "fboundp", &fboundp, min_one, preEvaluate },
        { "fmakunbound", makunbound, one_arg, preEvaluate },
        { "function", &funct, one_arg },
        { "apply", &apply, min_two },
        { "funcall", &funcall, min_two },
        { "mapcar", &mapcar, min_two },
        { "maplist", &mapcar, min_two },
        { "dotimes", &doFuncs, min_two },
        { "dolist", &doFuncs, min_two },

        // // Number functions

        { "numberp", &numberp, one_arg, preEvaluate },
        { "integerp", &typep<Type::integer>, one_arg, preEvaluate },
        { "realp", &typep<Type::floating>, one_arg, preEvaluate },
        { "floatp", &typep<Type::floating>, one_arg, preEvaluate },

        { "zerop", zerop, one_num, preEvaluate },
        { "oddp", &nump<1>, one_int, preEvaluate },
        { "evenp", &nump<0>, one_int, preEvaluate },
        { "plusp", plusp, one_num, preEvaluate },
        { "minusp", minusp, one_num, preEvaluate },

        { "=", num_eq, two_num, preEvaluate },
        { "/=", num_neq, two_num, preEvaluate },
        { "<", num_lt, two_num, preEvaluate },
        { "<=", num_le, two_num, preEvaluate },
        { ">", num_gt, two_num, preEvaluate },
        { ">=", num_ge, two_num, preEvaluate },

        { "+", num_add, any_num, preEvaluate },
        { "-", &num_sub_init, any_num, preEvaluate },
        { "*", num_mult, any_num, preEvaluate },
        { "/", check_zeros(num_div), min_one_num, preEvaluate },
        { "mod", check_zeros(num_mod), two_num, preEvaluate },
        { "rem", check_zeros(num_rem), two_num, preEvaluate },
        { "^", num_power, min_one_num, preEvaluate },
        { "expt", num_power, min_one_num, preEvaluate },
        { "max", num_max, min_one_num, preEvaluate },
        { "min", num_min, min_one_num, preEvaluate },

        { "abs", num_abs, one_num, preEvaluate },
        { "floor", num_floor, one_num, preEvaluate },
        { "ceiling", num_ceil, one_num, preEvaluate },
        { "round", num_round, one_num, preEvaluate },
        { "truncate", num_trunc, one_num, preEvaluate },

        { "log", num_log, one_num, preEvaluate },
        { "exp", num_exp, one_num, preEvaluate },
        { "sin", num_sin, one_num, preEvaluate },
        { "cos", num_cos, one_num, preEvaluate },
        { "tan", num_tan, one_num, preEvaluate },
        { "asin", num_asin, one_num, preEvaluate },
        { "acos", num_acos, one_num, preEvaluate },
        { "atan", num_atan, one_num, preEvaluate },
        { "sqrt", num_sqrt, one_num, preEvaluate },

        { "incf", &incf, min_one },
        { "decf", &incf, min_one },
        { "1+", &inc<1>, one_num, preEvaluate },
        { "1-", &inc<-1>, one_num, preEvaluate },
        { "float", &float_f, one_num, preEvaluate },

        // String functions
        { "stringp", &typep<Type::string>, one_arg, preEvaluate },
        { "string=", str_eq, two_str, preEvaluate },
        { "string-equal", str_eq, two_str, preEvaluate },
        { "string/=", str_neq, two_str, preEvaluate },
        { "string-not-equal", str_neq, two_str, preEvaluate },
        { "string<", str_lt, two_str, preEvaluate },
        { "string-lessp", str_lt, two_str, preEvaluate },
        { "string>", str_gt, two_str, preEvaluate },
        { "string-greaterp", str_gt, two_str, preEvaluate },
        { "string<=", str_le, two_str, preEvaluate },
        { "string-not-greaterp", str_le, two_str, preEvaluate },
        { "string>=", str_ge, two_str, preEvaluate },
        { "string-not-lessp", str_ge, two_str, preEvaluate },

        { "string-ci=", funct_ci(str_eq, to_lower_str), two_str, preEvaluate },
        { "string-ci/=", funct_ci(str_neq, to_lower_str), two_str, preEvaluate },
        { "string-ci<", funct_ci(str_lt, to_lower_str), two_str, preEvaluate },
        { "string-ci>", funct_ci(str_gt, to_lower_str), two_str, preEvaluate },
        { "string-ci<=", funct_ci(str_le, to_lower_str), two_str, preEvaluate },
        { "string-ci>=", funct_ci(str_ge, to_lower_str), two_str, preEvaluate },

        { "string", &string_fnct, one_arg, preEvaluate },
        { "string-upcase", &string_fnct, one_arg, preEvaluate },
        { "string-downcase", &string_fnct, one_arg, preEvaluate },

        // Character functions
        { "characterp", &typep<Type::character>, one_arg, preEvaluate },
        { "char=", char_eq, two_char, preEvaluate },
        { "char-equal", char_eq, two_char, preEvaluate },
        { "char/=", char_neq, two_char, preEvaluate },
        { "char-not-equal", char_neq, two_char, preEvaluate },
        { "char<", char_lt, two_char, preEvaluate },
        { "char-lessp", char_lt, two_char, preEvaluate },
        { "char>", char_gt, two_char, preEvaluate },
        { "char-greaterp", char_gt, two_char, preEvaluate },
        { "char<=", char_le, two_char, preEvaluate },
        { "char-not-greaterp", char_le, two_char, preEvaluate },
        { "char>=", char_ge, two_char, preEvaluate },
        { "char-not-lessp", char_ge, two_char, preEvaluate },

        { "char-ci=", funct_ci(char_eq, to_lower_char), two_char, preEvaluate },
        { "char-ci<", funct_ci(char_lt, to_lower_char), two_char, preEvaluate },
        { "char-ci>", funct_ci(char_gt, to_lower_char), two_char, preEvaluate },
        { "char-ci<=", funct_ci(char_le, to_lower_char), two_char, preEvaluate },
        { "char-ci>=", funct_ci(char_ge, to_lower_char), two_char, preEvaluate },

        // // Sequence

        { "length", &length, one_arg, preEvaluate },
        { "elt", &elt, two_args, preEvaluate },
        { "set-elt", &setelt, three_arg, preEvaluate },
        { "subseq", &subseq, min_two, preEvaluate },
        { "make-sequence", &make_sequence, min_two, preEvaluate },

        // I/O
        { "error", &throw_error, one_arg, preEvaluate },
        { "quit", &quit, no_check, preEvaluate },

    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }

    setf_accessors[Atom("elt")] = &setf_elt;
}
}
