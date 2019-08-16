//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "primitive.hh"

#include <cmath>
#include <numeric>
#include <vector>

#include <boost/log/trivial.hpp>

#include "evaluator.hh"
#include "exceptions.hh"
#include "function.hh"
#include "parser.hh"

namespace ax {

map<string, Primitive> prim_table;

Expr atom(List& args)
{
    return is_atomic(args[0]) || is_false(args[0]);
}

Expr symbolp(List& args)
{
    auto a = args.front();
    if (is_atomic(a)
        && (is_a<Atom>(a) || is_a<Bool>(a))) {
        return sT;
    }
    if (is_a<List>(a) && any_cast<List>(a).empty()) {
        return sT;
    }
    return sF;
}

template <typename T>
Expr typep(List& args)
{
    return is_a<T>(args[0]);
}

Expr null(List& args)
{
    return is_false(args.front());
}

Expr andor(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    if (args.empty()) {
        return name == "and";
    }
    Expr last = sF;
    for (auto& x : args) {
        last = l.eval(x, a);
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

Expr consp(List& args)
{
    auto res = args.front();
    return is_a<List>(res) && any_cast<List>(res).size() > 0;
}

Expr listp(List& args)
{
    auto res = args.front();
    return is_a<List>(res) || is_sF(res);
}

//   (cons 'a '(x y z)) --> (a x y z)
Expr cons(List& args)
{
    if (is_a<List>(args[1])) {
        List list = any_cast<List>(args[1]);
        List res;
        res.reserve(1 + list.size());
        res.push_back(args[0]);
        res.insert(res.end(), list.begin(), list.end());
        return res;

    } else if (is_sF(args[1])) {
        return List(1, args[0]);
    }
    return sF;
}

Expr list(List& args)
{
    if (args.empty()) {
        return sF;
    }
    return args;
}

Expr rplaca(List& args)
{
    if (is_a<List>(args[0])) {
        auto l = any_cast<List>(args[0]);
        l[0] = args[1];
        return l;
    }
    throw EvalException("rplaca first argument not a list");
}

Expr rplacd(List& args)
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
    throw EvalException("rplacd first argument not a list");
}

//
// eq functions
//

Expr eq_p(List& args)
{
    return expr_eq(args[0], args[1]);
}

Expr eql_p(List& args)
{
    return expr_eql(args[0], args[1]);
}

Expr equal_p(List& args)
{
    return expr_equal(args[0], args[1]);
}

//
// variable functions
//

Expr defvar(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    if (args.empty()) {
        throw EvalException(name + " needs a name");
    }
    if (args.size() > 3) {
        throw EvalException(name + " only takes maximum of 3 arguments");
    }
    auto n = args[0];
    if (!is_a<Atom>(n)) {
        throw EvalException(name + " requires a symbol as a first argument");
    }
    if (name == "defvar") {
        if (args.size() > 1) {
            auto val = l.eval(args[1], a);
            a.put(any_cast<Atom>(n), val);
        } else {
            a.put(any_cast<Atom>(n), sF);
        }
    } else if (name == "defparameter") {
        if (args.size() == 1) {
            throw EvalException(name + " needs a value");
        }
        auto val = l.eval(args[1], a);
        a.put(any_cast<Atom>(n), val); // should be global symbol table
    } else if (name == "defconstant") {
        if (args.size() == 1) {
            throw EvalException(name + " needs a value");
        }
        if (auto x = a.find(any_cast<Atom>(n))) {
            throw RuntimeException(name + " redefined const " + any_cast<Atom>(n));
        }
        auto val = l.eval(args[1], a);
        a.put(any_cast<Atom>(n), val); // should be global symbol table
    }
    return n;
}

Expr setq(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    if (args.size() % 2 != 0) {
        throw EvalException(name + " requires an even number of variables");
    }
    Expr val = sF;
    for (auto x = args.begin(); x != args.end(); x++) {
        auto n = *x;
        x++;
        auto second = *x;
        if (!is_a<Atom>(n)) {
            throw EvalException(name + " requires a symbol as an argument");
        }
        val = l.eval(second, a);
        a.put(any_cast<Atom>(n), val);
    }
    return val;
}

Expr makunbound(const string&, List& args, SymbolTable& a)
{
    if (is_a<Atom>(args[0])) {
        a.remove(any_cast<Atom>(args[0]));
    }
    return args[0];
}

//
// Program control
//

Expr ifFunc(Evaluator& l, const string&, List& args, SymbolTable& a)
{
    if (args.size() < 2) {
        throw EvalException("if requires 2 or 3 arguments");
    }
    auto res = l.eval(args[0], a);
    if (is_false(res)) {
        if (args.size() < 3) {
            return sF;
        }
        return l.eval(args[2], a);
    }
    //
    if (args.size() < 2) {
        return sF;
    }
    return l.eval(args[1], a);
}

Expr cond(Evaluator& l, const string&, List& args, SymbolTable& a)
{
    for (auto clause : args) {
        if (is_a<List>(clause)) {
            auto clauseList = any_cast<List>(clause);
            auto first = l.eval(clauseList[0], a);
            if (is_false(first)) {
                continue;
            }
            if (clauseList.size() == 1) {
                return first;
            }
            auto stats = List(clauseList.begin() + 1, clauseList.end());
            return l.perform_list(stats, a);
        }
        return EvalException("cond: clause " + to_string(clause) + "is not a list");
    }
    return sF;
}

Expr progn(Evaluator& l, const string&, List& args, SymbolTable& a)
{
    return l.perform_list(args, a);
}

Expr prog1(Evaluator& l, const string&, List& args, SymbolTable& a)
{
    if (args.empty()) {
        return sF;
    }
    auto result = l.eval(args[0], a);
    auto rest = List(args.begin() + 1, args.end());
    l.perform_list(rest, a);
    return result;
}

//
// Function functions
//

Function createFunction(const string& name, List args)
{
    if (!is_a<List>(args[0])) {
        throw EvalException(name + " needs a list of parameters");
    }
    for (auto p : any_cast<List>(args[0])) {
        if (!(is_a<Atom>(p) || is_a<Keyword>(p) || is_a<List>(p))) {
            throw EvalException(name + " parameter needs to be an atom :" + to_string(p));
        }
    }
    Function f(name, any_cast<List>(args[0]));
    if (args.size() > 1) {
        f.body = List(args.begin() + 1, args.end());
    } else {
        f.body = List();
    }
    return f;
}

Expr defun(const string& name, List& args, SymbolTable& a)
{
    if (!is_a<Atom>(args[0])) {
        throw EvalException(name + " function name needs to an atom");
    }
    auto fname = any_cast<Atom>(args[0]);
    Function f = createFunction(fname, List(args.begin() + 1, args.end()));
    if (name == "defmacro") {
        f.macro = true;
    }
    a.put(fname, f);
    return fname;
}

Expr lambda(const string& name, List& args)
{
    auto f = createFunction(name, args);
    if (name == "macro") {
        f.macro = true;
    }
    return f;
}

Expr funct(const string& name, List& args)
{
    if (is_a<Atom>(args[0])) {
        return FunctionRef(any_cast<Atom>(args[0]));
    }
    throw EvalException(name + " function name needs to an atom");
}

template <typename T>
Expr find_funct(const T& f, SymbolTable& a)
{
    if (auto p = prim_table.find(any_cast<T>(f)); p != prim_table.end()) {
        return sT;
    }
    if (auto fs = a.find(any_cast<T>(f))) {
        if (is_a<Function>(*fs)) {
            return sT;
        }
    }
    return sF;
}

Expr functionp(const string&, List& args, SymbolTable& a)
{
    auto f = args[0];
    if (is_a<Function>(f)) { // This is the difference
        return sT;
    } else if (is_a<FunctionRef>(f)) {
        return find_funct<FunctionRef>(any_cast<FunctionRef>(f), a);
    }
    return sF;
}

// Like functionp but works on atoms
Expr fboundp(const string&, List& args, SymbolTable& a)
{
    auto f = args[0];
    if (!is_a<Atom>(f)) {
        return sF;
    } else {
        return find_funct<Atom>(any_cast<Atom>(f), a);
    }
    return sF;
}

List get_function(Evaluator& l, const string& name, Expr& arg, SymbolTable& a)
{
    auto fn = l.eval(arg, a);
    List ex;
    if (is_a<FunctionRef>(fn)) {
        ex.push_back(Atom(any_cast<FunctionRef>(fn)));
    } else if (is_a<Function>(fn)) {
        ex.push_back(fn);
    } else {
        throw EvalException(name + ": Not function ref or lambda expression: " + to_string(fn));
    }
    return ex;
}

Expr apply(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    List ex = get_function(l, name, args[0], a);
    auto res = l.eval(args[1], a);
    if (is_a<List>(res)) {
        for (auto x : any_cast<List>(res)) {
            List elem{ quote_atom, x };
            ex.push_back(elem);
        }
    } else {
        ex.push_back(res);
    }
    Expr e{ ex };
    return l.eval(e, a);
}

Expr funcall(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    List ex = get_function(l, name, args[0], a);
    ex.insert(ex.end(), args.begin() + 1, args.end());
    Expr e{ ex };
    return l.eval(e, a);
}

Expr mapcar(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    List ex = get_function(l, name, args[0], a);
    List aargs = List(args.begin() + 1, args.end());
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

//
// Number Functions
//

PrimBasicFunct numeric_predicate0(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](List& args) {
        return f(any_cast<Int>(args[0]), 0);
    };
}

static function<bool(Int, Int)> eq = equal_to<Int>();
static function<bool(Int, Int)> neq = not_equal_to<Int>();
static function<bool(Int, Int)> gt = greater<Int>();
static function<bool(Int, Int)> ge = greater_equal<Int>();
static function<bool(Int, Int)> lt = less<Int>();
static function<bool(Int, Int)> le = less_equal<Int>();

static PrimBasicFunct zerop = numeric_predicate0(eq);
static PrimBasicFunct plusp = numeric_predicate0(gt);
static PrimBasicFunct minusp = numeric_predicate0(lt);

template <Int N>
Expr nump(List& args)
// Generates a templated function which mods compared to N.
{
    return abs(any_cast<Int>(args[0]) % 2) == N;
}

PrimBasicFunct numeric_predicate(const function<bool(Int, Int)>& f)
// Returns a function with compare the first element to zero.
{
    return [&](List& args) -> Expr {
        return f(any_cast<Int>(args[0]), any_cast<Int>(args[1]));
    };
}

static PrimBasicFunct num_eq = numeric_predicate(eq);
static PrimBasicFunct num_neq = numeric_predicate(neq);
static PrimBasicFunct num_gt = numeric_predicate(gt);
static PrimBasicFunct num_ge = numeric_predicate(ge);
static PrimBasicFunct num_lt = numeric_predicate(lt);
static PrimBasicFunct num_le = numeric_predicate(le);

static function<Int(Int, Int)> add = plus<Int>();
static function<Int(Int, Int)> sub = minus<Int>();
static function<Int(Int, Int)> mult = multiplies<Int>();
static function<Int(Int, Int)> div = divides<Int>();
static function<Int(Int, Int)> mod = modulus<Int>();

PrimBasicFunct numeric_operation(const function<Int(Int, Int)>& f, Int s)
// Returns a function implementing the function f across the list of arguments.
{
    return [=](List& args) -> Expr {
        if (args.empty()) {
            return s;
        }
        // auto iter = args.begin();
        // auto acc = any_cast<Int>(*iter);
        // for (iter++; iter != args.end(); iter++) {
        //     acc = f(acc, any_cast<Int>(*iter));
        // }
        // return acc;
        return accumulate(args.begin() + 1,
            args.end(),
            any_cast<Int>(*args.begin()),
            [=](Expr a, Expr b) -> Int {
                return f(any_cast<Int>(a), any_cast<Int>(b));
            });
    };
}

static PrimBasicFunct num_add = numeric_operation(add, 0);
static PrimBasicFunct num_sub = numeric_operation(sub, 0);
static PrimBasicFunct num_mult = numeric_operation(mult, 1);
static PrimBasicFunct num_div = numeric_operation(div, 1);
static PrimBasicFunct num_mod = numeric_operation(mod, 0);

Expr num_sub_init(List& args)
{
    if (args.size() == 1) {
        return -any_cast<Int>(args[0]);
    }
    return num_sub(args);
}

PrimBasicFunct check_zeros(PrimBasicFunct f)
{
    return [=](List& args) -> Expr {
        for_each(args.begin() + 1,
            args.end(),
            [](Expr e) { if (any_cast<Int>(e) == 0) {
                throw NumericException("divide by zero");
            } });
        return f(args);
    };
}

static PrimBasicFunct num_power = numeric_operation(
    [](Int a, Int b) -> Int { return Int(pow(a, b)); },
    0);

static PrimBasicFunct num_max = numeric_operation(
    [](Int a, Int b) -> Int { return Int(max<Int>(a, b)); },
    0);
static PrimBasicFunct num_min = numeric_operation(
    [](Int a, Int b) -> Int { return Int(min<Int>(a, b)); },
    0);

PrimBasicFunct numeric_single(const function<Int(Int)>& f)
// Returns a function implementing the function f on one argument.
{
    return [=](List& args) -> Expr {
        return f(any_cast<Int>(args[0]));
    };
}

static PrimBasicFunct num_abs = numeric_single([](Int x) -> Int { return Int(abs(x)); });
static PrimBasicFunct num_floor = numeric_single([](Int x) -> Int { return Int(floor(x)); });
static PrimBasicFunct num_ceil = numeric_single([](Int x) -> Int { return Int(ceil(x)); });
static PrimBasicFunct num_round = numeric_single([](Int x) -> Int { return Int(round(x)); });
static PrimBasicFunct num_trunc = numeric_single([](Int x) -> Int { return Int(trunc(x)); });

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

        // eq

        { "eq", &eq_p, two_args, preEvaluate },
        { "eql", &eql_p, two_args, preEvaluate },
        { "equal", &equal_p, two_args, preEvaluate },

        // variables

        { "defvar", &defvar, no_check },
        { "defconstant", &defvar, no_check },
        { "defparameter", &defvar, no_check },
        { "setq", &setq, no_check },
        { "setf", &setq, no_check },
        { "makunbound", makunbound, one_arg, preEvaluate },

        // Program control

        { "if", &ifFunc, no_check },
        { "cond", &cond, no_check },

        { "progn", &progn, no_check },
        { "prog1", &prog1, no_check },

        // Function
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

        // Number functions

        { "numberp", &typep<Int>, one_arg, preEvaluate },
        { "integerp", &typep<Int>, one_arg, preEvaluate },

        { "zerop", zerop, one_num, preEvaluate },
        { "oddp", &nump<1>, one_num, preEvaluate },
        { "evenp", &nump<0>, one_num, preEvaluate },
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
        { "/", check_zeros(num_div), any_num, preEvaluate },
        { "mod", check_zeros(num_mod), two_num, preEvaluate },
        { "^", num_power, min_one_num, preEvaluate },
        { "expt", num_power, min_one_num, preEvaluate },
        { "max", num_max, min_one_num, preEvaluate },
        { "min", num_min, min_one_num, preEvaluate },

        { "abs", num_abs, one_num, preEvaluate },
        { "floor", num_floor, one_num, preEvaluate },
        { "ceiling", num_ceil, one_num, preEvaluate },
        { "round", num_round, one_num, preEvaluate },
        { "truncate", num_trunc, one_num, preEvaluate },

    };

    for (auto p : defs) {
        prim_table[p.name] = p;
    }
}
}
