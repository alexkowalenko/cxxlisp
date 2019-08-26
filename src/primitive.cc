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

Expr type_of(List& args)
{
    auto e = args[0];
    if (is_false(e)) {
        return type_null;
    }
    if (is_a<Atom>(e)) {
        return type_atom;
    } else if (is_a<List>(e)) {
        return type_list;
    } else if (is_a<Int>(e)) {
        return type_int;
    } else if (is_a<Float>(e)) {
        return type_float;
    } else if (is_a<String>(e)) {
        return type_string;
    } else if (is_a<Char>(e)) {
        return type_char;
    } else if (is_a<FunctionRef>(e)) {
        return type_funct;
    } else if (is_a<Bool>(e)) {
        return type_bool;
    }
    throw EvalException("Uknown type: " + to_string(e));
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
        return List{ args[0] };
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

Expr reverse(List& args)
{
    if (is_false(args[0])) {
        return sF;
    }
    if (is_a<List>(args[0])) {
        List l = any_cast<List>(args[0]);
        reverse(l.begin(), l.end());
        return l;
    }
    throw EvalException("reverse: argument not a list");
}

Expr append(List& args)
{
    if (args.empty()) {
        return sF;
    }
    List l;
    for (auto a : args) {
        if (is_a<List>(a)) {
            List x = any_cast<List>(a);
            l.insert(l.end(), x.begin(), x.end());
        } else if (!is_false(a)) {
            l.push_back(a);
        }
    }
    return l;
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
        val = l.eval(second, a);
        if (is_a<Atom>(n)) {
            if (!a.set(any_cast<Atom>(n), val)) {
                a.put(any_cast<Atom>(n), val);
            }
        } else if (is_a<List>(n) && name == "setf") {
            List accessor = any_cast<List>(n);
            if (accessor.size() < 1 && is_a<Atom>(accessor[0])) {
                throw EvalException(name + " expecting accessor name");
            }
            List aargs(accessor.begin() + 1, accessor.end());
            if (auto setf = setf_accessors.find(any_cast<Atom>(accessor[0])); setf != setf_accessors.end()) {
                val = setf->second(l, aargs, val, a);
            } else {
                throw EvalException(name + " accessor not found: " + any_cast<Atom>(accessor[0]));
            }
        } else {
            throw EvalException(name + " requires a symbol as an argument");
        }
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
            List stats(clauseList.begin() + 1, clauseList.end());
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
    List rest(args.begin() + 1, args.end());
    l.perform_list(rest, a);
    return result;
}

Expr let(Evaluator& l, const string& name, List& args, SymbolTable& a)
{
    if (!is_a<List>(args[0])) {
        throw EvalException(name + ": expecting a list of bindings");
    }
    SymbolTable context(&a);
    for (auto b : any_cast<List>(args[0])) {
        if (!is_a<List>(b)) {
            throw EvalException(name + ": expecting a binding " + to_string(b));
        }
        Expr sym = any_cast<List>(b)[0];
        if (!is_a<Atom>(sym)) {
            throw EvalException(name + ": expecting a symbol for the binding - " + to_string(sym));
        }
        Expr val;
        if (name == "let") {
            val = l.eval(any_cast<List>(b)[1], a);
        } else {
            // let*
            val = l.eval(any_cast<List>(b)[1], context);
        }
        context.put(any_cast<Atom>(sym), val);
    }
    List code(args.begin() + 1, args.end());
    return l.perform_list(code, context);
}

// Strings

static function<bool(String, String)> eq_str = equal_to<String>();
static function<bool(String, String)> neq_str = not_equal_to<String>();
static function<bool(String, String)> gt_str = greater<String>();
static function<bool(String, String)> ge_str = greater_equal<String>();
static function<bool(String, String)> lt_str = less<String>();
static function<bool(String, String)> le_str = less_equal<String>();

static PrimBasicFunct str_eq = predicate<String>(eq_str);
static PrimBasicFunct str_neq = predicate<String>(neq_str);
static PrimBasicFunct str_gt = predicate<String>(gt_str);
static PrimBasicFunct str_ge = predicate<String>(ge_str);
static PrimBasicFunct str_lt = predicate<String>(lt_str);
static PrimBasicFunct str_le = predicate<String>(le_str);

// Chars

static function<bool(Char, Char)> eq_char = equal_to<Char>();
static function<bool(Char, Char)> neq_char = not_equal_to<Char>();
static function<bool(Char, Char)> gt_char = greater<Char>();
static function<bool(Char, Char)> ge_char = greater_equal<Char>();
static function<bool(Char, Char)> lt_char = less<Char>();
static function<bool(Char, Char)> le_char = less_equal<Char>();

static PrimBasicFunct char_eq = predicate<Char>(eq_char);
static PrimBasicFunct char_neq = predicate<Char>(neq_char);
static PrimBasicFunct char_gt = predicate<Char>(gt_char);
static PrimBasicFunct char_ge = predicate<Char>(ge_char);
static PrimBasicFunct char_lt = predicate<Char>(lt_char);
static PrimBasicFunct char_le = predicate<Char>(le_char);

void init_prims()
{
    vector<Primitive> defs{
        { "atom", &atom, one_arg, preEvaluate },
        { "symbolp", &symbolp, one_arg, preEvaluate },
        { "keywordp", &typep<Keyword>, one_arg, preEvaluate },
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

        { "reverse", &reverse, one_arg, preEvaluate },
        { "append", &append, no_check, preEvaluate },
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

        { "let", &let, min_two },
        { "let*", &let, min_two },

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
        { "dotimes", &doFuncs, min_two },
        { "dolist", &doFuncs, min_two },

        // Number functions

        { "numberp", &numberp, one_arg, preEvaluate },
        { "integerp", &typep<Int>, one_arg, preEvaluate },
        { "realp", &typep<Float>, one_arg, preEvaluate },
        { "floatp", &typep<Float>, one_arg, preEvaluate },

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

        // String functions
        { "stringp", &typep<String>, one_arg, preEvaluate },
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

        { "string-ci=",
            funct_ci(str_eq, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }), two_str, preEvaluate },
        { "string-ci/=",
            funct_ci(str_neq, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }),
            two_str, preEvaluate },
        { "string-ci<",
            funct_ci(str_lt, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }),
            two_str, preEvaluate },
        { "string-ci>",
            funct_ci(str_gt, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }),
            two_str, preEvaluate },
        { "string-ci<=",
            funct_ci(str_le, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }),
            two_str, preEvaluate },
        { "string-ci>=",
            funct_ci(str_ge, [](const Expr& s) -> Expr { return String(boost::algorithm::to_lower_copy(wstring(any_cast<String>(s)))); }),
            two_str, preEvaluate },

        { "string", &string_fnct, one_arg, preEvaluate },
        { "string-upcase", &string_fnct, one_arg, preEvaluate },
        { "string-downcase", &string_fnct, one_arg, preEvaluate },

        // Character functions
        { "characterp", &typep<Char>, one_arg, preEvaluate },
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

        { "char-ci=",
            funct_ci(char_eq, [](const Expr& c) -> Expr { return Char(tolower(any_cast<Char>(c))); }), two_char, preEvaluate },
        { "char-ci<", funct_ci(char_lt, [](const Expr& c) -> Expr { return Char(tolower(any_cast<Char>(c))); }), two_char, preEvaluate },
        { "char-ci>", funct_ci(char_gt, [](const Expr& c) -> Expr { return Char(tolower(any_cast<Char>(c))); }), two_char, preEvaluate },
        { "char-ci<=", funct_ci(char_le, [](const Expr& c) -> Expr { return Char(tolower(any_cast<Char>(c))); }), two_char, preEvaluate },
        { "char-ci>=", funct_ci(char_ge, [](const Expr& c) -> Expr { return Char(tolower(any_cast<Char>(c))); }), two_char, preEvaluate },

        // Sequence

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
