//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_eval

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "test.hh"

using namespace std;

BOOST_AUTO_TEST_CASE(test_eval)
{
    vector<TestEval> tests = {
        { "t", "t" },
        { "nil", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_quote)
{
    vector<TestEval> tests = {
        { "(quote a)", "a" },

        { "'a", "a" },
        { "'t", "t" },
        { "'nil", "nil" },
        { "()", "nil" },

        { "(quote)", "nil" },
        { "(quote (a b))", "(a b)" },

        { "a", "Eval error: unbound variable: a" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_atom)
{
    vector<TestEval> tests = {
        { "(atom t)", "t" },
        { "(atom nil)", "t" },
        { "(atom (quote x))", "t" },
        { "(atom (quote (a b)))", "nil" },

        { "(atom t)", "t" },
        { "(atom ())", "t" },
        { "(atom (cdr '(a)))", "t" },
        { "(atom (cdr '(a b)))", "nil" },

        { "(atom 1)", "t" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_symbolp)
{
    vector<TestEval> tests = {
        { "(symbolp 'a)", "t" },
        { "(symbolp '(a b))", "nil" },
        { "(symbolp (car '(a b)))", "t" },
        { "(symbolp t)", "t" },
        { "(symbolp nil)", "t" },
        { "(symbolp '())", "t" },

        { "(symbolp 3)", "nil" },

        { "(symbolp)", "Eval error: symbolp expecting an argument" },
        { "(symbolp nil nil)", "Eval error: symbolp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_null)
{
    vector<TestEval> tests = {
        { "(null nil)", "t" },
        { "(null '())", "t" },

        { "(null 'a)", "nil" },
        { "(null t)", "nil" },
        { "(null 1)", "nil" },
        { "(null '(a b))", "nil" },
        { "(null (list))", "t" },

        { "(null)", "Eval error: null expecting an argument" },
        { "(null nil nil)", "Eval error: null expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_not)
{
    vector<TestEval> tests = {
        { "(not nil)", "t" },
        { "(not '())", "t" },

        { "(not t)", "nil" },
        { "(not 'a)", "nil" },
        { "(not 1)", "nil" },
        { "(not '(a b))", "nil" },
        { "(not (list))", "t" },

        { "(not)", "Eval error: not expecting an argument" },
        { "(not nil nil)", "Eval error: not expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_and)
{
    vector<TestEval> tests = {
        { "(and t t)", "t" },
        { "(and t nil)", "nil" },
        { "(and nil t)", "nil" },
        { "(and nil nil)", "nil" },
        { "(and t t nil)", "nil" }, // multi value and
        { "(and t)", "t" },
        { "(and nil)", "nil" },
        { "(and '())", "nil" },

        { "(and 'nil 'nil)", "nil" },

        { "(and (symbolp (quote x)) (symbolp (quote y)))", "t" },
        { "(and (symbolp (quote x)) (symbolp (quote (y))))", "nil" },
        //{ "(and (symbolp (quote z)) (eq (quote x) (quote x)))", "t" },
        //{ "(and (symbolp (quote z)) (eq (quote x) (quote 2)))", "nil" },

        // R3R2
        { "(and (= 2 2) (> 2 1))", "t" },
        { "(and (= 2 2) (< 2 1))", "nil" },
        { "(and 1 2 'c '(f g))", "(f g)" },

        { "(and)", "t" },
        { "(and 1)", "1" },
        { "(and 1 2)", "2" },
        { "(and 1 2 3)", "3" },
        { "(and 1 2 3 4)", "4" },

        { "(and nil 2)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_or)
{
    vector<TestEval> tests = {
        { "(or t t)", "t" },
        { "(or t nil)", "t" },
        { "(or nil t)", "t" },
        { "(or nil nil)", "nil" },
        { "(or t)", "t" },
        { "(or nil)", "nil" },
        { "(or nil nil t)", "t" },
        { "(or () t)", "t" },
        { "(or ())", "nil" },

        { "(or (symbolp (quote x)) (symbolp (quote y)))", "t" },
        { "(or (symbolp (quote x)) (symbolp (quote (y))))", "t" },
        //{ "(or (symbolp (quote (z y))) (eq (quote x) (quote x)))", "t" },
        //{ "(or (symbolp (quote (z y))) (eq (quote x) (quote 2)))", "nil" },

        // R3R2
        { "(or (= 2 2) (> 2 1))", "t" },
        { "(or (= 2 2) (< 2 1))", "t" },
        { "(or nil nil nil)", "nil" },
        { "(or 'a (/ 3 0))", "a" },

        { "(or)", "nil" },
        { "(or 1)", "1" },
        { "(or 1 2)", "1" },
        { "(or 1 2 3)", "1" },
        { "(or 1 2 3 4)", "1" },

        { "(or nil 2)", "2" },
        { "(or nil nil 3)", "3" },
        { "(or nil nil nil 4)", "4" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_car)
{
    vector<TestEval> tests = {
        { "(car ())", "nil" },
        { "(car nil)", "nil" },

        { "(car (quote (a b c)))", "a" },
        { "(car '(a))", "a" },
        { "(car (quote ((a b) c d)))", "(a b)" },
        // { "(car '(a . b))", "a" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_cdr)
{
    vector<TestEval> tests = {
        { "(cdr ())", "nil" },
        { "(cdr nil)", "nil" },

        { "(cdr '(a b))", "(b)" },
        { "(cdr (quote (a b c)))", "(b c)" },
        { "(cdr (quote (a (b c))))", "((b c))" },
        // { "(cdr '(a . b))", "b" },

        { "(cdr '(a))", "nil" }, // not ()
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_consp)
{
    vector<TestEval> tests = {
        // true if list and not empty
        { "(consp 'a)", "nil" },
        { "(consp nil)", "nil" },
        { "(consp t)", "nil" },
        { "(consp 1)", "nil" },
        { "(consp '())", "nil" },
        { "(consp (list))", "nil" },

        // { "(consp '(a . b))", "t" },
        { "(consp '(a))", "t" },
        // { "(consp (cons nil nil))", "t" },
        { "(consp '(a b c))", "t" },

        // ;; For everything in *universe*, it is either an atom, or satisfies
        // ;; consp, but not both
        // (deftest consp-xor-atom-universe
        //   (check-predicate #'(lambda (x) (or (and (consp x) (not (atom x)))
        //                                      (and (not (consp x)) (atom x)))))
        //   nil)

        { "(consp)", "Eval error: consp expecting an argument" },
        { "(consp 'a 'b)", "Eval error: consp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_listp)
{
    vector<TestEval> tests = {
        // true if list and not empty
        { "(listp 'a)", "nil" },
        { "(listp t)", "nil" },
        { "(listp 1)", "nil" },

        { "(listp '())", "t" },
        { "(listp nil)", "t" }, // this is true as it is also the empty list
        { "(consp '(a))", "t" },
        //{ "(listp '(a . b))", "t" },
        { "(listp '(a b c d e f g h))", "t" },

        { "(listp)", "Eval error: listp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_cons)
{
    vector<TestEval> tests = {
        // { "(cons t t)", "(t . t)" },
        { "(cons t nil)", "(t)" },
        // { "(cons nil t)", "(nil . t)" },
        { "(cons nil nil)", "(nil)" },

        // { "(cons 'a 'b)", "(a . b)" },
        { "(cons 'a nil)", "(a)" },

        { "(cons t '())", "(t)" },

        { "(cons 'a '(x y z))", "(a x y z)" },
        { "(cons '(a b) '(x y z))", "((a b) x y z)" },
        { "(cons '(a b) nil)", "((a b))" },
        { "(cons 'z (cdr '(a b c)))", "(z b c)" },
        { "(cons 'a (cons 'b (cons 'c nil)))", "(a b c)" },

        { "(cons 'z '())", "(z)" },
        // { "(cons '(a b) 'c)", "((a b) . c)" },

        // { "(cons 1 2)", "(1 . 2)" },
        // {`(cons 1 "2")`, `(1. "2")`},
        // { "(cons 1 's)", "(1 . s)" },

        { "(cons 'all (cons (cons 'these (cons 'problems '())) '()))",
            "(all (these problems))" },

        { "(cons)", "Eval error: cons expecting 2 arguments" },
        { "(cons 'a)", "Eval error: cons expecting 2 arguments" },
        { "(cons 'a 'b 'c)", "Eval error: cons expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_list)
{
    vector<TestEval> tests = {
        { "(list)", "nil" },
        { "(list 'a)", "(a)" },
        { "(list 'a 'b)", "(a b)" },
        { "(list 'a 'b 'c)", "(a b c)" },
        { "(list 'a 'b 'c 'd)", "(a b c d)" },
        { "(list 'a 'b '(c1 2) 'd 'e)", "(a b (c1 2) d e)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_rplaca)
{
    vector<TestEval> tests = {
        { "(rplaca '(a b) 'x)", "(x b)" },
        // {"(rplaca '(a . b) 'x)", "(x . b)"},

        { "(rplaca '((a1 a2) b) 'x)", "(x b)" },
        { "(rplaca '(a b) '(c c))", "((c c) b)" },
        //{"(setq x '(1 2))", "(1 2)"},
        //{"x", "(1 2)"},
        //{"(rplaca x 'x)", "(x 2)"},
        //{"x", "(x 2)"},

        //{"(defun f (x) (rplaca x 'a))", "f"},
        //{"(f '(1 2 3))", "(a 2 3)"},

        // errors
        { "(rplaca '(a b))", "Eval error: rplaca expecting 2 arguments" },
        { "(rplaca)", "Eval error: rplaca expecting 2 arguments" },
        { "(rplaca nil 'x)", "Eval error: rplaca first argument not a list" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_rplacd)
{
    vector<TestEval> tests = {
        //{ "(rplacd '(a b) 'x)", "(a . x)" },
        { "(rplacd '(a b) '(x))", "(a x)" },
        //{ "(rplacd '(a . b) 'x)", "(a . x)" },

        { "(rplacd '((a1 a2) b) '(x))", "((a1 a2) x)" },
        { "(rplacd '(a b) '(c c))", "(a c c)" },
        //{ "(setq x '(1 2))", "(1 2)" },
        //{ "x", "(1 2)" },
        //{ "(rplacd x 'x)", "(1 . x)" },
        // {"x", "(1 . x)"}, // this does not work, due to Go's immutable lists

        //{ "(defun f (x) (rplacd x '(a b)))", "f" },
        //{ "(f '(1 2 3))", "(1 a b)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_eq)
{
    auto fmt = boost::format("(eq %1% %1%)");
    vector<TestEval> tests = {
        { "(eq t t)", "t" },
        { "(eq nil nil)", "t" },
        { "(eq t nil)", "nil" },
        { "(eq nil t)", "nil" },
        { "(eq () ())", "t" },

        { "(eq 'a 'a)", "t" },
        { "(eq 'a 'b)", "nil" },

        { "(eq '(a b) '(a b)) ; this is false", "nil" },
        { "(eq '(a b) '(a z))", "nil" },
        { "(eq 'a '(a b))", "nil" },

        // Characters
        // {`(eq #\a #\a)`, "t" },
        // {`(eq #\a #\A)`, "nil" },
        // {`(eq #\α #\α)`, "t" },
        // {`(eq #\β #\α)`, "nil" },
        // {`(eq #\a 1)`, "nil" },

        // Int
        { "(eq 1 1)", "t" },
        { "(eq 1 2)", "nil" },
        { "(eq 0 -0)", "t" },
        { "(eq -1 1)", "nil" },
        { "(eq 1 'a)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        // Real
        // {`(eq 1.1 1.1)`, "t" },
        // {`(eq 1.1 2.1)`, "nil" },
        // {`(eq 0.0 - 0.0)`, "t" },
        // {`(eq - 0.9 0.09)`, "nil" },

        // Errors
        { "(eq)", "Eval error: eq expecting 2 arguments" },
        { "(eq ())", "Eval error: eq expecting 2 arguments" },
        { "(eq nil nil nil)", "Eval error: eq expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_eql)
{
    auto fmt = boost::format("(eql %1% %1%)");
    vector<TestEval> tests = {
        { "(eql '(a) '(b))", "nil" },

        // Characters
        // {`(eql #\a #\a)`, "t" },
        // {`(eql #\a #\A)`, "nil" },
        // {`(eql #\α #\α)`, "t" },
        // {`(eql #\β #\α)`, "nil" },
        // {`(eql #\a 1)`, "nil" },

        // Int
        { "(eql 1 1)", "t" },
        { "(eql 1 2)", "nil" },
        { "(eql 0 -0)", "t" },
        { "(eql -1 1)", "nil" },
        { "(eql 1 'a)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        // Real
        // {`(eql 1.1 1.1)`, "t" },
        // {`(eql 1.1 2.1)`, "nil" },
        // {`(eql 0.0 - 0.0)`, "t" },
        // {`(eql - 0.9 0.09)`, "nil" },

        // Errors
        { "(eql)", "Eval error: eql expecting 2 arguments" },
        { "(eql ())", "Eval error: eql expecting 2 arguments" },
        { "(eql nil nil nil)", "Eval error: eql expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_equal)
{
    auto fmt = boost::format("(equal %1% %1%)");
    vector<TestEval> tests = {
        { "(equal t t)", "t" },
        { "(equal nil nil)", "t" },
        { "(equal t nil)", "nil" },
        { "(equal nil t)", "nil" },
        { "(equal () ())", "t" },

        { "(equal (quote a) (quote a))", "t" },
        { "(equal (quote a) (quote b))", "nil" },
        { "(equal (quote (a b)) (quote (a z)) )", "nil" },
        { "(equal (quote a) (quote (a b)))", "nil" },

        { "(equal 1 1)", "t" },
        { "(equal 0 0)", "t" },
        { "(equal 1 2)", "nil" },
        { "(equal -1 1)", "nil" },
        { "(equal 1 'a)", "nil" },
        { boost::str(fmt % numeric_limits<long>::min()), "t" },
        { boost::str(fmt % (numeric_limits<long>::min() + 1)), "t" },
        { boost::str(fmt % numeric_limits<long>::max()), "t" },
        { boost::str(fmt % (numeric_limits<long>::max() - 1)), "t" },

        // Characters
        // {`(equal #\a #\a)`, "t"},
        // {`(equal #\a #\A)`, "nil"},
        // {`(equal #\α #\α)`, "t"},
        // {`(equal #\β #\α)`, "nil"},
        // {`(equal #\a 1)`, "nil"},

        // Real
        // {`(equal 1.1 1.1)`, "t"},
        // {`(equal 1.1 2.1)`, "nil"},
        // {`(equal 0.0 -0.0)`, "t"},
        // {`(equal -0.9 0.09)`, "nil"},

        // changes from eql
        { "(equal '(a b) '(a b))", "t" },
        //{ "(equal (cons 'a 'b) (cons 'a 'b))", "t" },
        { "(equal (list 'a 'b) (list 'a 'b))", "t" },
        { "(equal (list 'a 'b) (list 'a 'c))", "nil" },
        { "(equal '(a b (c)) '(a b (c)))", "t" },
        { "(equal '(a b) '(a z))", "nil" },
        { "(equal (quote a) (quote (a b)))", "nil" },

        // Strings
        // {`(equal "cat" "cat")`, "t"},
        // {`(equal "cat" "dog")`, "nil"},
        // {`(equal "" "")`, "t"},
        // {`(equal "cat" 'cat)`, "nil"},

        // Errors
        { "(equal)", "Eval error: equal expecting 2 arguments" },
        { "(equal ())", "Eval error: equal expecting 2 arguments" },
        { "(equal nil nil nil)", "Eval error: equal expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_const)
{
    vector<TestEval> tests = {
        { "(defconstant a 1)", "a" },
        { "a", "1" },

        { "(defconstant x '(a b c))", "x" },
        { "(car x)", "a" },
        { "(defconstant y '(i j k))", "y" },
        { "(cdr y)", "(j k)" },
        { "(defconstant z 'c)", "z" },
        { "z", "c" },
        { "(defconstant z 'd)", "Runtime exception: defconstant redefined const z" },
        { "z", "c" },

        //{ R"x((defconstant zz 'd "Documentation"))x", "zz" },

        //{ R"x((defconstant my_pi 3.14 "My pi is better than your pi"))x", "my_pi" },
        //{ "my_pi", "3.14" },

        { "(defparameter xp '(a b c))", "xp" },
        { "(car xp)", "a" },
        { "(defparameter yp '(i j k))", "yp" },
        { "(cdr yp)", "(j k)" },
        { "(defparameter zp 'c)", "zp" },
        { "zp", "c" },
        { "(defparameter zp 'd)", "zp" },
        { "zp", "d" },

        //{ "(defparameter * my_pi * 3.1 \" My pi is better than your pi \")", " * my_pi * " },
        //{ "*my_pi*", "3.1" },

        { "(defconstant)", "Eval error: defconstant needs a name" },
        { "(defconstant w)", "Eval error: defconstant needs a value" },

        // globals - should not change
        { "(defconstant xx 1)", "xx" },
        //{ "(defun f() (defconstant xx '2))", "f" },
        { "xx", "1" },
        //{ "(f)", "xx" },
        //{ "xx", "2" },

        // fail
        { "(defconstant 1 'd)", "Eval error: defconstant requires a symbol as a first argument" },
        //{ "(defconstant \"s\" 'd)", "Error: defconstant: requires a symbol as a first argument" },
        { "(defconstant n1 100 n2 200 )", "Eval error: defconstant only takes maximum of 3 arguments" },

    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_defvar)
{
    vector<TestEval> tests = {
        { "(defvar x '(a b c))", "x" },
        { "(car x)", "a" },
        { "(defvar y '(i j k))", "y" },
        { "(cdr y)", "(j k)" },
        { "(defvar z 'c)", "z" },
        { "z", "c" },
        { "(defvar z 'd)", "z" },
        { "z", "d" },

        //{`(defvar zz 'e "Doc string")`, "zz"},
        //{"zz", "e"},

        { "(defvar)", "Eval error: defvar needs a name" },
        { "(defvar w)", "w" },

        // globals - should not change
        { "(defvar xx 1)", "xx" },
        //{"(defun f() (defvar xx 2))", "f"},
        { "xx", "1" },
        //{"(f)", "xx"},
        { "xx", "1" },

        // locally defined
        //{ "(defun g () (defvar y 1) y)", "g" },
        //{ "(g)", "1" },

        // fail
        { "(defvar 1 'd)", "Eval error: defvar requires a symbol as a first argument" },
        //{ "(defvar \"s\" 'd)", "Error: defvar requires a symbol as a first argument" },
        { "(defvar n1 100 n2 200 )", "Eval error: defvar only takes maximum of 3 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_setq)
{
    vector<TestEval> tests = {
        { "(setq x '(a b c))", "(a b c)" },
        { "(car x)", "a" },
        { "(setq y '(i j k))", "(i j k)" },
        { "(cdr y)", "(j k)" },
        { "(setq z 'c)", "c" },
        { "z", "c" },
        { "(setq z 'd)", "d" },
        { "z", "d" },
        { "(setq n1 100 n2 200 )", "200" },
        { "n1", "100" },
        { "n2", "200" },

        { "(setq a 1 b 2 c 3)", "3" },
        { "a", "1" },
        { "b", "2" },
        { "c", "3" },

        { "(setq)", "nil" },

        { "(setq w)", "Eval error: setq requires an even number of variables" },

        // globals - should change
        { "(setq x '1)", "1" },
        //{ "(defun f() (setq x '2))", "f" },
        { "x", "1" },
        //{ "(f)", "2" },
        //{ "x", "2" },

        // fail
        { "(setq 1 'd)", "Eval error: setq requires a symbol as an argument" },
        //{ "(setq \"s\" 'd)", "Error: setq: requires a var as a first argument\nnil" }
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_setf)
{
    vector<TestEval> tests = {
        { "(setf x '(a b c))", "(a b c)" },
        { "(car x)", "a" },
        { "(setf y '(i j k))", "(i j k)" },
        { "(cdr y)", "(j k)" },
        { "(setf z 'c)", "c" },
        { "z", "c" },
        { "(setf z 'd)", "d" },
        { "z", "d" },
        { "(setf n1 100 n2 200 )", "200" },
        { "n1", "100" },
        { "n2", "200" },

        //{ "(setf)", "Eval error: setf expecting at least 2 argument(s)" },
        //{ "(setf w)", "Eval error: setf expecting at least 2 argument(s)" },

        // fail
        { "(setf 1 'd)", "Eval error: setf requires a symbol as an argument" },
        //{ "(setf \"s\" 'd)", "Error: setf: requires a var or list as a first argument\nnil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_makunbound)
{
    vector<TestEval> tests = {
        { "(setq x '(a b c))", "(a b c)" },
        { "x", "(a b c)" },
        { "(makunbound 'x)", "x" },
        { "x", "Eval error: unbound variable: x" },
        { "(makunbound 'y)", "y" },

        // fail
        { "(makunbound)", "Eval error: makunbound expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_if)
{
    vector<TestEval> tests = {
        { "(if t 'a 'z)", "a" },
        { "(if t 1 2)", "1" },
        { "(if nil 'a 'z)", "z" },
        { "(if nil 1 2)", "2" },

        { "(if t 'a)", "a" },
        { "(if nil 'a)", "nil" },

        { "(if '0 'a 'z)", "a" },

        // Incorrect, return nil
        { "(if t)", "Eval error: if requires 2 or 3 arguments" },
        { "(if nil)", "Eval error: if requires 2 or 3 arguments" },
        { "(if)", "Eval error: if requires 2 or 3 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_cond)
{
    vector<TestEval> tests = {
        { R"((cond (t t) 
			(nil nil)))",
            "t" },
        { R"((cond (t nil) 
			(nil t)))",
            "nil" },
        { R"((cond (nil t) 
			(t nil)) )",
            "nil" },
        { R"((cond (nil nil) 
			(t (quote x))) )",
            "x" },
        { R"((cond (t 1) 
			(nil nil)))",
            "1" },
        { R"((cond (nil 1) 
			(t 2)))",
            "2" },
        { R"((cond (nil 1) 
			(nil 2)
			(t 3)))",
            "3" },

        { R"((cond (() t) 
			(t nil)))",
            "nil" },
        { R"((cond (t (quote a))) )", "a" },
        { R"((cond (nil nil) 
			(t (quote a))) )",
            "a" },
        { R"((cond (nil (quote a))
			(t   (quote (1 2 3)))) )",
            "(1 2 3)" },
        { R"((cond ( (eq (quote a) (quote a))   (quote equal))) )", "equal" },
        { R"((cond ((eq (quote a) (quote b)) (quote first))
			    ((atom (quote a)) (quote second))) )",
            "second" },
        { R"((cond ((eq (quote a) (quote b))   (quote equal))
			    ((quote t) (quote unequal))) )",
            "unequal" },
        { R"((cond ((atom t) (quote is-atom)) 
				(t (quote not-atom))) )",
            "is-atom" },

        { R"((cond (t (quote a)) 
			    (nil (quote b))) )",
            "a" },
        { R"((cond (nil (quote a))
			    (t (quote b))) )",
            "b" },
        { R"((cond (t (quote a))
			    (t (quote b))) )",
            "a" },
        { R"((cond (nil (quote a))
				(nil (quote b))) )",
            "nil" },
        { R"((cond (nil (quote a))
				('b)) )",
            "b" },

        { R"((cond (nil 'a 'a)
			    (t 'b 'c)) )",
            "c" },

        { R"((cond (() t) 
			(t nil)) )",
            "nil" },
        { R"((cond (t 'a)) )", "a" },
        { R"((cond (nil nil) 
			(t 'a)) )",
            "a" },
        { R"((cond (nil (quote a))
			(t '(1 2 3))) )",
            "(1 2 3)" },

        // multiple expressions
        { R"((cond (t 1 2 3) 
			    (nil (quote b))) )",
            "3" },
        { R"((cond (nil (quote a))
				(t 'a 'b)) )",
            "b" },
        // R3RS
        { R"((cond ((> 3 2) 'greater)
			((< 3 2) 'less)) )",
            "greater" },
        { R"((cond ((> 3 3) 'greater)
				((< 3 3) 'less)
				(t 'equal)) )",
            "equal" },

        { "(cond)",
            "nil" },
        { "(cond ('a))", "a" },
        { "(cond (nil))", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_progn)
{
    vector<TestEval> tests = {
        { "(progn 'α 'β 'γ 'δ)", "δ" },
        { "(progn 4)", "4" },
        { "(progn (atom 'a) (atom '(a v)))", "nil" },
        { "(progn)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_prog1)
{
    vector<TestEval> tests = {
        { "(prog1'α 'β 'γ 'δ)", "α" },
        { "(prog1 4)", "4" },
        { "(prog1 (atom 'a) (atom '(a v)))", "t" },
        { "(prog1)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_noargs)
{
    vector<TestEval> tests = {
        { "(defun k () 1)", "k" },
        { "(k)", "1" },
        { "(defun k () 2)", "k" },
        { "(k)", "2" },

        { "(k 1)", "Eval error: k: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_1args)
{
    vector<TestEval> tests = {
        { "(defun id (x) x)", "id" },
        { "(id 1)", "1" },

        { "(id 1 2)", "Eval error: id: invalid number of arguments" },
        { "(id)", "Eval error: id: invalid number of arguments" },

        { "(defun idx (1) x)", "Eval error: idx parameter needs to be an atom :1" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_2args)
{
    vector<TestEval> tests = {
        { "(defun g (x y) x)", "g" },
        { "(g 1 2)", "1" },
        { "(defun g (x y) y)", "g" },
        { "(g 1 2)", "2" },

        { "(g 1)", "Eval error: g: invalid number of arguments" },
        { "(g)", "Eval error: g: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_3args)
{
    vector<TestEval> tests = {
        { "(defun h (x y z) x)", "h" },
        { "(h 1 2 3)", "1" },
        { "(defun h (x y z) y)", "h" },
        { "(h 1 2 3)", "2" },
        { "(defun h (x y z) z)", "h" },
        { "(h 1 2 3)", "3" },

        { "(h 1 2)", "Eval error: h: invalid number of arguments" },
        { "(h 1)", "Eval error: h: invalid number of arguments" },
        { "(h)", "Eval error: h: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_4args)
{
    vector<TestEval> tests = {
        { "(defun hh (x y z a) x)", "hh" },
        { "(hh 1 2 3 4)", "1" },
        { "(defun hh (x y z a) y)", "hh" },
        { "(hh 1 2 3 4)", "2" },
        { "(defun hh (x y z a) z)", "hh" },
        { "(hh 1 2 3 4)", "3" },
        { "(defun hh (x y z a) a)", "hh" },
        { "(hh 1 2 3 4)", "4" },

        { "(hh 1 2 3)", "Eval error: hh: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_defun)
{
    vector<TestEval> tests = {
        // defun.1
        { R"( (defun a (x)
		 	(atom x)) )",
            "a" },
        { "(a t)", "t" },
        { "(a nil)", "t" },
        { "(a 34)", "t" },
        { "(a '(a b))", "nil" },
        { R"( (defun xcadr (x)
		 	(car (cdr x))) )",
            "xcadr" },
        { "(xcadr '(a b))", "b" },
        { "(xcadr 'a)", "nil" },
        { "(xcadr '(a (b c) d))", "(b c)" },

        // multi-expression defines
        { R"( (defun hx (x)
		 	(list 'a x)
		 	(list 'c x)
		 	(list 'z x)) )",
            "hx" },
        { "(hx 1)", "(z 1)" },

        // Doc string
        // { R"( (defun dx (x)
        //  	"Doc string here."
        //  	(list 'd x)) )",
        //     "dx" },
        // { "(dx 1)", "(d 1)" },

        // defun.2
        { "(defun)", "Eval error: defun expecting at least 2 arguments" },
        { "(defun x)", "Eval error: defun expecting at least 2 arguments" },
        { "(defun x y)", "Eval error: x needs a list of parameters" },
        { "(defun x () t)", "x" },
        { "(x)", "t" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_defun_recursive)
{
    vector<TestEval> tests = {
        { R"( (defun factorial (n)
		 	(defun iter (product counter max-count)
		 	  (if (> counter max-count)
		 		  product
		 		  (iter (* counter product)
		 				(+ counter 1)
		 				max-count)))
			 (iter 1 1 n)) )",
            "factorial" },
        { "(factorial 0)", "1" },
        { "(factorial 1)", "1" },
        { "(factorial 4)", "24" },
        { "(factorial 14)", "87178291200" },

        // // defun.3
        { R"( (defun subst(x y z)
                (cond(nil nil)
                     ((atom z) 
                        (cond((eq z y)x)
                              (t z)))
                    (t (cons(subst x y(car z))
                       (subst x y(cdr z)))))) )",
            "subst" },
        { "(subst 'm 'b 'b)", "m" },
        { "(subst 'm 'b '(a b))", "(a m)" },
        { "(subst 'm 'b '(a b (a b c) d))", "(a m (a m c) d)" },

        // Fix for crash with unbound variables
        { "(defun addx (s d) (+ s d))", "addx" },
        { "(addx 1 2)", "3" },
        { "(addx 1)", "Eval error: addx: invalid number of arguments" },

        // Empty missing function body should return nil
        { "(defun fff () )", "fff" },
        { "(fff)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_defun2)
{
    vector<TestEval> tests = {
        { "(defconstant x 1)", "x" },
        { "x", "1" },
        { "(defun k () x)", "k" },
        { "(k)", "1" },

        { "(defvar yy 2)", "yy" },
        { "yy", "2" },
        { "(defun g () (defvar yy 4) yy)", "g" },
        { "yy", "2" },
        { "(g)", "4" },
        { "yy", "2" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_closures)
{
    vector<TestEval> tests = {
        { "(defvar a 1)", "a" },
        { "(defun b () a)", "b" },
        { "(b)", "1" },
        { "(defvar a 3)", "a" },
        { "a", "3" },
        { "(b)", "3" },

        { "(defun c (x) (defun d(y) (+ x y )) (d x))", "c" },
        { "(c 2)", "4" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_undef)
{
    vector<TestEval> tests = {
        { "(defun d (x) (f x))", "d" }, // f not defined yet
        { "(defun f (x) (g x))", "f" }, // g not defined yet
        { "(defun g (x) x)", "g" },
        { "(f 1)", "1" },
        { "(d 3)", "3" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_littlelisper)
{
    vector<TestEval> tests = {
        // The little lisper, pg. 17.
        { R"( (defun lat (l) 
			(cond
				((null l) t)
				((atom (car l)) (lat (cdr l)))
				(t nil))) )",
            "lat" },
        { "(lat '(J S c e n c f))", "t" },
        { "(lat '((J) S c e n c f))", "nil" },
        { "(lat '(J (S c) e n c f))", "nil" },
        { "(lat '())", "t" },
        { R"( (defun lat2(l)(if (null l)
                            t(if (atom(car l))(lat2(cdr l))
                                    nil))) )",
            "lat2" },
        { "(lat2 '(J S c e n c f))", "t" },
        { "(lat2 '((J) S c e n c f))", "nil" },
        { "(lat2 '(J (S c) e n c f))", "nil" },
        { "(lat2 '())", "t" },

        // The Little Lisper, pg. 41.
        { R"( (defun rember (a lat) 
		(cond
			((null lat) '())
			((eq (car lat) a) (cdr lat))
			(t (cons (car lat)
					 (rember a (cdr lat)))))) )",
            "rember" },
        { "(rember 'b  '())", "nil" },
        { "(rember 'b  '(b l a t))", "(l a t)" },
        { "(rember 'a  '(b l a t))", "(b l t)" },
        { "(rember 'x  '(b l a t))", "(b l a t)" },

        // The Little Lisper, pg. 48
        // { R"( (defun firsts (x) 
		// (cond
		// 	((null x) nil)
		// 	(t (cons (caar x)
		// 			 (firsts (cdr x)))))) )",
        //     "firsts" },
        // { "(firsts '())", "nil" },
        // { "(firsts '((a b) (c d) (e f)))", "(a c e)" },
        // { "(firsts '((a b) (c) (d e f)))", "(a c d)" },

        // The Little Lisper, pg. 54
        { R"( (defun insertr (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons old (cons new (cdr lat))))
			(t (cons (car lat) (insertr new old (cdr lat)))))) )",
            "insertr" },
        { "(insertr 'e 'd '(a b c d f g d))", "(a b c d e f g d)" },

        { R"( (defun insertl (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons new (cons old (cdr lat))))
			(t (cons (car lat) (insertl new old (cdr lat)))))) )",
            "insertl" },
        { "(insertl 'e 'd '(a b c d f g d))", "(a b c e d f g d)" },

        { R"( (defun subst (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons new (cdr lat)))
			(t (cons (car lat) (subst new old (cdr lat)))))) )",
            "subst" },
        { "(subst 'e 'd '(a b c d f g d))", "(a b c e f g d)" },

        { R"( (defun subst2 (new o1 o2 lat) 
		(cond
			((null lat) nil)
			((or (eq (car lat) o1) (eq (car lat) o2))
				(cons new (cdr lat)))
			(t (cons (car lat) (subst2 new o1 o2 (cdr lat)))))) )",
            "subst2" },
        { "(subst2 'e 'd 'f '(a b c d f g d))", "(a b c e f g d)" },
        { "(subst2 'e 'd 'f '(a b c f g d))", "(a b c e g d)" },

        // Little Lisp pg.
        { R"( (defun eqanp (a1 a2)
		(cond ((and (numberp a1) (numberp a2)) (= a1 a2))
			(t (eq a1 a2)))) )",
            "eqanp" },
        { R"( (defun eqlist (x1 x2)
		(cond 
			((and (null x1) (null x2)) t)
			((or (null x1) (null x2)) nil)
			((and (not (atom (car x1))) (not (atom (car x2))))
				(and (eqlist (car x1) (car x2))
					 (eqlist (cdr x1) (cdr x2))))
			((or (not (atom (car x1))) (not (atom (car x2)))) nil)
			(t (and (eqanp (car x1) (car x2))
					(eqlist (cdr x1) (cdr x2))))
		)) )",
            "eqlist" },

        { "(eqlist '() '())", "t" },
        { "(eqlist '() '(s))", "nil" },
        { "(eqlist '(1) '(1))", "t" },
        { "(eqlist '(1 2) '(1 2))", "t" },
        { "(eqlist '(1 2) '(1 3))", "nil" },
        { "(eqlist '(a b c f g d) '(a b c f g d))", "t" },
        { "(eqlist '(a (b) c f g d) '(a (b) c f g d))", "t" },
        { "(eqlist '(a (b) c f g d) '(a (b) c c g d))", "nil" },
    };
    test_Evaluator(tests);
}
