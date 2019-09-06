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

BOOST_AUTO_TEST_CASE(test_eval_basic)
{
    vector<TestEval> tests = {
        { ":keyword", ":keyword" },
        { "&keyword", "&keyword" },
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

        { "(quote)", "Eval error: quote: requires one argument" },
        { "(quote (a b))", "(a b)" },
        { "(quote (a b(a b)))", "(a b (a b))" },

        { "'a", "a" },
        { "'λ", "λ" },
        { "'א", "א" },
        { "'七", "七" },
        { "'👾", "👾" },
        { "'🍊🍎🍉🍌", "🍊🍎🍉🍌" },

        { "a", "Eval error: unbound variable: a" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_backquote)
{
    vector<TestEval> tests = {
        { "(backquote a)", "a" },
        { "`a", "a" },

        { "(backquote (a b))", "(a b)" },
        { "`(a b)", "(a b)" },

        // test comma
        { "(defvar x 'test)", "x" },
        { "`(this is a var)", "(this is a var)" },
        { "`(this is a ,x)", "(this is a test)" },
        { "`(this is a (,x ,x))", "(this is a (test test))" },

        // test splice
        { "(setq var '(more difficult test))", "(more difficult test)" },
        { "`(this is a var)", "(this is a var)" },
        { "`(this is a ,var)", "(this is a (more difficult test))" },
        { "`(this is a ,@var)", "(this is a more difficult test)" },

        { "(defvar x '(1 2 3))", "x" },
        { "`(0 ,@x 4)", "(0 1 2 3 4)" },
        { "(defvar x 'a)", "x" },
        { "`(0 ,@x 4)", "(0 a 4)" },
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

        { "(atom #\\c)", "t" },

        { "(atom 1)", "t" },
        { "(atom 1.273)", "t" },

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

BOOST_AUTO_TEST_CASE(test_eval_typeof)
{
    vector<TestEval> tests = {
        { "(type-of 'a)", "symbol" },
        { "(type-of '(a b))", "cons" },
        { "(type-of 1)", "integer" },
        { "(type-of -2.3)", "float" },
        { "(type-of \"123\")", "string" },
        { "(type-of #\\c)", "char" },
        { "(type-of #'atom)", "function" },
        { "(type-of t)", "boolean" },
        { "(type-of nil)", "null" },
        { "(type-of '())", "null" },

        { "(type-of)", "Eval error: type-of expecting an argument" },
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
        { "(and (symbolp (quote z)) (eq (quote x) (quote x)))", "t" },
        { "(and (symbolp (quote z)) (eq (quote x) (quote 2)))", "nil" },

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
        { "(or (symbolp (quote (z y))) (eq (quote x) (quote x)))", "t" },
        { "(or (symbolp (quote (z y))) (eq (quote x) (quote 2)))", "nil" },

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
        { "(consp (cons nil nil))", "t" },
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
        { "(cons t t)", "(t . t)" },
        { "(cons t nil)", "(t)" },
        { "(cons nil t)", "(nil . t)" },
        { "(cons nil nil)", "(nil)" },

        { "(cons 'a 'b)", "(a . b)" },
        { "(cons 'a nil)", "(a)" },

        { "(cons t '())", "(t)" },

        { "(cons 'a '(x y z))", "(a x y z)" },
        { "(cons '(a b) '(x y z))", "((a b) x y z)" },
        { "(cons '(a b) nil)", "((a b))" },
        { "(cons 'z (cdr '(a b c)))", "(z b c)" },
        { "(cons 'a (cons 'b (cons 'c nil)))", "(a b c)" },

        { "(cons 'z '())", "(z)" },
        { "(cons '(a b) 'c)", "((a b) . c)" },

        { "(cons 1 2)", "(1 . 2)" },
        { "(cons 1 \"2\")", "(1 . \"2\")" },
        { "(cons 1 's)", "(1 . s)" },

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

/*

BOOST_AUTO_TEST_CASE(test_eval_reverse)
{
    vector<TestEval> tests = {
        { "(reverse)", "Eval error: reverse expecting an argument" },
        { "(reverse 'a)", "Eval error: reverse: argument not a list" },
        { "(reverse '(a ))", "(a)" },
        { "(reverse '(a b))", "(b a)" },
        { "(reverse '(a b c))", "(c b a)" },
        { "(reverse '(a b c d))", "(d c b a)" },
        { "(reverse '(a b (c1 2) d e))", "(e d (c1 2) b a)" },
        { "(reverse '())", "nil" },
        { "(reverse nil)", "nil" },
    };
    test_Evaluator(tests);
}
*/

BOOST_AUTO_TEST_CASE(test_eval_append)
{
    vector<TestEval> tests = {
        { "(append)", "nil" },
        { "(append '())", "nil" },
        { "(append '(a))", "(a)" },
        { "(append '(a) '(b))", "(a b)" },
        { "(append '(a b) '(c))", "(a b c)" },
        { "(append '(a b) '(c) '(d))", "(a b c d)" },
        { "(append '(a) '(b (c1 2)) '(d) '(e))", "(a b (c1 2) d e)" },
        // { "(append '(a b) '(c . d))", "(a b c . d)" },
        { "(append 'a)", "a" },
        { "(append '(a b c) '(d e f) '() '(g))", "(a b c d e f g)" },
        { "(append '(a b c) 'd)", "(a b c . d)" },

        //{ "(append nil '(a))", "(a)" },
        // { "(append '() '(a))", "(a)" },

        { "(append '(a) '() '(b))", "(a b)" },
        { "(append '(a) nil '(b))", "(a b)" },

        { "(append '() '())", "nil" },
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
        { "(setq x '(1 2))", "(1 2)" },
        { "x", "(1 2)" },
        { "(rplaca x 'x)", "(x 2)" },
        { "x", "(x 2)" },

        { "(defun f (x) (rplaca x 'a))", "f" },
        { "(f '(1 2 3))", "(a 2 3)" },

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
        { "(rplacd '(a b) 'x)", "(a . x)" },
        { "(rplacd '(a b) '(x))", "(a x)" },
        //{ "(rplacd '(a . b) 'x)", "(a . x)" },

        { "(rplacd '((a1 a2) b) '(x))", "((a1 a2) x)" },
        { "(rplacd '(a b) '(c c))", "(a c c)" },
        { "(setq x '(1 2))", "(1 2)" },
        { "x", "(1 2)" },
        { "(rplacd x 'x)", "(1 . x)" },
        { "x", "(1 . x)" },

        { "(defun f (x) (rplacd x '(a b)))", "f" },
        { "(f '(1 2 3))", "(1 a b)" },
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
        { "(eq #\\a #\\a)", "t" },
        { "(eq #\\a #\\A)", "nil" },
        { "(eq #\\α #\\α)", "t" },
        { "(eq #\\β #\\α)", "nil" },
        { "(eq #\\a 1)", "nil" },

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
        { "(eq 1.1 1.1)", "t" },
        { "(eq 1.1 2.1)", "nil" },
        { "(eq 0.0 -0.0)", "t" },
        { "(eq -0.9 0.09)", "nil" },

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
        { "(eql #\\a #\\a)", "t" },
        { "(eql #\\a #\\A)", "nil" },
        { "(eql #\\α #\\α)", "t" },
        { "(eql #\\β #\\α)", "nil" },
        { "(eql #\\a 1)", "nil" },

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
        { "(eql 1.1 1.1)", "t" },
        { "(eql 1.1 2.1)", "nil" },
        { "(eql 0.0 -0.0)", "t" },
        { "(eql -0.9 0.09)", "nil" },

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

        // // Characters
        { "(equal #\\a #\\a)", "t" },
        { "(equal #\\a #\\A)", "nil" },
        { "(equal #\\α #\\α)", "t" },
        { "(equal #\\β #\\α)", "nil" },
        { "(equal #\\a 1)", "nil" },

        // // Real
        { "(equal 1.1 1.1)", "t" },
        { "(equal 1.1 2.1)", "nil" },
        { "(equal 0.0 -0.0)", "t" },
        { "(equal -0.9 0.09)", "nil" },

        // changes from eql
        { "(equal '(a b) '(a b))", "t" },
        { "(equal (cons 'a 'b) (cons 'a 'b))", "t" },
        { "(equal (list 'a 'b) (list 'a 'b))", "t" },
        { "(equal (list 'a 'b) (list 'a 'c))", "nil" },
        { "(equal '(a b (c)) '(a b (c)))", "t" },
        { "(equal '(a b) '(a z))", "nil" },
        { "(equal (quote a) (quote (a b)))", "nil" },

        // Strings
        { "(equal \"cat\" \"cat\")", "t" },
        { "(equal \"cat\" \"dog\")", "nil" },
        { "(equal \"\" \"\")", "t" },
        { "(equal \"cat\" 'cat)", "nil" },

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

        { R"x((defconstant zz 'd "Documentation"))x", "zz" },

        { R"x((defconstant my_pi 3.14 "My pi is better than your pi"))x", "my_pi" },
        { "my_pi", "3.14" },

        { "(defparameter xp '(a b c))", "xp" },
        { "(car xp)", "a" },
        { "(defparameter yp '(i j k))", "yp" },
        { "(cdr yp)", "(j k)" },
        { "(defparameter zp 'c)", "zp" },
        { "zp", "c" },
        { "(defparameter zp 'd)", "zp" },
        { "zp", "d" },

        { "(defparameter *my_pi* 3.1 \" My pi is better than your pi \")", "*my_pi*" },
        { "*my_pi*", "3.1" },

        { "(defconstant)", "Eval error: defconstant needs a name" },
        { "(defconstant w)", "Eval error: defconstant needs a value" },

        // globals - should not change
        { "(defconstant xx 1)", "xx" },
        { "(defun f() (defconstant xx '2))", "f" },
        { "xx", "1" },
        { "(f)", "Runtime exception: defconstant redefined const xx" },
        { "xx", "1" },

        // fail
        { "(defconstant 1 'd)", "Eval error: defconstant requires a symbol as a first argument" },
        { "(defconstant \"s\" 'd)", "Eval error: defconstant requires a symbol as a first argument" },
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

        { "(defvar zz 'e \"Doc string\")", "zz" },
        { "zz", "e" },

        { "(defvar)", "Eval error: defvar needs a name" },
        { "(defvar w)", "w" },

        // globals - should not change
        { "(defvar xx 1)", "xx" },
        { "(defun f() (defvar xx 2))", "f" },
        { "xx", "1" },
        { "(f)", "xx" },
        { "xx", "1" },

        // locally defined
        { "(defun g () (defvar y 1) y)", "g" },
        { "(g)", "1" },

        // fail
        { "(defvar 1 'd)", "Eval error: defvar requires a symbol as a first argument" },
        { "(defvar \"s\" 'd)", "Eval error: defvar requires a symbol as a first argument" },
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

        // should change
        { "(setq x '1)", "1" },
        { "(defun f() (setq x '2))", "f" },
        { "x", "1" },
        { "(f)", "2" },
        { "x", "2" },

        // fail
        { "(setq 1 'd)", "Eval error: setq requires a symbol as an argument" },
        { "(setq \"s\" 'd)", "Eval error: setq requires a symbol as an argument" }
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

        { "(setf)", "nil" },
        { "(setf w)", "Eval error: setf requires an even number of variables" },

        // fail
        { "(setf 1 'd)", "Eval error: setf requires a symbol as an argument" },
        { "(setf \"s\" 'd)", "Eval error: setf requires a symbol as an argument" },
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
        { "(progn '🍎 '🍐 '🍊 '🍌)", "🍌" },
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
        { "(prog1 '🍎 '🍐 '🍊 '🍌)", "🍎" },
        { "(prog1 4)", "4" },
        { "(prog1 (atom 'a) (atom '(a v)))", "t" },
        { "(prog1)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_let)
{
    vector<TestEval> tests = {
        { "(let ((x 2) ) (+ x x))", "4" },
        { "(let ((x 2) (y 3)) (* x y))", "6" },
        { "(let ((x 2) (y 3)) (* x y) (* x x))", "4" },
        { "(let () (defvar zzz 6) zzz)", "6" },

        { "(let  ((x 1) (y (+ x 1))) y)", "Eval error: unbound variable: x" },
        { "(let* ((x 1) (y (+ x 1))) y)", "2" },
        { "(let  ((x 1)) y)", "Eval error: unbound variable: y" },

        { "(let () 1)", "1" },
        { "(let () 1 2 3)", "3" },
        { "(let () 1)", "1" },
        { "(let ((x 1) (y 2) (z 3)) (list x y z))", "(1 2 3)" },

        { R"( (let ((x 0))
                (let ((x 1)
                     (y (* x 1)))
                  y)) )",
            "0" },
        { R"( (let ((x 0))
                (let ((x 1))
                    (let ((y (* x 1)))
                y))) )",
            "1" },

        { "(let* () 1)", "1" },
        { "(let* () 1 2 3)", "3" },
        { "(let* ((x 'first)) x)", "first" },
        { "(let* ((x 'first) (y 'second) (z 'third)) (list x y z))", "(first second third)" },

        { R"( (let* ((x 0))
                (let* ((x 1)
                    (y (* x 5)))
                y)) )",
            "5" },

        { R"( (let* ((even? (lambda (n)
         					(if (zerop n)
         						t
         						(oddp (- n 1)))))
         			(odd? (lambda (n)
         					(if (zerop n)
         						nil
         						(evenp (- n 1))))))
         			(even? 88)) )",
            "t" },

        { "(let)", "Eval error: let expecting at least 2 arguments" },
        { "(let 1)", "Eval error: let expecting at least 2 arguments" },
        { "(let 1 2)", "Eval error: let: expecting a list of bindings" },
        { "(let (1 2) 1)", "Eval error: let: expecting a binding 1" },
    };
    test_Evaluator(tests);
}
