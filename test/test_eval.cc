//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_eval
#include <boost/format.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

using namespace ax;
using namespace std;
namespace logging = boost::log;

struct TestEval {
    string input;
    string output;
};

void test_Evaluator(const vector<TestEval>& tests);

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

        { "a", "Eval error: Can't evaluate a" },
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
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_symbolp)
{
    vector<TestEval> tests = {
        { "(symbolp 'a)", "t" },
        { "(symbolp '(a b))", "nil" },
        //{ "(symbolp (car '(a b)))", "t" },
        { "(symbolp t)", "t" },
        { "(symbolp nil)", "t" },
        { "(symbolp '())", "t" },

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
        //{ "(null 1)", "nil" },
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
        //{"(not 1)", "nil"},
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

        //{ "(and (symbolp (quote x)) (symbolp (quote y)))", "t" },
        //{ "(and (symbolp (quote x)) (symbolp (quote (y))))", "nil" },
        //{ "(and (symbolp (quote z)) (eq (quote x) (quote x)))", "t" },
        //{ "(and (symbolp (quote z)) (eq (quote x) (quote 2)))", "nil" },

        // R3R2
        //{"(and (= 2 2) (> 2 1))", "t"},
        //{"(and (= 2 2) (< 2 1))", "nil"},
        // {"(and 1 2 'c '(f g))", "(f g)"},

        { "(and)", "t" },
        //{ "(and 1)", "1" },
        //{ "(and 1 2)", "2" },
        //{ "(and 1 2 3)", "3" },
        //{ "(and 1 2 3 4)", "4" },

        //{ "(and nil 2)", "nil" },
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

        //{ "(or (symbolp (quote x)) (symbolp (quote y)))", "t" },
        //{ "(or (symbolp (quote x)) (symbolp (quote (y))))", "t" },
        //{ "(or (symbolp (quote (z y))) (eq (quote x) (quote x)))", "t" },
        //{ "(or (symbolp (quote (z y))) (eq (quote x) (quote 2)))", "nil" },

        // R3R2
        //{"(or (= 2 2) (> 2 1))", "t"},
        //{"(or (= 2 2) (< 2 1))", "t"},
        { "(or nil nil nil)", "nil" },
        //{"(or 'a (/ 3 0))", "a"},

        //{ "(or)", "nil" },
        //{ "(or 1)", "1" },
        //{ "(or 1 2)", "1" },
        //{ "(or 1 2 3)", "1" },
        //{ "(or 1 2 3 4)", "1" },

        //{ "(or nil 2)", "2" },
        //{ "(or nil nil 3)", "3" },
        //{ "(or nil nil nil 4)", "4" },
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
        // { "(consp 1)", "nil" },
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
        //{ "(listp 1)", "nil" },

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

void test_Evaluator(const vector<TestEval>& tests)
{
    Options options;
    options.silent = true;
    options.readline = false;

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::info);
    Lisp lisp(options);
    lisp.init();

    for (auto test : tests) {
        istringstream is(test.input);
        ostringstream out;

        //BOOST_TEST_CHECKPOINT(test.input);
        lisp.repl(is, out);

        string result = out.str();
        result.pop_back(); // chop off \n
        cout << "eval: " << test.input << " -> " << result << endl;
        if (test.output != result) {
            BOOST_ERROR(boost::format("\n%1% should be: %3%, \n      not:  %2%") % test.input % result % test.output);
            continue;
        }
    }
}
