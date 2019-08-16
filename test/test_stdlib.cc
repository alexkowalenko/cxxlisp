//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_stdlib

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "expr.hh"

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_cadr)
{
    vector<TestEval> tests = {
        { "(cadr '(a b))", "b" },
        { "(cadr '(a (b c)))", "(b c)" },

        { "(caar '((a b) x))", "a" },
        { "(caar '((a)))", "a" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_second)
{
    vector<TestEval> tests = {
        { "(second '(a b c d))", "b" },
        { "(third '(a b c d))", "c" },
        { "(fourth '(a b c d))", "d" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_when)
{
    vector<TestEval> tests = {
        { "(when t 'true)", "true" },
        { "(when (atom 's) 'is-atom)", "is-atom" },
        { "(when (atom '(a b)) 'is-atom)", "nil" },
        { "(when nil 'true)", "nil" },
        { "(when (not (atom 's)) 'is-not-atom)", "nil" },
        { "(when (not (atom '(a b))) 'is-not-atom)", "is-not-atom" },
        { "(when t (progn 1 2 3 4))", "4" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_unless)
{
    vector<TestEval> tests = {
        { "(unless t 'true)", "nil" },
        { "(unless (atom 's) 'is-atom)", "nil" },
        { "(unless (atom '(a b)) 'is-no-atom)", "is-no-atom" },
        { "(unless nil 'true)", "true" },
        { "(unless (not (atom 's)) 'is-not-atom)", "is-not-atom" },
        { "(unless (not (atom '(a b))) 'is-not-atom)", "nil" },
        { "(unless nil (progn 1 2 3 4))", "4" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_member)
{
    vector<TestEval> tests = {
        { "(member)", "Eval error: member: invalid number of arguments" },
        { "(member '())", "Eval error: member: invalid number of arguments" },
        { "(member 'a 'a)", "nil" },

        { "(member 'a '(a b c))", "(a b c)" },
        { "(member 'a nil)", "nil" },
        { "(member 'b '(a b c))", "(b c)" },
        { "(member 'z '(a b c))", "nil" },
        { "(member 101 '(100 101 102))", "(101 102)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_plus1)
{
    vector<TestEval> tests = {
        //{ "(1+ 1)", "2" },
        //{ "(1- 2)", "1" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_map)
{
    vector<TestEval> tests = {
        // {"(mapcar #'+ '(1 2 3) '(4 5 6))", "(5 7 9)"},
		// {"(defun id(x) x)", "id"},
		// {"(maplist #'id '(1 2 3) )", "((1 2 3) (2 3) (3))"},

		// {"(mapcar (lambda (n) (^ n n)) '(1 2 3 4) )", "(1 4 27 256)"},
		// {"(mapcar #'cadr '((a b) (d e) (g h)) )", "(b e h)"},

		// {"(mapcar #'list '(1 2 3))", "((1) (2) (3))"},

		// // Test mapcar called in function definition
		// {"(defun f (a b) (mapcar a b))", "f"},
		// {"(f #'1+ '(1 2 3))", "(2 3 4)"},

		// {"(defun g (a b) (mapcar a b))", "g"},
		// {"(g (lambda (n) (^ n n)) '(1 2 3))", "(1 4 27)"},

		// // {"(mapcar #'+ '(s s s) '(a a a))", "Error: + arguments needs to be number\nnil"},

		// {"(mapcar)", "Eval error: mapcar: invalid number of arguments"},
		// {"(mapcar #'f)", "Eval error: mapcar: invalid number of arguments"},
    };
    test_Evaluator(tests);
}
