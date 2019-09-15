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
BOOST_AUTO_TEST_CASE(test_eval_list_length)
{
    vector<TestEval> tests = {
        { "(list-length '(a b c d))", "4" },
        { "(list-length '())", "0" },
        { "(list-length nil)", "0" },
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
        { "(when t 1 2 3 4)", "4" },
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
        { "(unless nil 1 2 3 4)", "4" },
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

BOOST_AUTO_TEST_CASE(test_eval_assoc)
{
    vector<TestEval> tests = {
        { "(pair '(1 2) '(a b))", "((1 . a) (2 . b))" },
        { "(setq v '((x . 100) (y . 200) (z . 50)))",
            "((x . 100) (y . 200) (z . 50))" },
        { "(assoc 'y v)", "(y . 200)" },
        { "(assoc 'a v)", "nil" },

        { "(pair nil '(a b))", "nil" },
        { "(pair '(a b) nil)", "nil" },
        { "(pair nil nil)", "nil" },

        { "(assoc 'z nil)", "nil" },
        { "(assoc nil v)", "nil" },
        { "(assoc nil nil)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_remove)
{
    vector<TestEval> tests = {
        { "(remove 's '(a v s))", "(a v)" },
        { "(remove 'a '(a v s))", "(v s)" },
        { "(remove 'b '(a v s))", "(a v s)" },
        { "(remove nil '(a v s))", "(a v s)" },
        { "(remove 's nil)", "nil" },
        { "(remove 's nil)", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_plus1)
{
    vector<TestEval> tests = {
        { "(1+ 1)", "2" },
        { "(1- 2)", "1" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_nth)
{
    vector<TestEval> tests = {
        { "(nth)", "Eval error: nth: invalid number of arguments" },
        { "(nth '())", "Eval error: nth: invalid number of arguments" },
        { "(nth 'a '(a b))", "Eval error: = arguments needs to be a number" },

        { "(nth 1 'a)", "nil" },
        { "(nth 0 '(a))", "a" },
        { "(nth 1 '(a b))", "b" },
        { "(nth 2 '(a b c))", "c" },
        { "(nth 2 '(a b c d))", "c" },
        { "(nth 2 '(a b (c1 2) d e))", "(c1 2)" },
        { "(nth 6 '(a b (c1 2) d e))", "nil" },
        { "(nth 50 '(a b c))", "nil" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_nth_tail)
{
    vector<TestEval> tests = {
        { "(nthcdr)", "Eval error: nthcdr: invalid number of arguments" },
        { "(nthcdr '())", "Eval error: nthcdr: invalid number of arguments" },
        { "(nthcdr 'a '(a b))", "Eval error: = arguments needs to be a number" },

        { "(nthcdr 1 'a)", "nil" },
        { "(nthcdr 0 '(a))", "(a)" },
        { "(nthcdr 1 '(a b))", "(b)" },
        { "(nthcdr 2 '(a b c))", "(c)" },
        { "(nthcdr 2 '(a b c d))", "(c d)" },
        { "(nthcdr 2 '(a b (c1 2) d e))", "((c1 2) d e)" },
        { "(nthcdr 6 '(amm b (c1 2) d e))", "nil" }
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_fold)
{
    vector<TestEval> tests = {
        { "(fold #'+ 0 '(1 1))", "2" },
        { "(fold #'+ 0 '(1 1 1))", "3" },
        { "(fold #'+ 0 '(1 2 3))", "6" },
        { "(fold #'+ 0 '())", "0" },

        { "(fold 'a 0 '( 1))", "Eval error: funcall: Not function ref or lambda expression: a" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_last)
{
    vector<TestEval> tests = {
        { "(last '(1 2 3 4))", "(4)" },
        { "(last nil)", "nil" },

        { "(last 1)", "Eval error: length: needs sequence argument" },

    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_butlast)
{
    vector<TestEval> tests = {
        { "(butlast '(1 2 3 4))", "(1 2 3)" },
        { "(butlast '(1))", "nil" },
        { "(butlast nil)", "nil" },

        { "(butlast)", "Eval error: butlast: invalid number of arguments" },

    };
    test_Evaluator(tests);
}
