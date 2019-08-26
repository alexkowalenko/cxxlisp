// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_functions

#include "test.hh"

#include <limits>

#include <boost/test/unit_test.hpp>

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_length)
{
    vector<TestEval> tests = {
        { "(length \"123\")", "3" },
        { "(length \"🦑🦐🦀\")", "3" },
        { "(length '(1 2 3 4))", "4" },
        // { "(length #(1 2 3.5))", "3" },

        //{ "(length #())", "0" },
        { "(length '())", "0" },
        { "(length \"\")", "0" },
        { "(length nil)", "0" },

        { "(length)", "Eval error: length expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_elt)
{
    vector<TestEval> tests = {
        { "(elt \"123\" 1)", "#\\2" },
        { "(elt '(1 2 3 4) 2)", "3" },
        //{ "(elt #(1 2 3.5) 2)", "3.5" },

        { "(elt \"🦑🦐🦀\" 1)", "#\\🦐" },

        //{ "(elt #() 0)", "Error: elt: index out of range\nnil" },
        { "(elt '() 0)", "Eval error: elt: index out of range" },
        { "(elt \"\" 0)", "Eval error: elt: index out of range" },
        { "(elt nil 0)", "Eval error: elt: index out of range" },

        { "(elt)", "Eval error: elt expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_subseq)
{
    vector<TestEval> tests = {
        { "(subseq \"123\" 1)", "\"23\"" },
        { "(subseq '(1 2 3 4) 2)", "(3 4)" },
        // {"(subseq #(1 2 3.5) 2)", "#(3.5)"},

        { "(subseq \"一二三四五六七\" 2 3)", "\"三四五\"" },

        { "(subseq \"Hello daughter\" 0 4)", "\"Hell\"" },
        { "(subseq '(0 1 2 3 4 5 6 7) 1 4)", "(1 2 3 4)" },
        { "(subseq '(0 1 2 3 4 5 6 7) 4 1)", "(4)" },
        //{"(subseq #(0 1 2 3 4 5 6 7) 2 4)", "#(2 3 4)"},
        { "(subseq '(0 1 2 3 4 5 6 7) 7)", "(7)" },
        { "(subseq '(0 1 2 3 4 5 6 7) 8)", "nil" },

        //{"(subseq #() 0)", "Error: subseq: index out of range\nnil"},
        { "(subseq '() 0)", "nil" },
        { "(subseq \"\" 0)", "\"\"" },
        { "(subseq nil 0)", "nil" },

        { "(subseq)", "Eval error: subseq expecting at least 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_copyseq)
{
    vector<TestEval> tests = {
        { "(copy-seq \"123\")", "\"123\"" },
        { "(copy-seq \"一二三四五六七\")", "\"一二三四五六七\"" },

        { "(copy-seq '(1 2 3 4))", "(1 2 3 4)" },
        //{"(copy-seq #(1 2 3.5))", "#(1 2 3.5)"},

        //{"(copy-seq #())", "#()"},
        { "(copy-seq '())", "nil" },
        { "(copy-seq \"\")", "\"\"" },
        { "(copy-seq nil)", "nil" },

        { "(copy-seq)", "Eval error: copy-seq: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_find)
{
    vector<TestEval> tests = {
        { "(find #\\l \"hello\")", "#\\l" },
        { "(find #\\A \"hello\")", "nil" },
        { "(find #\\ग \"कखगघङ\")", "#\\ग" },
        { "(find 'l \"hello\")", "nil" },

        { "(find 'a '(a b c a b c))", "a" },
        //{ "(find 'a '(a b c a b c) :start 4)", "nil" },
        { "(find 'z '(a b c a b c))", "nil" },

        //{ "(find 1 #(0 0 1))", "1" },
        // { "(find 'c #(a b c a b c) :start 2 :end 4)", "c" },

        { "(find)", "Eval error: find: invalid number of arguments" },
        { "(find \"hello\")", "Eval error: find: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_find_if)
{
    vector<TestEval> tests = {
        { "(find-if #'atom '(1 2 3 4))", "1" },
        { "(find-if-not #'atom '(1 2 3 4))", "nil" },

        { "(find-if (lambda (x) (eq x 'red)) '(blue green s red s))", "red" },

        //{ "(find-if #'zerop #(1 0 1))", "0" },
        //{ "(find-if-not #'zerop #(1 0 1))", "1" },

        // { "(find-if #'zerop #(1 0 1 0 1 0 2 3 0) :start 3 :end 6)", "0" },

        { "(find-if #'zerop)", "Eval error: find-if: invalid number of arguments" },
        { "(find-if)", "Eval error: find-if: invalid number of arguments" },
        { "(find-if #'zerop 'no)", "Eval error: length: needs sequence argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_position)
{
    vector<TestEval> tests = {
        { "(position #\\l \"hello\")", "2" },
        { "(position #\\A \"hello\")", "nil" },
        { "(position #\\ग \"कखगघङ\")", "2" },
        { "(position 'l \"hello\")", "nil" },

        { "(position 'a '(a b c a b c))", "0" },
        // { "(position 'b '(a b c a b c) :start 3)", "4" },
        { "(position 'z '(a b c a b c))", "nil" },

        //{"(position 1 #(0 0 1))", "2"},
        //{"(position 'c #(a b c a b c) :start 3)", "5"},

        { "(position)", "Eval error: position: invalid number of arguments" },
        { "(position \"hello\")", "Eval error: position: invalid number of arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_position_if)
{
    vector<TestEval> tests = {
        { "(position-if #'atom '(1 2 3 4))", "0" },
        //{"(position-if-not #'atom '(1 2 3 4))", "nil"},

        // Lambdas don't work
        //{ "(position-if (lambda (x) (eq x 'red)) '(blue green s red s))", "3" },

        //{ "(position-if #'zerop #(1 1 0))", "2" },
        //{"(position-if-not #'zerop #(0 0 0 1))", "3"}, // hangs

        //{"(position-if #'zerop #(1 0 1 0 1 0 2 3 0) :start 3 :end 6)", "3"},

        { "(position-if #'zerop)", "Eval error: position-if: invalid number of arguments" },
        { "(position-if)", "Eval error: position-if: invalid number of arguments" },
        { "(position-if #'zerop 'no)", "Eval error: length: needs sequence argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_setf)
{
    vector<TestEval> tests = {
        { "(defvar x '(1 2 3))", "x" },
        { "(setf (elt x 1) #\\b)", "(1 #\\b 3)" },
        { "x", "(1 #\\b 3)" },
        { "(setf (elt x 0) #\\a)", "(#\\a #\\b 3)" },
        { "(setf (elt x 2) #\\c)", "(#\\a #\\b #\\c)" },
        { "x", "(#\\a #\\b #\\c)" },
        { "(defvar x \"αβγ\")", "x" },
        { "(setf (elt x 1) #\\b)", "\"αbγ\"" },
        { "x", "\"αbγ\"" },
        { "(setf (elt x 0) #\\a)", "\"abγ\"" },
        { "(setf (elt x 2) #\\c)", "\"abc\"" },
        { "x", "\"abc\"" },

        { "(setf (elt x 4) #\\a )", "Eval error: setf elt: index out of bounds" },
        { "(setf (elt x ) #\\a )", "Eval error: setf elt: incorrect number of arguments" },
        { "(setf (elt y 4) #\\a)", "Eval error: setf elt: must be a reference" },
        { "(setf (elt y 4))", "Eval error: setf requires an even number of variables" },
        { "(setf (elt y))", "Eval error: setf requires an even number of variables" },
        { "(setf 1)", "Eval error: setf requires an even number of variables" },
    };
    test_Evaluator(tests);
}
