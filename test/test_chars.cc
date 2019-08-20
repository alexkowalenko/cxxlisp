//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_chars

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "expr.hh"

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_chars)
{
    vector<TestEval> tests = {
        { "#\\a", "#\\a" },
        // { "#\\abs", "Repl Error: Not a character: abs" },
        { "#\\1", "#\\1" },
        { "#\\A", "#\\A" },
        { "#\\.", "#\\." },
        { "#\\;", "#\\;" },
        { "#\\(", "#\\(" },
        { "#\\)", "#\\)" },
        { "#\\;", "#\\;" },
        { "#\\#", "#\\#" },
        { "#\\\\", "#\\\\" },
        { "#\\ἄ", "#\\ἄ" },
        { "#\\七", "#\\七" },
        { "#\\👾", "#\\👾" },
        { "#\\space", "#\\space" },
        { "#\\newline", "#\\newline" },
        { "#\\SPACE", "#\\space" },
        { "#\\NeWlInE", "#\\newline" },

        { "'#\\a", "#\\a" },
        { "'#\\;", "#\\;" },
        { "'#\\space", "#\\space" },
        { "'#\\newline", "#\\newline" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_characterp)
{
    vector<TestEval> tests = {
        { R"((characterp #\a))", "t" },
        { R"((characterp #\;))", "t" },
        { R"((characterp #\)))", "t" },
        { R"((characterp #\七))", "t" },
        { R"((characterp #\🏀))", "t" },
        { R"((characterp "one two three"))", "nil" },
        { R"((characterp ""))", "nil" },
        { R"((characterp 0))", "nil" },
        { R"((characterp t))", "nil" },
        { R"((characterp 'a))", "nil" },
        { R"((characterp '(a b c)))", "nil" },

        { R"((characterp))", "Eval error: characterp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_eq)
{
    vector<TestEval> tests = {
        { R"((char= #\a #\A))", "nil" },
        { R"((char-equal #\a #\a))", "t" },
        { R"((char= #\a 1))", "Eval error: char= arguments needs to be a character" },
        { R"((char-equal #\space #\space))", "t" },
        { R"((char= #\a ))", "Eval error: char= expecting 2 arguments" },
        { R"((char-equal))", "Eval error: char-equal expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_neq)
{
    vector<TestEval> tests = {
        { R"((char/= #\a #\A))", "t" },
        { R"((char-not-equal #\a #\a))", "nil" },
        { R"((char/= #\a 1))", "Eval error: char/= arguments needs to be a character" },
        { R"((char-not-equal #\space #\space))", "nil" },
        { R"((char/= #\a ))", "Eval error: char/= expecting 2 arguments" },
        { R"((char-not-equal))", "Eval error: char-not-equal expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_lt)
{
    vector<TestEval> tests = {
        { R"((char< #\a #\A))", "nil" },
        { R"((char-lessp #\a #\a))", "nil" },
        { R"((char< #\a #\b))", "t" },
        { R"((char-lessp #\a 1))", "Eval error: char-lessp arguments needs to be a character" },
        { R"((char< #\space #\space))", "nil" },
        { R"((char-lessp #\a ))", "Eval error: char-lessp expecting 2 arguments" },
        { R"((char<))", "Eval error: char< expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_gt)
{
    vector<TestEval> tests = {
        { R"((char> #\a #\A))", "t" },
        { R"((char-greaterp #\a #\a))", "nil" },
        { R"((char> #\a #\b))", "nil" },
        { R"((char-greaterp #\a 1))", "Eval error: char-greaterp arguments needs to be a character" },
        { R"((char> #\space #\space))", "nil" },
        { R"((char-greaterp #\a ))", "Eval error: char-greaterp expecting 2 arguments" },
        { R"((char>))", "Eval error: char> expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_le)
{
    vector<TestEval> tests = {
        { R"((char<= #\a #\A))", "nil" },
        { R"((char-not-greaterp #\a #\a))", "t" },
        { R"((char<= #\a #\b))", "t" },
        { R"((char-not-greaterp #\a 1))", "Eval error: char-not-greaterp arguments needs to be a character" },
        { R"((char<= #\newline #\newline))", "t" },
        { R"((char-not-greaterp #\a ))", "Eval error: char-not-greaterp expecting 2 arguments" },
        { R"((char<=))", "Eval error: char<= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_ge)
{
    vector<TestEval> tests = {
        { R"((char>= #\a #\A))", "t" },
        { R"((char-not-lessp #\a #\a))", "t" },
        { R"((char>= #\a #\b))", "nil" },
        { R"((char-not-lessp #\a 1))", "Eval error: char-not-lessp arguments needs to be a character" },
        { R"((char>= #\newline #\newline))", "t" },
        { R"((char-not-lessp #\a ))", "Eval error: char-not-lessp expecting 2 arguments" },
        { R"((char>=))", "Eval error: char>= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_eq_ci)
{
    vector<TestEval> tests = {
        { R"((char-ci= #\a #\A))", "t" },
        { R"((char-ci= #\a #\a))", "t" },
        { R"((char-ci= #\a #\b))", "nil" },
        { R"((char-ci= #\a 1))", "Eval error: char-ci= arguments needs to be a character" },
        { R"((char-ci= #\newline #\newline))", "t" },
        { R"((char-ci= #\a ))", "Eval error: char-ci= expecting 2 arguments" },
        { R"((char-ci=))", "Eval error: char-ci= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_lt_ci)
{
    vector<TestEval> tests = {
        { R"((char-ci< #\a #\A))", "nil" },
        { R"((char-ci< #\a #\a))", "nil" },
        { R"((char-ci< #\a #\b))", "t" },
        { R"((char-ci< #\a 1))", "Eval error: char-ci< arguments needs to be a character" },
        { R"((char-ci< #\newline #\newline))", "nil" },
        { R"((char-ci< #\a ))", "Eval error: char-ci< expecting 2 arguments" },
        { R"((char-ci<))", "Eval error: char-ci< expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_gt_ci)
{
    vector<TestEval> tests = {
        { R"((char-ci> #\a #\A))", "nil" },
        { R"((char-ci> #\a #\a))", "nil" },
        { R"((char-ci> #\a #\b))", "nil" },
        { R"((char-ci> #\a 1))", "Eval error: char-ci> arguments needs to be a character" },
        { R"((char-ci> #\newline #\newline))", "nil" },
        { R"((char-ci> #\a ))", "Eval error: char-ci> expecting 2 arguments" },
        { R"((char-ci>))", "Eval error: char-ci> expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_le_ci)
{
    vector<TestEval> tests = {
        { R"((char-ci<= #\a #\A))", "t" },
        { R"((char-ci<= #\a #\a))", "t" },
        { R"((char-ci<= #\a #\b))", "t" },
        { R"((char-ci<= #\a 1))", "Eval error: char-ci<= arguments needs to be a character" },
        { R"((char-ci<= #\newline #\newline))", "t" },
        { R"((char-ci<= #\a ))", "Eval error: char-ci<= expecting 2 arguments" },
        { R"((char-ci<=))", "Eval error: char-ci<= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_char_ge_ci)
{
    vector<TestEval> tests = {
        { R"((char-ci>= #\a #\A))", "t" },
        { R"((char-ci>= #\a #\a))", "t" },
        { R"((char-ci>= #\a #\b))", "nil" },
        { R"((char-ci>= #\a 1))", "Eval error: char-ci>= arguments needs to be a character" },
        { R"((char-ci>= #\newline #\newline))", "t" },
        { R"((char-ci>= #\a ))", "Eval error: char-ci>= expecting 2 arguments" },
        { R"((char-ci>=))", "Eval error: char-ci>= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}
