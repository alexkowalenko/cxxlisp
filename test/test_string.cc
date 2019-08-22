//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_strings

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

#include "expr.hh"

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_eval_strings)
{
    vector<TestEval> tests = {
        { R"("one two three")", R"("one two three")" },
        { R"("ἑν δύο τρεῖς")", R"("ἑν δύο τρεῖς")" },
        { R"("一二三四五六七")", R"("一二三四五六七")" },
        { R"("🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒")", R"("🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒")" },
        { R"("")", R"("")" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_stringp)
{
    vector<TestEval> tests = {
        { R"( (stringp "one two three") )", "t" },
        { R"((stringp "ἑν δύο τρεῖς"))", "t" },
        { R"((stringp ""))", "t" },

        { R"((stringp 0))", "nil" },
        { R"((stringp t))", "nil" },
        { R"((stringp 'a))", "nil" },
        { R"((stringp '(a b c)))", "nil" },
        { R"((stringp "one \"two\" three"))", "t" },
        { R"((stringp))", "Eval error: stringp expecting an argument" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_eq)
{
    vector<TestEval> tests = {
        { R"((string= "one" "two three"))",
            "nil" },
        { R"((string-equal "one" "one"))",
            "t" },
        { R"((string= "one" 1))", "Eval error: string= arguments needs to be a string" },
        { R"((string-equal "" ""))", "t" },
        { R"((string= "one"))", "Eval error: string= expecting 2 arguments" },
        { R"((string-equal))", "Eval error: string-equal expecting 2 arguments" },
    };
    test_Evaluator(tests);
}
BOOST_AUTO_TEST_CASE(test_eval_string_neq)
{
    vector<TestEval> tests = {
        { R"((string/= "one"
                     "two three"))",
            "t" },
        { R"((string-not-equal "one" "one"))",
            "nil" },
        { R"((string/= "one" 1))", "Eval error: string/= arguments needs to be a string" },
        { R"((string-not-equal "" ""))",
            "nil" },
        { R"((string/= "one"))", "Eval error: string/= expecting 2 arguments" },
        { R"((string-not-equal))", "Eval error: string-not-equal expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_lt)
{
    vector<TestEval> tests = {
        { R"((string> "one" "two three"))", "nil" },
        { R"((string-greaterp "one" "one"))", "nil" },
        { R"((string> "ZZZa" "ZZZb"))", "nil" },
        { R"((string-greaterp "one" 1))", "Eval error: string-greaterp arguments needs to be a string" },
        { R"((string> "" ""))", "nil" },
        { R"((string-greaterp "one" ))", "Eval error: string-greaterp expecting 2 arguments" },
        { R"((string>))", "Eval error: string> expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_gt)
{
    vector<TestEval> tests = {
        { R"((string> "one" "two three"))", "nil" },
        { R"((string-greaterp "one" "one"))", "nil" },
        { R"((string> "ZZZa" "ZZZb"))", "nil" },
        { R"((string-greaterp "one" 1))", "Eval error: string-greaterp arguments needs to be a string" },
        { R"((string> "" ""))", "nil" },
        { R"((string-greaterp "one" ))", "Eval error: string-greaterp expecting 2 arguments" },
        { R"((string>))", "Eval error: string> expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_le)
{
    vector<TestEval> tests = {
        { R"((string<= "one" "two three"))", "t" },
        { R"((string-not-greaterp "one" "one"))", "t" },
        { R"((string<= "one" 1))", "Eval error: string<= arguments needs to be a string" },
        { R"((string-not-greaterp "" ""))", "t" },
        { R"((string<= "one" ))", "Eval error: string<= expecting 2 arguments" },
        { R"((string-not-greaterp))", "Eval error: string-not-greaterp expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_ge)
{
    vector<TestEval> tests = {
        { R"((string>= "one" "two three"))", "nil" },
        { R"((string-not-lessp "one" "one"))", "t" },
        { R"((string>= "one" "ONE"))", "t" },
        { R"((string-not-lessp "one" 1))", "Eval error: string-not-lessp arguments needs to be a string" },
        { R"((string>= "" ""))", "t" },
        { R"((string-not-lessp "one" ))", "Eval error: string-not-lessp expecting 2 arguments" },
        { R"((string>=))", "Eval error: string>= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_eq_ci)
{
    vector<TestEval> tests = {
        { R"((string-ci= "one" "two three"))", "nil" },
        { R"((string-ci= "one" "one"))", "t" },
        { R"((string-ci= "one" "ONE"))", "t" },
        { R"((string-ci= "OnE" "one"))", "t" },
        { R"((string-ci= "one" 1))", "Eval error: string-ci= arguments needs to be a string" },
        { R"((string-ci= "" ""))", "t" },
        { R"((string-ci= "one" ))", "Eval error: string-ci= expecting 2 arguments" },
        { R"((string-ci=))", "Eval error: string-ci= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_lt_ci)
{
    vector<TestEval> tests = {
        { R"((string-ci< "one" "two three"))", "t" },
        { R"((string-ci< "one" "ONE"))", "nil" },
        { R"((string-ci< "b" "aaa"))", "nil" },
        { R"((string-ci< "one" 1))", "Eval error: string-ci< arguments needs to be a string" },
        { R"((string-ci< "" ""))", "nil" },
        { R"((string-ci< "one" ))", "Eval error: string-ci< expecting 2 arguments" },
        { R"((string-ci<))", "Eval error: string-ci< expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_gt_ci)
{
    vector<TestEval> tests = {
        { R"((string-ci> "one" "two three"))", "nil" },
        { R"((string-ci> "ONE" "one"))", "nil" },
        { R"((string-ci> "ZZZa" "ZZZb"))", "nil" },
        { R"((string-ci> "one" 1))", "Eval error: string-ci> arguments needs to be a string" },
        { R"((string-ci> "" ""))", "nil" },
        { R"((string-ci> "one" ))", "Eval error: string-ci> expecting 2 arguments" },
        { R"((string-ci>))", "Eval error: string-ci> expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_le_ci)
{
    vector<TestEval> tests = {
        { R"((string-ci<= "one" "two three"))", "t" },
        { R"((string-ci<= "one" "ONE"))", "t" },
        { R"((string-ci<= "ONE" "one"))", "t" },
        { R"((string-ci<= "one" 1))", "Eval error: string-ci<= arguments needs to be a string" },
        { R"((string-ci<= "" ""))", "t" },
        { R"((string-ci<= "one" ))", "Eval error: string-ci<= expecting 2 arguments" },
        { R"((string-ci<=))", "Eval error: string-ci<= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_ge_ci)
{
    vector<TestEval> tests = {
        { R"((string-ci>= "one" "two three"))", "nil" },
        { R"((string-ci>= "ONE" "one"))", "t" },
        { R"((string-ci>= "one" "ONE"))", "t" },
        { R"((string-ci>= "one" 1))", "Eval error: string-ci>= arguments needs to be a string" },
        { R"((string-ci>= "" ""))", "t" },
        { R"((string-ci>= "one" ))", "Eval error: string-ci>= expecting 2 arguments" },
        { R"((string-ci>=))", "Eval error: string-ci>= expecting 2 arguments" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_eval_string_string)
{
    vector<TestEval> tests = {
        { R"((string "one"))", R"("one")" },
        { R"((string 'one))", R"("one")" },
        { R"((string #\a))", R"("a")" },
        { R"((string 123))", R"("123")" },

        // { R"((string-capitalize "one"))", R"("One")" },
        // { R"((string-capitalize 'one))", R"("One")" },
        // { R"((string-capitalize #\a))", R"("A")" },

        { R"((string-upcase "one"))", R"("ONE")" },
        { R"((string-upcase 'one))", R"("ONE")" },
        { R"((string-upcase #\a))", R"("A")" },

        { R"((string-downcase "one"))", R"("one")" },
        { R"((string-downcase 'oNe))", R"("one")" },
        { R"((string-downcase #\a))", R"("a")" },

        { R"((string))", "Eval error: string expecting an argument" },
    };
    test_Evaluator(tests);
}