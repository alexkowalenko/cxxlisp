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