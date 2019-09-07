// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_winston
// Memorial to Patrick Henry Winston and his Lisp book.

#include "test.hh"

#include <limits>

#include <boost/format.hpp>
#include <boost/test/unit_test.hpp>

using namespace std;

struct TestEval;
void test_Evaluator(const vector<TestEval>& tests);

BOOST_AUTO_TEST_CASE(test_chapter_1)
{
    vector<TestEval> tests = {
        { "(+ 3.14 2.71)", "5.85" },
        { "(setf friends '(dick jane sally))", "(dick jane sally)" },
        { "friends", "(dick jane sally)" },
        { "(setf enemies '(troll grinch ghost))", "(troll grinch ghost)" },
        { "(setf enemies (remove 'ghost enemies))", "(troll grinch)" },
        { "(setf friends (cons 'ghost friends))", "(ghost dick jane sally)" },
        { "enemies", "(troll grinch)" },
        { "friends", "(ghost dick jane sally)" },
        { R"( (defun newfriend (name)
                (setf enemies (remove name enemies))
                (setf friends (cons name friends))) )",
            "newfriend" },
        { "(newfriend 'ghost)", "(ghost ghost dick jane sally)" },

        { "(* 9 3)", "27" },
        { "(/ 27 3)", "9" },
        { "(+ (* 2 2) (/ 2 2))", "5" },

        { "(first '(fast computers are nice))", "fast" },
        { "(first '(a b c))", "a" },
        { "(rest '(fast computers are nice))", "(computers are nice)" },
        { "(rest '(a b c))", "(b c)" },
        { "(rest '(c))", "nil" },
        { "(first '())", "" }, // error here
        { "(rest ())", "nil" },
        { "(first '((a b) (c d)))", "(a b)" },
        { "(first (rest '(a b c)))", "b" },
        { "(first '(rest (a b c)))", "rest" },
        { "(cadr '(a b c))", "b" },

        { "(setf ab-list '(a b))", "(a b)" },
        { "ab-list", "(a b)" },
        { "'ab-list", "ab-list" },
        { "(first ab-list)", "a" },
        { "(first 'ab-list)", "nil" },
        { "(rest ab-list)", "(b)" },
        { "(rest 'ab-list)", "nil" },
        { "(setf ab-list '(a b) xy-list '(x y))", "(x y)" },
        { "t", "t" },
        { "nil", "nil" },
        { "(setf t nil)", "Eval error: setf requires a symbol as an argument" },
        { "2", "2" },
        { "2.71", "2.71" },

        // pg. 24
        { "(cons 'a '(b c))", "(a b c)" },
        { "(setf new-front 'a old-list '(b c))", "(b c)" },
        { "(cons new-front old-list)", "(a b c)" },
        { "(first (cons new-front old-list))", "a" },
        { "(rest (cons new-front old-list))", "(b c)" },
        { "(append '(a b c) '(x y z))", "(a b c x y z)" },
        { "(setf ab-list '(a b) xy-list '(x y))", "(x y)" },
        { "(append ab-list xy-list ab-list)", "(a b x y a b)" },
        { "(append ab-list '() xy-list '())", "(a b x y)" },
        { "(append 'ab-list xy-list)", "Eval error: append: is not a list: ab-list" },
        { "(append '((a) (b)) '((c) (d)))", "((a) (b) (c) (d))" },
        { "(list 'a 'b 'c)", "(a b c)" },
        { "(setf front 'a middle 'b back 'c)", "c" },
        { "(list front middle back)", "(a b c)" },
        { "(front middle back)", "Eval error: A non function in function location a" },
        { "(setf ab-list '(a b))", "(a b)" },
        { "(list ab-list ab-list)", "((a b) (a b))" },
        { "(list 'ab-list ab-list)", "(ab-list (a b))" },
        { "(setf ab-list '(a b) cd-list '(c d))", "(c d)" },
        { "(append ab-list cd-list)", "(a b c d)" },
        { "(list ab-list cd-list)", "((a b) (c d))" },
        { "(cons ab-list cd-list)", "((a b) c d)" },
        { "(append ab-list ab-list)", "(a b a b)" },
        { "(cons ab-list ab-list)", "((a b) a b)" },
        { "(list ab-list ab-list)", "((a b) (a b))" },
        { "(append 'ab-list ab-list)", "Eval error: append: is not a list: ab-list" },
        { "(list 'ab-list ab-list)", "(ab-list (a b))" },
        { "(cons 'ab-list ab-list)", "(ab-list a b)" },

        // pg. 27
        { "(setf new-front 'a old-list '(b c))", "(b c)" },
        { "(cons new-front old-list)", "(a b c)" },
        { "new-front", "a" },
        { "old-list", "(b c)" },
        { "(setf new-front 'a list-to-be-changed '(b c))", "(b c)" },
        { "(setf list-to-be-changed (cons new-front list-to-be-changed))", "(a b c)" },
        { "new-front", "a" },
        { "list-to-be-changed", "(a b c)" },
        { "(setf new-front 'a list-to-be-changed '(b c))", "(b c)" },
        // { "(push new-front list-to-be-changed)", "(a b c)" },
        // { "list-to-be-changed", "(a b c)" },
        // { "(pop list-to-be-changed)", "a" },
        // { "list-to-be-changed", "(b c)" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },
        { "", "" },

    };
    test_Evaluator(tests);
}
