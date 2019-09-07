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

BOOST_AUTO_TEST_CASE(test_chapter_2)
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
        { "(push new-front list-to-be-changed)", "(a b c)" },
        { "list-to-be-changed", "(a b c)" },
        { "(pop list-to-be-changed)", "a" },
        { "list-to-be-changed", "(b c)" },

        // pg.28
        { "(setf abc-list '(a b c))", "(a b c)" },
        { "(rest abc-list)", "(b c)" },
        { "(nthcdr 2 abc-list)", "(c)" },
        { "(setf abc-list '(a b c))", "(a b c)" },
        { "(nthcdr 50 abc-list)", "nil" },
        { "(setf abc-list '(a b c))", "(a b c)" },
        //{ "(butlast abc-list 2)", "(a)" },
        { "(setf f 'front b 'back abc-list '(a b c))", "(a b c)" },
        { "(cons f abc-list)", "(front a b c)" },
        { "(append abc-list (list b))", "(a b c back)" },
        { "(setf abc-list '(a b c) ab-cd-list '((a b) (c d)))", "((a b) (c d))" },
        { "(last abc-list)", "(c)" },
        { "(last ab-cd-list)", "((c d))" },
        { "(last 'abc-list)", "Eval error: length: needs sequence argument" },
        { "(setf ab-list '(a b) ab-cd-list '((a b) (c d)))", "((a b) (c d))" },
        { "(length ab-list)", "2" },
        { "(length ab-cd-list)", "2" },
        { "(length (append ab-cd-list ab-cd-list))", "4" },
        { "(reverse ab-list)", "(b a)" },
        { "(reverse ab-cd-list)", "((c d) (a b))" },
        { "(reverse (append ab-list ab-list))", "(b a b a)" },
        // pg.31
        { "(setf sarah '((height .54) (weight 4.4)))", "((height 0.54) (weight 4.4))" },
        { "(assoc 'weight sarah)", "(weight 4.4)" },
        // pg. 32
        { "(/ 1.234321 1.111)", "1.111" },
        { "(/ 27 9)", "3" },
        // { "(/ 22 7)", "22/7" }, //rationals not supported
        { "(float (/ 22.0 7))", "3.14285714286" },
        { "(round (/ 22 7))", "3" }, // this should return two values 3, 1/7
        { "(+ (round (/ 22.0 7)) (round (/ 7.0 3)))", "5" },
        // { "(round 5 2)", "2" }, // rounds towards even integer, not supported
        { "(+ 2 1.5)", "3.5" },
        { "(+ (float 2) (float 1.5))", "3.5" },
        { "(- 8)", "-8" },
        { "(- -8)", "8" },
        // { "(/ 1 2)", "" }, rationals
        // { "(/ 2)", "1/2" }, this works as a float

        // pg 34
        { "(max 2 4 3)", "4" },
        { "(min 2 4 3)", "2" },
        { "(expt 2 3)", "8" },
        { "(expt 3 2)", "9" },
        { "(expt 3.3 2.2)", "13.827086118" },
        { "(expt 2.2 3.3)", "13.4894687605" },
        { "(sqrt 9)", "3" },
        { "(expt 3 2)", "9" },
        { "(abs 5)", "5" },
        { "(abs -5)", "5" },
    };
    test_Evaluator(tests);
}
