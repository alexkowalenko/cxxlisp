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

BOOST_AUTO_TEST_CASE(test_chapter_3)
{
    vector<TestEval> tests = {
        { "(setf meals '(breakfast lunch tea dinner))", "(breakfast lunch tea dinner)" },
        { "(cons (first meals) (last meals))", "(breakfast dinner)" },
        { "(setf route2 '(boston cambridge lincoln concord))", "(boston cambridge lincoln concord)" },
        { "(cons (first route2) (last route2))", "(boston concord)" },
        { R"( (defun both-ends (whole-list)
                (cons (first whole-list)
                    (last whole-list))) )",
            "both-ends" },
        { "(both-ends meals)", "(breakfast dinner)" },
        { "(both-ends route2)", "(boston concord)" },
        { "(setf whole-list '(monday tuesday wednesday thursday friday saturday sunday))",
            "(monday tuesday wednesday thursday friday saturday sunday)" },
        { "(both-ends '(boston cambridge lincoln concord))", "(boston concord)" },
        { "whole-list", "(monday tuesday wednesday thursday friday saturday sunday)" },
        { R"( (defun both-end-with-special-variable ()
                (setf whole-list 
                        (cons (first whole-list) (last whole-list)))) )",
            "both-end-with-special-variable" },
        { "(setf whole-list '(monday tuesday wednesday thursday friday saturday sunday))",
            "(monday tuesday wednesday thursday friday saturday sunday)" },
        { "(both-end-with-special-variable)", "(monday sunday)" },
        { "whole-list", "(monday sunday)" },
        { R"( (defun both-ends-with-two-parameters (l m)
                (cons (first l) (last m))) )",
            "both-ends-with-two-parameters" },
        { "(both-ends-with-two-parameters '(breakfast lunch) '(tea dinner))", "(breakfast dinner)" },
        { R"( (defun both-ends-with-side-effect (whole-list)
                    (setf last-list-processed whole-list)
                    (cons (first whole-list)
                           (last whole-list))) )",
            "both-ends-with-side-effect" },

        // pg. 45
        //{ "(setf whole-list '(breakfast lunch tea dinner))", "(breakfast lunch tea dinner)" },
        { R"( (let ((element (first whole-list))
                    (trailer (last whole-list)))
                    (cons element trailer)) )",
            "(monday sunday)" },
        { R"( (defun both-ends-with-let (whole-let)
                (let ((element (first whole-list))
                        (trailer (last whole-list)))
                    (cons element trailer))) )",
            "both-ends-with-let" },
        { "(both-ends-with-let whole-list)", "(monday sunday)" },

        { "(setf x 'outside)", "outside" },
        { R"( (let ((x 'inside)
                 (y x))
                 (list x y)) )",
            "(inside outside)" },
        { "(setf x 'outside)", "outside" },
        { R"( (let* ((x 'inside)
                 (y x))
                 (list x y)) )",
            "(inside inside)" },

        { "(setf x 'outside)", "outside" },
        { R"( (let ((x 'inside))
                 (let ((y x))
                     (list x y))) )",
            "(inside inside)" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_chapter_4)
{
    vector<TestEval> tests = {
        { "t", "t" },
        { "nil", "nil" },
        { "(equal (+ 2 2) 4)", "t" },
        { "(equal (+ 2 2) 3)", "nil" },
        { "(equal '(this is a list) (setf l '(this is a list)))", "t" },
        { "(equal '(this is a list) l)", "t" },
        { "(equal '(this is a list) (setf reverse-of-l '(list a is this)))", "nil" },
        { "(equal l (reverse reverse-of-l))", "t" },
        { "(eql 4 4.0)", "nil" },
        { "(eql 4 4)", "t" },
        { "(= 4 4.0)", "t" },
        { "(setf sentence '(tell me more about your mother please))",
            "(tell me more about your mother please)" },
        { "(member 'mother sentence)", "(mother please)" },
        { "(setf pairs '((father son) (mother daughter)))", "((father son) (mother daughter))" },
        { "(member 'mother pairs)", "nil" },
        { "(setf pairs '((maple shade) (apple fruit)))", "((maple shade) (apple fruit))" },
        { "(member '(maple shade) pairs)", "nil" },
        //{ "(member '(maple shade) pairs :test #'equal)", "" },
        { ":test", ":test" },
        { "(setf predicate #'equal)", "#'equal" },
        //{ "(member '(maple shade) pairs :test predicate)", "" },
        { "(setf pairs '((maple shade) (apple fruit)))", "((maple shade) (apple fruit))" },
        //{ "(member '(maple shade) pairs :test-not predicate)", "" },
        //{ "(member '(maple shade) ((maple shade) (maple shade)) :test-not predicate)", "" },
        { "(atom 'pi)", "t" },
        { "(atom pi)", "t" },
        { "(numberp pi)", "t" },
        { "(symbolp 'pi)", "t" },
        { "(listp 'pi)", "nil" },
        { "(listp '(this is a list pi in it))", "t" },
        { "(eq nil '())", "t" },
        { "(eql nil '())", "t" },
        { "(equal nil '())", "t" },
        { "nil", "nil" },
        { "()", "nil" },
        { "(atom nil)", "t" },
        { "(atom ())", "t" },
        { "(symbolp nil)", "t" },
        { "(symbolp ())", "t" },
        { "(listp nil)", "t" },
        { "(listp ())", "t" },
        { "(null '(this is not empty))", "nil" },
        { "(endp '(this is not empty))", "nil" },
        { "(null ())", "t" },
        { "(endp ())", "t" },
        { "(null 'this-is-a-symbol)", "nil" },
        { "(endp 'this-is-a-symbol)", "Eval error: end: must be a list this-is-a-symbol" },

        // pg. 57
        { "(setf zero 0 one 1 two 2 three 3 four 4)", "4" },
        { "(setf digits (list zero one two three four))", "(0 1 2 3 4)" },
        { "(numberp 4)", "t" },
        { "(numberp four)", "t" },
        { "(numberp 'four)", "nil" },
        { "(numberp digits)", "nil" },
        { "(numberp 'digits)", "nil" },
        { "(zerop zero)", "t" },
        { "(zerop 'zero)", "Eval error: zerop argument needs to be a number" },
        { "(zerop four)", "nil" },
        { "(plusp one)", "t" },
        { "(plusp (- one))", "nil" },
        { "(plusp zero)", "nil" },
        { "(evenp (* 9 7 5 3 1))", "nil" },
        { "(evenp (* 10 8 6 4 2))", "t" },
        { "(> four two)", "t" },
        { "(> two four)", "nil" },
        //{ "(> three two one)", "t" },
        //{ "(> three one two)", "nil" },

        // pg. 60
        { "(setf pets '(dog cat))", "(dog cat)" },
        { "(and (member 'dog pets) (member 'tiger pets))", "nil" },
        { "(or (member 'dingo pets) (member 'tiger pets))", "nil" },
        { "(and (member 'dog pets) (member 'cat pets))", "(cat)" },
        { "(or (member 'dog pets) (member 'cat pets))", "(dog cat)" },
        { "(not nil)", "t" },
        { "(not t)", "nil" },
        { "(not 'dog)", "nil" },
        { "(setf pets '(dog cat))", "(dog cat)" },
        { "(member 'dog pets)", "(dog cat)" },
        { "(not (member 'dog pets))", "nil" },
        { "(member 'dingo pets)", "nil" },
        { "(not (member 'dingo pets))", "t" },
        { "(and (member 'dog pets) (member 'tiger pets))", "nil" },
        { "(and (member 'dog pets) (not (member 'tiger pets)))", "t" },

        // pg.62
        { "(setf day-or-date 'monday)", "monday" },
        { "(if (symbolp day-or-date) 'day 'date)", "day" },
        { "(setf day-or-date 9)", "9" },
        { "(if (symbolp day-or-date) 'day 'date)", "date" },
        { "(setf high 98 temperature 102)", "102" },
        // { R"((when (> temperature high) when macro should accept any number of lines
        //         (setf high temperature)
        //         'new-record))",
        // "new-record" },
        //{ "high", "102" },
        { "(setf thing 'sphere r 1)", "1" },
        { R"((cond ((eq thing 'circle) (* pi r r))
                    ((eq thing 'sphere) (* 4 pi r r))))",
            "12.5663706144" },
        { "(setf thing 'sphere r 1)", "1" },
        { R"((cond ((eq thing 'circle) (* pi r r))
                    (t (* 4 pi r r))))",
            "12.5663706144" },
        { "(setf thing 'sphere r 1)", "1" },
        { R"((cond ((eq thing 'circle) (* pi r r))
                    ((* 4 pi r r))))",
            "12.5663706144" },
        { "(setf p .6)", "0.6" },
        { R"((cond ((> p .75) 'very-likely)
                    ((> p .5) 'likely)
                    ((> p .25) 'unlikely)
                    (t 'very-unlikely)))",
            "likely" },
        { "(setf breakfast '(egg bacon toast tea))", "(egg bacon toast tea)" },
        { R"((cond ((> (length breakfast) 10) 'gutton)
                ((not (endp breakfast)) 'normal)
                (t 'anorexic)))",
            "normal" },
        { R"((cond ((> (length breakfast) 10) 'gutton)
                (breakfast 'normal)
                (t 'anorexic)))",
            "normal" },
        { "(setf thing 'point r 1)", "1" },
        // { R"((case thing
        //         (circle (* pi r r))
        //         (sphere (* 4 pi r r))))",
        //     "nil" },
        { "(setf thing 'point r 1)", "1" },
        // { R"((case thing
        //         (circle (* pi r r))
        //         (sphere (* 4 pi r r))
        //         (otherwise 0)))",
        //     "nil" },
        { "(setf thing 'ball r 1)", "1" },
        // { R"((case thing
        //         (circle (* pi r r))
        //         (sphere (* 4 pi r r))
        //         (otherwise 0)))",
        //     "nil" },

        // pg. 66
        { R"((defun express-probability (p)
            (cond ((> p .75) 'very-likely)
                    ((> p .5) 'likely)
                    ((> p .25) 'unlikely)
                    (t 'very-unlikely))))",
            "express-probability" },
        { "(express-probability .8)", "very-likely" },
        { "(express-probability .4)", "unlikely" },
        { "(express-probability .2)", "very-unlikely" },
        // { R"((defun both-ends (whole-list)
        //     (case (length whole-list)
        //         (0 nil)
        //         (1 (cons (first whole-list)
        //             whole-list))
        //         (2 whole-list)
        //         (t (cons (first whole-list)
        //                 (last whole-list))))))",
        //     "both-ends" },
    };
    test_Evaluator(tests);
}

BOOST_AUTO_TEST_CASE(test_chapter_A)
{
    vector<TestEval> tests = {
        // { "()", "" },
    };
    test_Evaluator(tests);
}