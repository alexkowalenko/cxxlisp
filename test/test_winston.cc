// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

// Memorial to Patrick Henry Winston and his Lisp book.

#include "test.hh"

#include <gtest/gtest.h>

TEST(chapter, 2) {
    std::vector<TestEval> tests = {
        {"(+ 3.14 2.71)", "5.85"},
        {"(setf friends '(dick jane sally))", "(dick jane sally)"},
        {"friends", "(dick jane sally)"},
        {"(setf enemies '(troll grinch ghost))", "(troll grinch ghost)"},
        {"(setf enemies (remove 'ghost enemies))", "(troll grinch)"},
        {"(setf friends (cons 'ghost friends))", "(ghost dick jane sally)"},
        {"enemies", "(troll grinch)"},
        {"friends", "(ghost dick jane sally)"},
        {R"( (defun newfriend (name)
                (setf enemies (remove name enemies))
                (setf friends (cons name friends))) )",
         "newfriend"},
        {"(newfriend 'ghost)", "(ghost ghost dick jane sally)"},

        {"(* 9 3)", "27"},
        {"(/ 27 3)", "9"},
        {"(+ (* 2 2) (/ 2 2))", "5"},

        {"(first '(fast computers are nice))", "fast"},
        {"(first '(a b c))", "a"},
        {"(rest '(fast computers are nice))", "(computers are nice)"},
        {"(rest '(a b c))", "(b c)"},
        {"(rest '(c))", "nil"},
        {"(first '())", ""}, // error here
        {"(rest ())", "nil"},
        {"(first '((a b) (c d)))", "(a b)"},
        {"(first (rest '(a b c)))", "b"},
        {"(first '(rest (a b c)))", "rest"},
        {"(cadr '(a b c))", "b"},

        {"(setf ab-list '(a b))", "(a b)"},
        {"ab-list", "(a b)"},
        {"'ab-list", "ab-list"},
        {"(first ab-list)", "a"},
        {"(first 'ab-list)", "nil"},
        {"(rest ab-list)", "(b)"},
        {"(rest 'ab-list)", "nil"},
        {"(setf ab-list '(a b) xy-list '(x y))", "(x y)"},
        {"t", "t"},
        {"nil", "nil"},
        {"(setf t nil)", "Eval error: setf requires a symbol as an argument"},
        {"2", "2"},
        {"2.71", "2.71"},

        // pg. 24
        {"(cons 'a '(b c))", "(a b c)"},
        {"(setf new-front 'a old-list '(b c))", "(b c)"},
        {"(cons new-front old-list)", "(a b c)"},
        {"(first (cons new-front old-list))", "a"},
        {"(rest (cons new-front old-list))", "(b c)"},
        {"(append '(a b c) '(x y z))", "(a b c x y z)"},
        {"(setf ab-list '(a b) xy-list '(x y))", "(x y)"},
        {"(append ab-list xy-list ab-list)", "(a b x y a b)"},
        {"(append ab-list '() xy-list '())", "(a b x y)"},
        {"(append 'ab-list xy-list)", "Eval error: append: is not a list: ab-list"},
        {"(append '((a) (b)) '((c) (d)))", "((a) (b) (c) (d))"},
        {"(list 'a 'b 'c)", "(a b c)"},
        {"(setf front 'a middle 'b back 'c)", "c"},
        {"(list front middle back)", "(a b c)"},
        {"(front middle back)", "Eval error: A non function in function location a"},
        {"(setf ab-list '(a b))", "(a b)"},
        {"(list ab-list ab-list)", "((a b) (a b))"},
        {"(list 'ab-list ab-list)", "(ab-list (a b))"},
        {"(setf ab-list '(a b) cd-list '(c d))", "(c d)"},
        {"(append ab-list cd-list)", "(a b c d)"},
        {"(list ab-list cd-list)", "((a b) (c d))"},
        {"(cons ab-list cd-list)", "((a b) c d)"},
        {"(append ab-list ab-list)", "(a b a b)"},
        {"(cons ab-list ab-list)", "((a b) a b)"},
        {"(list ab-list ab-list)", "((a b) (a b))"},
        {"(append 'ab-list ab-list)", "Eval error: append: is not a list: ab-list"},
        {"(list 'ab-list ab-list)", "(ab-list (a b))"},
        {"(cons 'ab-list ab-list)", "(ab-list a b)"},

        // pg. 27
        {"(setf new-front 'a old-list '(b c))", "(b c)"},
        {"(cons new-front old-list)", "(a b c)"},
        {"new-front", "a"},
        {"old-list", "(b c)"},
        {"(setf new-front 'a list-to-be-changed '(b c))", "(b c)"},
        {"(setf list-to-be-changed (cons new-front list-to-be-changed))", "(a b c)"},
        {"new-front", "a"},
        {"list-to-be-changed", "(a b c)"},
        {"(setf new-front 'a list-to-be-changed '(b c))", "(b c)"},
        {"(push new-front list-to-be-changed)", "(a b c)"},
        {"list-to-be-changed", "(a b c)"},
        {"(pop list-to-be-changed)", "a"},
        {"list-to-be-changed", "(b c)"},

        // pg.28
        {"(setf abc-list '(a b c))", "(a b c)"},
        {"(rest abc-list)", "(b c)"},
        {"(nthcdr 2 abc-list)", "(c)"},
        {"(setf abc-list '(a b c))", "(a b c)"},
        {"(nthcdr 50 abc-list)", "nil"},
        {"(setf abc-list '(a b c))", "(a b c)"},
        // { "(butlast abc-list 2)", "(a)" },
        {"(setf f 'front b 'back abc-list '(a b c))", "(a b c)"},
        {"(cons f abc-list)", "(front a b c)"},
        {"(append abc-list (list b))", "(a b c back)"},
        {"(setf abc-list '(a b c) ab-cd-list '((a b) (c d)))", "((a b) (c d))"},
        {"(last abc-list)", "(c)"},
        {"(last ab-cd-list)", "((c d))"},
        {"(last 'abc-list)", "Eval error: length: needs sequence argument"},
        {"(setf ab-list '(a b) ab-cd-list '((a b) (c d)))", "((a b) (c d))"},
        {"(length ab-list)", "2"},
        {"(length ab-cd-list)", "2"},
        {"(length (append ab-cd-list ab-cd-list))", "4"},
        {"(reverse ab-list)", "(b a)"},
        {"(reverse ab-cd-list)", "((c d) (a b))"},
        {"(reverse (append ab-list ab-list))", "(b a b a)"},
        // pg.31
        {"(setf sarah '((height .54) (weight 4.4)))", "((height 0.54) (weight 4.4))"},
        {"(assoc 'weight sarah)", "(weight 4.4)"},
        // pg. 32
        {"(/ 1.234321 1.111)", "1.111"},
        {"(/ 27 9)", "3"},
        // { "(/ 22 7)", "22/7" }, //rationals not supported
        {"(float (/ 22.0 7))", "3.14285714286"},
        {"(round (/ 22 7))", "3"}, // this should return two values 3, 1/7
        {"(+ (round (/ 22.0 7)) (round (/ 7.0 3)))", "5"},
        // { "(round 5 2)", "2" }, // rounds towards even integer, not supported
        {"(+ 2 1.5)", "3.5"},
        {"(+ (float 2) (float 1.5))", "3.5"},
        {"(- 8)", "-8"},
        {"(- -8)", "8"},
        // { "(/ 1 2)", "" }, rationals
        // { "(/ 2)", "1/2" }, this works as a float

        // pg 34
        {"(max 2 4 3)", "4"},
        {"(min 2 4 3)", "2"},
        {"(expt 2 3)", "8"},
        {"(expt 3 2)", "9"},
        {"(expt 3.3 2.2)", "13.827086118"},
        {"(expt 2.2 3.3)", "13.4894687605"},
        {"(sqrt 9)", "3"},
        {"(expt 3 2)", "9"},
        {"(abs 5)", "5"},
        {"(abs -5)", "5"},
    };
    test_Evaluator(tests);
}

TEST(chapter, 3) {
    std::vector<TestEval> tests = {
        {"(setf meals '(breakfast lunch tea dinner))", "(breakfast lunch tea dinner)"},
        {"(cons (first meals) (last meals))", "(breakfast dinner)"},
        {"(setf route2 '(boston cambridge lincoln concord))",
         "(boston cambridge lincoln concord)"},
        {"(cons (first route2) (last route2))", "(boston concord)"},
        {R"( (defun both-ends (whole-list)
                (cons (first whole-list)
                    (last whole-list))) )",
         "both-ends"},
        {"(both-ends meals)", "(breakfast dinner)"},
        {"(both-ends route2)", "(boston concord)"},
        {"(setf whole-list '(monday tuesday wednesday thursday friday saturday sunday))",
         "(monday tuesday wednesday thursday friday saturday sunday)"},
        {"(both-ends '(boston cambridge lincoln concord))", "(boston concord)"},
        {"whole-list", "(monday tuesday wednesday thursday friday saturday sunday)"},
        {R"( (defun both-end-with-special-variable ()
                (setf whole-list 
                        (cons (first whole-list) (last whole-list)))) )",
         "both-end-with-special-variable"},
        {"(setf whole-list '(monday tuesday wednesday thursday friday saturday sunday))",
         "(monday tuesday wednesday thursday friday saturday sunday)"},
        {"(both-end-with-special-variable)", "(monday sunday)"},
        {"whole-list", "(monday sunday)"},
        {R"( (defun both-ends-with-two-parameters (l m)
                (cons (first l) (last m))) )",
         "both-ends-with-two-parameters"},
        {"(both-ends-with-two-parameters '(breakfast lunch) '(tea dinner))", "(breakfast dinner)"},
        {R"( (defun both-ends-with-side-effect (whole-list)
                    (setf last-list-processed whole-list)
                    (cons (first whole-list)
                           (last whole-list))) )",
         "both-ends-with-side-effect"},

        // pg. 45
        //{ "(setf whole-list '(breakfast lunch tea dinner))", "(breakfast lunch tea dinner)" },
        {R"( (let ((element (first whole-list))
                    (trailer (last whole-list)))
                    (cons element trailer)) )",
         "(monday sunday)"},
        {R"( (defun both-ends-with-let (whole-let)
                (let ((element (first whole-list))
                        (trailer (last whole-list)))
                    (cons element trailer))) )",
         "both-ends-with-let"},
        {"(both-ends-with-let whole-list)", "(monday sunday)"},

        {"(setf x 'outside)", "outside"},
        {R"( (let ((x 'inside)
                 (y x))
                 (list x y)) )",
         "(inside outside)"},
        {"(setf x 'outside)", "outside"},
        {R"( (let* ((x 'inside)
                 (y x))
                 (list x y)) )",
         "(inside inside)"},

        {"(setf x 'outside)", "outside"},
        {R"( (let ((x 'inside))
                 (let ((y x))
                     (list x y))) )",
         "(inside inside)"},
    };
    test_Evaluator(tests);
}

TEST(chapter, 4) {
    std::vector<TestEval> tests = {
        {"t", "t"},
        {"nil", "nil"},
        {"(equal (+ 2 2) 4)", "t"},
        {"(equal (+ 2 2) 3)", "nil"},
        {"(equal '(this is a list) (setf l '(this is a list)))", "t"},
        {"(equal '(this is a list) l)", "t"},
        {"(equal '(this is a list) (setf reverse-of-l '(list a is this)))", "nil"},
        {"(equal l (reverse reverse-of-l))", "t"},
        {"(eql 4 4.0)", "nil"},
        {"(eql 4 4)", "t"},
        {"(= 4 4.0)", "t"},
        {"(setf sentence '(tell me more about your mother please))",
         "(tell me more about your mother please)"},
        {"(member 'mother sentence)", "(mother please)"},
        {"(setf pairs '((father son) (mother daughter)))", "((father son) (mother daughter))"},
        {"(member 'mother pairs)", "nil"},
        {"(setf pairs '((maple shade) (apple fruit)))", "((maple shade) (apple fruit))"},
        {"(member '(maple shade) pairs)", "nil"},
        //{ "(member '(maple shade) pairs :test #'equal)", "" },
        {":test", ":test"},
        {"(setf predicate #'equal)", "#'equal"},
        //{ "(member '(maple shade) pairs :test predicate)", "" },
        {"(setf pairs '((maple shade) (apple fruit)))", "((maple shade) (apple fruit))"},
        //{ "(member '(maple shade) pairs :test-not predicate)", "" },
        //{ "(member '(maple shade) ((maple shade) (maple shade)) :test-not predicate)", "" },
        {"(atom 'pi)", "t"},
        {"(atom pi)", "t"},
        {"(numberp pi)", "t"},
        {"(symbolp 'pi)", "t"},
        {"(listp 'pi)", "nil"},
        {"(listp '(this is a list pi in it))", "t"},
        {"(eq nil '())", "t"},
        {"(eql nil '())", "t"},
        {"(equal nil '())", "t"},
        {"nil", "nil"},
        {"()", "nil"},
        {"(atom nil)", "t"},
        {"(atom ())", "t"},
        {"(symbolp nil)", "t"},
        {"(symbolp ())", "t"},
        {"(listp nil)", "t"},
        {"(listp ())", "t"},
        {"(null '(this is not empty))", "nil"},
        {"(endp '(this is not empty))", "nil"},
        {"(null ())", "t"},
        {"(endp ())", "t"},
        {"(null 'this-is-a-symbol)", "nil"},
        {"(endp 'this-is-a-symbol)", "Eval error: end: must be a list this-is-a-symbol"},

        // pg. 57
        {"(setf zero 0 one 1 two 2 three 3 four 4)", "4"},
        {"(setf digits (list zero one two three four))", "(0 1 2 3 4)"},
        {"(numberp 4)", "t"},
        {"(numberp four)", "t"},
        {"(numberp 'four)", "nil"},
        {"(numberp digits)", "nil"},
        {"(numberp 'digits)", "nil"},
        {"(zerop zero)", "t"},
        {"(zerop 'zero)", "Eval error: zerop argument needs to be a number"},
        {"(zerop four)", "nil"},
        {"(plusp one)", "t"},
        {"(plusp (- one))", "nil"},
        {"(plusp zero)", "nil"},
        {"(evenp (* 9 7 5 3 1))", "nil"},
        {"(evenp (* 10 8 6 4 2))", "t"},
        {"(> four two)", "t"},
        {"(> two four)", "nil"},
        //{ "(> three two one)", "t" },
        //{ "(> three one two)", "nil" },

        // pg. 60
        {"(setf pets '(dog cat))", "(dog cat)"},
        {"(and (member 'dog pets) (member 'tiger pets))", "nil"},
        {"(or (member 'dingo pets) (member 'tiger pets))", "nil"},
        {"(and (member 'dog pets) (member 'cat pets))", "(cat)"},
        {"(or (member 'dog pets) (member 'cat pets))", "(dog cat)"},
        {"(not nil)", "t"},
        {"(not t)", "nil"},
        {"(not 'dog)", "nil"},
        {"(setf pets '(dog cat))", "(dog cat)"},
        {"(member 'dog pets)", "(dog cat)"},
        {"(not (member 'dog pets))", "nil"},
        {"(member 'dingo pets)", "nil"},
        {"(not (member 'dingo pets))", "t"},
        {"(and (member 'dog pets) (member 'tiger pets))", "nil"},
        {"(and (member 'dog pets) (not (member 'tiger pets)))", "t"},

        // pg.62
        {"(setf day-or-date 'monday)", "monday"},
        {"(if (symbolp day-or-date) 'day 'date)", "day"},
        {"(setf day-or-date 9)", "9"},
        {"(if (symbolp day-or-date) 'day 'date)", "date"},
        {"(setf high 98 temperature 102)", "102"},
        {R"((when (> temperature high) 
                 (setf high temperature)
                 'new-record))",
         "new-record"},
        {"high", "102"},
        {"(setf thing 'sphere r 1)", "1"},
        {R"((cond ((eq thing 'circle) (* pi r r))
                    ((eq thing 'sphere) (* 4 pi r r))))",
         "12.5663706144"},
        {"(setf thing 'sphere r 1)", "1"},
        {R"((cond ((eq thing 'circle) (* pi r r))
                    (t (* 4 pi r r))))",
         "12.5663706144"},
        {"(setf thing 'sphere r 1)", "1"},
        {R"((cond ((eq thing 'circle) (* pi r r))
                    ((* 4 pi r r))))",
         "12.5663706144"},
        {"(setf p .6)", "0.6"},
        {R"((cond ((> p .75) 'very-likely)
                    ((> p .5) 'likely)
                    ((> p .25) 'unlikely)
                    (t 'very-unlikely)))",
         "likely"},
        {"(setf breakfast '(egg bacon toast tea))", "(egg bacon toast tea)"},
        {R"((cond ((> (length breakfast) 10) 'gutton)
                ((not (endp breakfast)) 'normal)
                (t 'anorexic)))",
         "normal"},
        {R"((cond ((> (length breakfast) 10) 'gutton)
                (breakfast 'normal)
                (t 'anorexic)))",
         "normal"},
        {"(setf thing 'point r 1)", "1"},
        {R"((case thing
                 (circle (* pi r r))
                 (sphere (* 4 pi r r))))",
         "nil"},
        {"(setf thing 'point r 1)", "1"},
        {R"((case thing
                 (circle (* pi r r))
                 (sphere (* 4 pi r r))
                 (otherwise 0)))",
         "0"},
        {"(setf thing 'ball r 1)", "1"},
        {R"((case thing
                 ((circle wheel) (* pi r r))
                 ((sphere ball) (* 4 pi r r))
                 (otherwise 0)))",
         "12.5663706144"},

        // pg. 66
        {R"((defun express-probability (p)
            (cond ((> p .75) 'very-likely)
                    ((> p .5) 'likely)
                    ((> p .25) 'unlikely)
                    (t 'very-unlikely))))",
         "express-probability"},
        {"(express-probability .8)", "very-likely"},
        {"(express-probability .4)", "unlikely"},
        {"(express-probability .2)", "very-unlikely"},
        {R"((defun both-ends (whole-list)
             (case (length whole-list)
                 (0 nil)
                 (1 (cons (first whole-list)
                     whole-list))
                 (2 whole-list)
                 (otherwise (cons (first whole-list)
                         (last whole-list))))))",
         "both-ends"},
        {"(both-ends nil)", "nil"},
        {"(both-ends '(a))", "(a a)"},
        {"(both-ends '(a b))", "(a b)"},
        {"(both-ends '(a b c))", "(a c)"},
        {"(both-ends '(a b c d))", "(a d)"},
    };
    test_Evaluator(tests);
}

TEST(chapter, 5) {
    std::vector<TestEval> tests = {
        {R"((defun both-ends (whole-list)
            (combine-elements
                (extract-first-element whole-list)
                (extract-last-element whole-list)
                )))",
         "both-ends"},
        {R"((defun combine-elements (e1 e2)
            (list e1 e2)))",
         "combine-elements"},
        {R"((defun extract-first-element (l)
            (first l)))",
         "extract-first-element"},
        {R"((defun extract-last-element (l)
            (first (last l))))",
         "extract-last-element"},
        {"(both-ends '(1 2 3 4 5))", "(1 5)"},
        {R"((defun recursive-expt (m n)
                (if (zerop n) 
                    1
                    (* m (recursive-expt m (- n 1))))) )",
         "recursive-expt"},
        {"(recursive-expt 2 3)", "8"},
        {"(recursive-expt 2 0)", "1"},
        {"(recursive-expt 3 3)", "27"},
        {R"((defun fibonacci (n)
                (if (or (= n 0) (= n 1))
                    1
                    (+ (fibonacci (- n 1))
                        (fibonacci (- n 2))))) )",
         "fibonacci"},
        {"(fibonacci 4)", "5"},
        {"(fibonacci 10)", "89"},
        {R"((defun count-elements (l)
                (if (endp l)
                    0
                    (+ 1 (count-elements (rest l))))) )",
         "count-elements"},
        {"(count-elements '(fast computers are nice))", "4"},
        {R"((defun count-elements-cleverly (l)
                (count-elements-aux l 0)) )",
         "count-elements-cleverly"},
        {R"((defun count-elements-aux (l result)
                (if (endp l)
                    result
                    (count-elements-aux (rest l) (+ 1 result)))) )",
         "count-elements-aux"},
        {"(count-elements-cleverly '(fast computers are nice))", "4"},
        {R"((defun count-elements-mutually (l)
                (count-elements-indirectly l 0))

            (defun count-elements-indirectly (l result)
                (if (endp l)
                    result
                    (count-elements-buffer (rest l) (+ 1 result))))

            (defun count-elements-buffer (l result)
                (count-elements-indirectly l result)) )",
         "count-elements-mutually\ncount-elements-indirectly\ncount-elements-buffer"},
        {"(count-elements-mutually '(fast computers are nice))", "4"},
        // pg. 79
        {R"( (defun count-atoms (l)
                (cond ((null l) 0)
                    ((atom l) 1)
                    (t (+ (count-atoms (first l))
                            (count-atoms (rest l)))))) )",
         "count-atoms"},
        {"(count-atoms '(fsqrt (expr x 2) (expr y 2)))", "7"},
        // pg. 83
        {R"( (defun root (x &optional n)
                (if n (expt x (/ 1.0 n)) ; need 1.0 to make the result float
                        (sqrt x))) )",
         "root"},
        {"(root 9 )", "3"},
        {"(root 9 2)", "3"},
        {"(root 27 3)", "3"},
        {R"( (defun root (x &optional (n 2))
               (expt x (/ 1.0 n))) ; need 1.0 to make the result float
              )",
         "root"},
        {"(root 9 )", "3"},
        {"(root 9 2)", "3"},
        {"(root 27 3)", "3"},
        {R"((defun count-elements-optional (l &optional (result 0))
                (if (endp l)
                    result
                    (count-elements-optional (rest l) (+ 1 result)))) )",
         "count-elements-optional"},
        {"(count-elements-optional '(fast computers are nice))", "4"},
        // pg. 87
        {R"((defun raise-aux (result number-list)
                (if (endp number-list)
                    result
                    (raise-aux (expt result (first number-list))
                                (rest number-list)))) )",
         "raise-aux"},
        {R"((defun raise (x &rest numbers)
                (raise-aux x numbers)) )",
         "raise"},
        {"(raise 2)", "2"},
        {"(raise 2 3)", "8"},
        {"(raise 2 3 5)", "32768"},

        {R"((defun rotate-list (l &key direction distance)
                (if (eq direction 'left)
                    (rotate-list-left l (if distance distance 1))
                    (rotate-list-right l (if distance distance 1)))) )",
         "rotate-list"},
        {R"((defun rotate-list-right (l n)
                (if (zerop n )
                    l
                    (rotate-list-right (append (last l) (butlast l))
                                      (- n 1)))) )",
         "rotate-list-right"},
        {R"((defun rotate-list-left (l n)
                (if (zerop n )
                    l
                    (rotate-list-left (append (rest l) (list (first l)))
                                      (- n 1)))) )",
         "rotate-list-left"},
        {"(rotate-list '(a b c d e))", "(e a b c d)"},
        {"(rotate-list '(a b c d e) :direction 'left)", "(b c d e a)"},
        {"(rotate-list '(a b c d e) :distance 2)", "(d e a b c)"},
        {"(rotate-list '(a b c d e) :direction 'left :distance 2)", "(c d e a b)"},
        {"(rotate-list '(a b c d e) :distance 2 :direction 'left)", "(c d e a b)"},
        {R"( (defun rotate-list (l &key direction (distance 1))
                (if (eq direction 'left)
                    (rotate-list-left l distance)
                    (rotate-list-right l distance))) )",
         "rotate-list"},
        {"(rotate-list '(a b c d e))", "(e a b c d)"},
        {"(rotate-list '(a b c d e) :direction 'left)", "(b c d e a)"},
        {"(rotate-list '(a b c d e) :distance 2)", "(d e a b c)"},
        {"(rotate-list '(a b c d e) :direction 'left :distance 2)", "(c d e a b)"},
        {"(rotate-list '(a b c d e) :distance 2 :direction 'left)", "(c d e a b)"},
        {R"( (defun both-ends-with-aux 
                    (whole-list &aux
                                (element (first whole-list))
                                (trailer (last whole-list)))
                    (cons element trailer)) )",
         "both-ends-with-aux"},
        // { "(both-ends-with-aux '(a b c d e))", "(e a b c d)" }, // no &aux parameters

        {R"( (defun user-defined-length (l)
                    (if (endp l)
                    0
                    (+ 1 (user-defined-length (rest l))))) )",
         "user-defined-length"},
        {"(user-defined-length '(a b c d e))", "5"},
        {R"( (defun user-defined-append2 (l1 l2)
                    (if (endp l1)
                    l2
                    (cons (first l1) (user-defined-append2 (rest l1) l2)))) )",
         "user-defined-append2"},
        {"(user-defined-append2 '(a b c d e) '(1 2 3))", "(a b c d e 1 2 3)"},
        {R"( (defun user-defined-append (&rest lists)
                    (append-aux lists)) )",
         "user-defined-append"},
        {R"( (defun append-aux (lists)
                (if (endp lists)
                    nil
                    (user-defined-append2 (first lists) (append-aux (rest lists))))) )",
         "append-aux"},
        {"(user-defined-append '(a b c d e) '(1 2 3))", "(a b c d e 1 2 3)"},
        {"(user-defined-append '(a b c d e) '(1 2 3) '(red green blue))",
         "(a b c d e 1 2 3 red green blue)"},

    };
    test_Evaluator(tests);
}

TEST(chapter, 6) {
    std::vector<TestEval> tests = {
        {R"( (setf book-example-1 '((Artifical Intelligence)
                                    (Patrick Henry Winston)
                                    (Technical AI))) )",
         "((Artifical Intelligence) (Patrick Henry Winston) (Technical AI))"},
        {"(second book-example-1)", "(Patrick Henry Winston)"},
        {R"( (setf book-example-2 '((title (Artifical Intelligence))
                                    (author (Patrick Henry Winston))
                                    (classification (Technical AI)))) )",
         "((title (Artifical Intelligence)) (author (Patrick Henry Winston)) (classification "
         "(Technical AI)))"},
        {"(second (assoc 'author book-example-2))", "(Patrick Henry Winston)"},
        {R"( (setf book-example-3 '((title (Artifical Intelligence))
                                    (author (Patrick Henry Winston))
                                    (classification (Technical AI))
                                    (loaned-to (Karen Prendergast))
                                    (loaned-on (26 May 88)))) )",
         "((title (Artifical Intelligence)) (author (Patrick Henry Winston)) (classification "
         "(Technical AI)) (loaned-to (Karen Prendergast)) (loaned-on (26 May 88)))"},
        {R"( (defun book-author (book) 
                    (second book)) )",
         "book-author"},
        {"(book-author book-example-1)", "(Patrick Henry Winston)"},
        {R"( (defun book-author (book) 
                    (second (assoc 'author book))) )",
         "book-author"},
        {"(book-author book-example-2)", "(Patrick Henry Winston)"},
        {"(book-author book-example-3)", "(Patrick Henry Winston)"},
        {R"( (defun make-book (title author classification) 
                    (list (list 'title title)
                        (list 'author author)
                        (list 'classification classification))) )",
         "make-book"},
        {R"( (setf book-example-4 (make-book '(Common Lisp)
                                    '(Guy Steele)
                                    '(Technical Lisp))) )",
         "((title (Common Lisp)) (author (Guy Steele)) (classification (Technical Lisp)))"},
        {R"( (defun book-title (book) 
                    (second (assoc 'title book))) )",
         "book-title"},
        {R"( (defun book-author (book) 
                    (second (assoc 'author book))) )",
         "book-author"},
        {R"( (defun book-classification (book) 
                    (second (assoc 'classification book))) )",
         "book-classification"},
        {R"( (defun book-author-writer (book author) 
                    (cons  (list 'author author) book)) )",
         "book-author-writer"},
        {R"( (setf book-example-4 (book-author-writer book-example-4 '(Guy L Steele)
                                    )) )",
         "((author (Guy L Steele)) (title (Common Lisp)) (author (Guy Steele)) (classification "
         "(Technical Lisp)))"},
        {"(book-author book-example-4)", "(Guy L Steele)"},
        {R"( (defun book-author-writer (book author) 
                    (if (eql 'author (first (first book)))
                        (cons (list 'author author) (rest book))
                        (cons (first book)
                              (book-author-writer (rest book author))))) )",
         "book-author-writer"},
        {R"( (setf book-example-4 (book-author-writer book-example-4 '(Guy L Steele)
                                    )) )",
         "((author (Guy L Steele)) (title (Common Lisp)) (author (Guy Steele)) (classification "
         "(Technical Lisp)))"},
        {"(book-author book-example-4)", "(Guy L Steele)"},

        // pg. 99
        {R"( (setf books
                    (list 
                        (make-book '(artificial intelligence)
                                    '(patrick henry winston)
                                    '(technical ai))
                        (make-book '(common lisp)
                                    '(guy l steele)
                                    '(technical lisp))
                         (make-book '(moby dick)
                                    '(herman melville)
                                    '(fiction))
                         (make-book '(tom sayer)
                                    '(mark twain)
                                    '(fiction))
                         (make-book '(the black orchid)
                                    '(rex stout)
                                    '(fiction mystery))
                                     )) )",
         "(((title (artificial intelligence)) (author (patrick henry winston)) (classification "
         "(technical ai))) ((title (common lisp)) (author (guy l steele)) (classification "
         "(technical lisp))) ((title (moby dick)) (author (herman melville)) (classification "
         "(fiction))) ((title (tom sayer)) (author (mark twain)) (classification (fiction))) "
         "((title (the black orchid)) (author (rex stout)) (classification (fiction mystery))))"},
        {R"( (defun list-authors (books)
                (if (endp books)
                    nil
                    (cons (book-author (first books))
                            (list-authors (rest books))))) )",
         "list-authors"},
        {"(list-authors books)",
         "((patrick henry winston) (guy l steele) (herman melville) (mark twain) (rex stout))"},
        {R"( (defun fictionp (book)
                (member 'fiction (book-classification book))) )",
         "fictionp"},
        {"(fictionp '((title (tom sayer)) (author (mark twain)) (classification (fiction))))",
         "(fiction)"},
        {"(fictionp '((title (common lisp)) (author (guy l steele)) (classification (technical "
         "lisp))))",
         "nil"},
        {R"( (defun list-fiction-books (books)
                (cond ((endp books) nil)
                      ((fictionp (first books))
                          (cons (first books)
                                 (list-fiction-books (rest books))))
                       (t (list-fiction-books (rest books))))) )",
         "list-fiction-books"},
        {"(list-fiction-books books)",
         "(((title (moby dick)) (author (herman melville)) (classification (fiction))) ((title "
         "(tom sayer)) (author (mark twain)) (classification (fiction))) ((title (the black "
         "orchid)) (author (rex stout)) (classification (fiction mystery))))"},
        // pg. 101
        {"(length (list-fiction-books books))", "3"},
        {"(first (list-fiction-books books))",
         "((title (moby dick)) (author (herman melville)) (classification (fiction)))"},
        {R"( (defun count-fiction-books (books)
                (cond ((endp books) 0)
                      ((fictionp (first books))
                          (+ 1  (count-fiction-books (rest books))))
                       (t (count-fiction-books (rest books))))) )",
         "count-fiction-books"},
        {R"( (defun find-first-fiction-books (books)
                (cond ((endp books) nil)
                      ((fictionp (first books))
                          (first books))
                       (t (find-first-fiction-books (rest books))))) )",
         "find-first-fiction-books"},
        {"(count-fiction-books books)", "3"},
        {"(find-first-fiction-books books)",
         "((title (moby dick)) (author (herman melville)) (classification (fiction)))"},

        // pg. 104
        {"(mapcar #'oddp '(1 2 3))", "(t nil t)"},
        {"(mapcar #'= '(1 2 3) '(3 2 1))", "(nil t nil)"},
        {"(list-authors books)",
         "((patrick henry winston) (guy l steele) (herman melville) (mark twain) (rex stout))"},

        {"(mapcar #'book-author books)",
         "((patrick henry winston) (guy l steele) (herman melville) (mark twain) (rex stout))"},

        {"(remove-if-not #'fictionp books)",
         "(((title (moby dick)) (author (herman melville)) (classification (fiction))) ((title "
         "(tom sayer)) (author (mark twain)) (classification (fiction))) ((title (the black "
         "orchid)) (author (rex stout)) (classification (fiction mystery))))"},

        {"(remove-if #'fictionp books)",
         "(((title (artificial intelligence)) (author (patrick henry winston)) (classification "
         "(technical ai))) ((title (common lisp)) (author (guy l steele)) (classification "
         "(technical lisp))))"},

        {"(count-if #'fictionp books)", "3"},
        {"(find-if #'fictionp books)",
         "((title (moby dick)) (author (herman melville)) (classification (fiction)))"},

        // pg. 107
        {"(funcall #'first '(e1 e2 e3))", "e1"},
        {"(first '(e1 e2 e3))", "e1"},
        {"(funcall #'append '(a b) '(x y))", "(a b x y)"},
        {"(append '(a b) '(x y))", "(a b x y)"},
        {R"( (defun toss (argument procedure)
                (funcall procedure argument)) )",
         "toss"},
        {"(toss '(victim of attack) #'first)", "victim"},
        {"(toss '(victim of attack) #'rest)", "(of attack)"},
        {R"( (defun toss (argument procedure)
                (procedure argument)) )",
         "toss"},
        {"(toss '(victim of attack) #'first)",
         "Eval error: A non function in function location #'first"},
        {"(apply #'first '((e1 e2 e3)))", "e1"},
        {"(first '(e1 e2 e3))", "e1"},
        {"(apply #'append '((a b) (x y)))", "(a b x y)"},
        {"(append '(a b) '(x y))", "(a b x y)"},
        {"(apply #'+ '(1 2 3 4 5 6))", "21"},
        // { "(apply #'+ 1 2 3 '(4 5 6))", "21" }, not supported
        {R"( (defun toss (argument procedure)
                (apply procedure (list argument))) )",
         "toss"},
        {"(toss '(victim of attack) #'first)", "victim"},
        {"(toss '(victim of attack) #'rest)", "(of attack)"},

        {R"( (defun book-last-name (book)
                (first (last (book-author book)))) )",
         "book-last-name"},
        {"(mapcar #'book-last-name books)", "(winston steele melville twain stout)"},
        // { "(mapcar #'(lambda (book) (first (last (book-author book)))) books)", "" },
        {"(mapcar (lambda (book) (first (last (book-author book)))) books)",
         "(winston steele melville twain stout)"},
        // { "(remove-if-not #'(lambda (book) (member 'fiction (book-classification book)))
        // books)",
        //     "(((title (moby dick)) (author (herman melville)) (classification (fiction)))
        //     ((title (tom sayer)) (author (mark twain)) (classification (fiction))) ((title (the
        //     black orchid)) (author (rex stout)) (classification (fiction mystery))))" },
        {"(remove-if-not (lambda (book) (member 'fiction (book-classification book))) books)",
         "(((title (moby dick)) (author (herman melville)) (classification (fiction))) ((title "
         "(tom sayer)) (author (mark twain)) (classification (fiction))) ((title (the black "
         "orchid)) (author (rex stout)) (classification (fiction mystery))))"},
        // { "(funcall #'(lambda (parameter) (first parameter)) '(e1 e2 e3))", "e1" },
        {"(funcall (lambda (parameter) (first parameter)) '(e1 e2 e3))", "e1"},
        // { "(apply #'(lambda (parameter1 parameter2) (append parameter1 parameter2)) '((a b) (x
        // y)))", "(a b x y)" },
        {"(apply (lambda (parameter1 parameter2) (append parameter1 parameter2)) '((a b) (x y)))",
         "(a b x y)"},
    };
    test_Evaluator(tests);
}

TEST(chapter, A) {
    std::vector<TestEval> tests = {
        // { "()", "" },
    };
    test_Evaluator(tests);
}