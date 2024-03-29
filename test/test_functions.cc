//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "test.hh"

#include <gtest/gtest.h>

TEST(eval, noargs) {
    std::vector<TestEval> tests = {
        {"(defun k () 1)", "k"},
        {"(k)", "1"},
        {"(defun k () 2)", "k"},
        {"(k)", "2"},

        {"(k 1)", "Eval error: k: invalid number of arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, 1args) {
    std::vector<TestEval> tests = {
        {"(defun id (x) x)", "id"},
        {"(id 1)", "1"},

        {"(id 1 2)", "Eval error: id: invalid number of arguments"},
        {"(id)", "Eval error: id: invalid number of arguments"},

        {"(defun idx (1) x)", "Eval error: idx parameter needs to be an atom :1"},
    };
    test_Evaluator(tests);
}

TEST(eval, 2args) {
    std::vector<TestEval> tests = {
        {"(defun g (x y) x)", "g"},
        {"(g 1 2)", "1"},
        {"(defun g (x y) y)", "g"},
        {"(g 1 2)", "2"},

        {"(g 1)", "Eval error: g: invalid number of arguments"},
        {"(g)", "Eval error: g: invalid number of arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, 3args) {
    std::vector<TestEval> tests = {
        {"(defun h (x y z) x)", "h"},
        {"(h 1 2 3)", "1"},
        {"(defun h (x y z) y)", "h"},
        {"(h 1 2 3)", "2"},
        {"(defun h (x y z) z)", "h"},
        {"(h 1 2 3)", "3"},

        {"(h 1 2)", "Eval error: h: invalid number of arguments"},
        {"(h 1)", "Eval error: h: invalid number of arguments"},
        {"(h)", "Eval error: h: invalid number of arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, 4args) {
    std::vector<TestEval> tests = {
        {"(defun hh (x y z a) x)", "hh"},
        {"(hh 1 2 3 4)", "1"},
        {"(defun hh (x y z a) y)", "hh"},
        {"(hh 1 2 3 4)", "2"},
        {"(defun hh (x y z a) z)", "hh"},
        {"(hh 1 2 3 4)", "3"},
        {"(defun hh (x y z a) a)", "hh"},
        {"(hh 1 2 3 4)", "4"},

        {"(hh 1 2 3)", "Eval error: hh: invalid number of arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, defun) {
    std::vector<TestEval> tests = {
        // defun.1
        {R"( (defun a (x)
		 	(atom x)) )",
         "a"},
        {"(a t)", "t"},
        {"(a nil)", "t"},
        {"(a 34)", "t"},
        {"(a '(a b))", "nil"},
        {R"( (defun xcadr (x)
		 	(car (cdr x))) )",
         "xcadr"},
        {"(xcadr '(a b))", "b"},
        {"(xcadr 'a)", "nil"},
        {"(xcadr '(a (b c) d))", "(b c)"},

        // multi-expression defines
        {R"( (defun hx (x)
		 	(list 'a x)
		 	(list 'c x)
		 	(list 'z x)) )",
         "hx"},
        {"(hx 1)", "(z 1)"},

        // Doc string
        {R"( (defun dx (x)
          	"Doc string here."
          	(list 'd x)) )",
         "dx"},
        {"(dx 1)", "(d 1)"},

        // functions are not variables
        {"(+ dx 1)", "Eval error: + arguments needs to be a number"},

        // defun.2
        {"(defun)", "Eval error: defun expecting at least 2 arguments"},
        {"(defun x)", "Eval error: defun expecting at least 2 arguments"},
        {"(defun x y)", "Eval error: x needs a list of parameters"},
        {"(defun x () t)", "x"},
        {"(x)", "t"},
    };
    test_Evaluator(tests);
}

TEST(eval, defun_recursive) {
    std::vector<TestEval> tests = {
        {R"( (defun factorial (n)
		 	(defun iter (product counter max-count)
		 	  (if (> counter max-count)
		 		  product
		 		  (iter (* counter product)
		 				(+ counter 1)
		 				max-count)))
			 (iter 1 1 n)) )",
         "factorial"},
        {"(factorial 0)", "1"},
        {"(factorial 1)", "1"},
        {"(factorial 4)", "24"},
        {"(factorial 14)", "87178291200"},

        // // defun.3
        {R"( (defun subst(x y z)
                (cond(nil nil)
                     ((atom z) 
                        (cond((eq z y)x)
                              (t z)))
                    (t (cons(subst x y(car z))
                       (subst x y(cdr z)))))) )",
         "subst"},
        {"(subst 'm 'b 'b)", "m"},
        {"(subst 'm 'b '(a b))", "(a m)"},
        {"(subst 'm 'b '(a b (a b c) d))", "(a m (a m c) d)"},

        // Fix for crash with unbound variables
        {"(defun addx (s d) (+ s d))", "addx"},
        {"(addx 1 2)", "3"},
        {"(addx 1)", "Eval error: addx: invalid number of arguments"},

        // Empty missing function body should return nil
        {"(defun fff () )", "fff"},
        {"(fff)", "nil"},
    };
    test_Evaluator(tests);
}

TEST(eval, defun2) {
    std::vector<TestEval> tests = {
        {"(defconstant x 1)", "x"},
        {"x", "1"},
        {"(defun k () x)", "k"},
        {"(k)", "1"},

        {"(defvar yy 2)", "yy"},
        {"yy", "2"},
        {"(defun g () (defvar yy 4) yy)", "g"},
        {"yy", "2"},
        {"(g)", "4"},
        {"yy", "2"},
    };
    test_Evaluator(tests);
}

TEST(eval, closures) {
    std::vector<TestEval> tests = {
        {"(defvar a 1)", "a"},
        {"(defun b () a)", "b"},
        {"(b)", "1"},
        {"(defvar a 3)", "a"},
        {"a", "3"},
        {"(b)", "3"},

        {"(defun c (x) (defun d(y) (+ x y )) (d x))", "c"},
        {"(c 2)", "4"},
    };
    test_Evaluator(tests);
}

TEST(eval, undef) {
    std::vector<TestEval> tests = {
        {"(defun d (x) (f x))", "d"}, // f not defined yet
        {"(defun f (x) (g x))", "f"}, // g not defined yet
        {"(defun g (x) x)", "g"},     {"(f 1)", "1"}, {"(d 3)", "3"},
    };
    test_Evaluator(tests);
}

TEST(eval, littlelisper) {
    std::vector<TestEval> tests = {
        // The little lisper, pg. 17.
        {R"( (defun lat (l) 
			(cond
				((null l) t)
				((atom (car l)) (lat (cdr l)))
				(t nil))) )",
         "lat"},
        {"(lat '(J S c e n c f))", "t"},
        {"(lat '((J) S c e n c f))", "nil"},
        {"(lat '(J (S c) e n c f))", "nil"},
        {"(lat '())", "t"},
        {R"( (defun lat2(l)(if (null l)
                            t(if (atom(car l))(lat2(cdr l))
                                    nil))) )",
         "lat2"},
        {"(lat2 '(J S c e n c f))", "t"},
        {"(lat2 '((J) S c e n c f))", "nil"},
        {"(lat2 '(J (S c) e n c f))", "nil"},
        {"(lat2 '())", "t"},

        // The Little Lisper, pg. 41.
        {R"( (defun rember (a lat) 
		(cond
			((null lat) '())
			((eq (car lat) a) (cdr lat))
			(t (cons (car lat)
					 (rember a (cdr lat)))))) )",
         "rember"},
        {"(rember 'b  '())", "nil"},
        {"(rember 'b  '(b l a t))", "(l a t)"},
        {"(rember 'a  '(b l a t))", "(b l t)"},
        {"(rember 'x  '(b l a t))", "(b l a t)"},

        // The Little Lisper, pg. 48
        {R"( (defun firsts (x)
          (cond
          	((null x) nil)
          	(t (cons (caar x)
          			 (firsts (cdr x)))))) )",
         "firsts"},
        {"(firsts '())", "nil"},
        {"(firsts '((a b) (c d) (e f)))", "(a c e)"},
        {"(firsts '((a b) (c) (d e f)))", "(a c d)"},

        // The Little Lisper, pg. 54
        {R"( (defun insertr (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons old (cons new (cdr lat))))
			(t (cons (car lat) (insertr new old (cdr lat)))))) )",
         "insertr"},
        {"(insertr 'e 'd '(a b c d f g d))", "(a b c d e f g d)"},

        {R"( (defun insertl (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons new (cons old (cdr lat))))
			(t (cons (car lat) (insertl new old (cdr lat)))))) )",
         "insertl"},
        {"(insertl 'e 'd '(a b c d f g d))", "(a b c e d f g d)"},

        {R"( (defun subst (new old lat) 
		(cond
			((null lat) nil)
			((eq (car lat) old)
				(cons new (cdr lat)))
			(t (cons (car lat) (subst new old (cdr lat)))))) )",
         "subst"},
        {"(subst 'e 'd '(a b c d f g d))", "(a b c e f g d)"},

        {R"( (defun subst2 (new o1 o2 lat) 
		(cond
			((null lat) nil)
			((or (eq (car lat) o1) (eq (car lat) o2))
				(cons new (cdr lat)))
			(t (cons (car lat) (subst2 new o1 o2 (cdr lat)))))) )",
         "subst2"},
        {"(subst2 'e 'd 'f '(a b c d f g d))", "(a b c e f g d)"},
        {"(subst2 'e 'd 'f '(a b c f g d))", "(a b c e g d)"},

        // Little Lisp pg.
        {R"( (defun eqanp (a1 a2)
		(cond ((and (numberp a1) (numberp a2)) (= a1 a2))
			(t (eq a1 a2)))) )",
         "eqanp"},
        {R"( (defun eqlist (x1 x2)
		(cond 
			((and (null x1) (null x2)) t)
			((or (null x1) (null x2)) nil)
			((and (not (atom (car x1))) (not (atom (car x2))))
				(and (eqlist (car x1) (car x2))
					 (eqlist (cdr x1) (cdr x2))))
			((or (not (atom (car x1))) (not (atom (car x2)))) nil)
			(t (and (eqanp (car x1) (car x2))
					(eqlist (cdr x1) (cdr x2))))
		)) )",
         "eqlist"},

        {"(eqlist '() '())", "t"},
        {"(eqlist '() '(s))", "nil"},
        {"(eqlist '(1) '(1))", "t"},
        {"(eqlist '(1 2) '(1 2))", "t"},
        {"(eqlist '(1 2) '(1 3))", "nil"},
        {"(eqlist '(a b c f g d) '(a b c f g d))", "t"},
        {"(eqlist '(a (b) c f g d) '(a (b) c f g d))", "t"},
        {"(eqlist '(a (b) c f g d) '(a (b) c c g d))", "nil"},
    };
    test_Evaluator(tests);
}

TEST(eval, demos) {
    std::vector<TestEval> tests = {

        {R"( (defun fib (n)
	 		(if (<= n 1)
	 			1
	 			(+ (fib (- n 1)) (fib (- n 2))))) )",
         "fib"},
        {R"( (fib 20) )", "10946"},

        {R"( (defun subst-x (x y z)
	(cond 
		((atom z)
			(cond ((eq z y) x)
					(t z)))
	(t (cons (subst-x x y (car z))
			 (subst-x x y (cdr z)))))) )",
         "subst-x"},
        {R"( (subst-x 'm 'b '(a b (a b (a b c) c) d a b (a b (a b c) c) c)) )",
         "(a m (a m (a m c) c) d a m (a m (a m c) c) c)"},

        {R"( (defun firstdenomination (kindsofcoins)
		(cond ((= kindsofcoins 1) 1)
			  ((= kindsofcoins 2) 2)
			  ((= kindsofcoins 3) 5)
			  ((= kindsofcoins 4) 10)
			  ((= kindsofcoins 5) 20)
			  ((= kindsofcoins 6) 50)
			  ((= kindsofcoins 7) 100)
			  ((= kindsofcoins 8) 200))
				)

	(defun cc (amount kindsofcoins)
		(cond ((= amount 0) 1)
			  ((or (< amount 0) (= kindsofcoins 0)) 0)
			   (t (+ (cc amount (- kindsofcoins 1))
					(cc (- amount (firstdenomination kindsofcoins)) kindsofcoins)))))
		
	(defun countchange (amount)
		(cc amount 5)) )",
         R"(firstdenomination
cc
countchange)"},
        {R"( (countchange 55) )", "596"},
    };
    test_Evaluator(tests);
}

TEST(eval, lambda) {
    std::vector<TestEval> tests = {
        {"((lambda (x) (cons x '(b))) 'a)", "(a b)"},
        {"((lambda (x y) (cons x (cdr y))) 'z '(a b c))", "(z b c)"},
        {"((lambda (x) (atom x)) 's)", "t"},

        {R"( ((lambda (a b)
			(list a b)) 'd 'g) )",
         "(d g)"},
        {R"( ((lambda (a b)
			(list a b)) 's 'y) )",
         "(s y)"},
        // multi-expression lambdas
        {R"( ((lambda (x) 
			(cons x '(z))
			(cons x '(y))
			(cons x '(b))) 'a) )",
         "(a b)"},
        // lambda.2
        {"((lambda))", "Eval error: lambda expecting at least 1 arguments"},
        {"((lambda (x)) )", "Eval error: lambda: invalid number of arguments"},
        {"((lambda (ss) ()))", "Eval error: lambda: invalid number of arguments"},
        {"((lambda (x) (list x x)) 'a)", "(a a)"},

        // { "(functionp (lambda (x)))", "t" },

        // lambda.3
        {"(lambda)", "Eval error: lambda expecting at least 1 arguments"},
        {"(lambda x)", "Eval error: lambda needs a list of parameters"},
        {"(lambda x 1)", "Eval error: lambda needs a list of parameters"},
        {"((lambda nil 'hello))", "Eval error: lambda needs a list of parameters"},
    };
    test_Evaluator(tests);
}

TEST(eval, defmacro) {
    std::vector<TestEval> tests = {
        {"(defmacro eight () (+ 3 5))", "eight"},
        {"(eight)", "8"},

        {"(defmacro if-x (x y z) `(cond (,x ,y) (t ,z)))", "if-x"},
        {"(if-x (atom 'x) 1 2)", "1"},
        {"(if-x (atom '(a b)) 1 2)", "2"},

        {"(defmacro when-x (test expr) `(cond (,test ,expr) (t nil)))", "when-x"},
        {"(when-x t 2)", "2"},
        {"(when-x nil 2)", "nil"},
        {"(when-x (atom '(a b)) 2)", "nil"},

        {"(defmacro when-pos (num expr) `(if (> ,num 0) ,expr))", "when-pos"},
        {"(when-pos 2 'positive)", "positive"},
        {"(when-pos 0 'positive)", "nil"},
        {"(when-pos -2 'positive)", "nil"},
        {"(when-pos (+ -2 4) 'positive)", "positive"},

        {"(defmacro test3 (x) `(quote (a ,x)))", "test3"},
        {"(test3 'b)", "(a 'b)"},

        {"(defmacro m3 (x) \"Doc string here\" `(list ,x))", "m3"},
        {"(m3 1)", "(1)"},
    };
    test_Evaluator(tests);
}

TEST(eval, macro) {
    std::vector<TestEval> tests = {
        {"(defvar eight (macro () (+ 3 5)))", "eight"},
        {"(eight)", "8"},

        {"(defvar when-pos (macro (num expr) `(if (> ,num 0) ,expr)))", "when-pos"},
        {"(when-pos 2 'positive)", "positive"},
        {"(when-pos 0 'positive)", "nil"},
        {"(when-pos -2 'positive)", "nil"},
        {"(when-pos (+ -2 4) 'positive)", "positive"},
    };
    test_Evaluator(tests);
}

TEST(eval, optional) {
    std::vector<TestEval> tests = {
        {"(defun f (x y) (list x y))", "f"},
        {"(f 1 2)", "(1 2)"},
        {"(f 1)", "Eval error: f: invalid number of arguments"},

        {"(defun g (x &optional y) (list x y))", "g"},
        {"(g 1 2)", "(1 2)"},
        {"(g 1)", "(1 nil)"},

        {"(defun h (x &optional (y 777)) (list x y))", "h"},
        {"(h 1 2)", "(1 2)"},
        {"(h 1)", "(1 777)"},

        {"(defun j (x y &optional z) (list x y z))", "j"},
        {"(j 1 2 3)", "(1 2 3)"},
        {"(j 1 2)", "(1 2 nil)"},
        {"(j 1)", "Eval error: j: invalid number of arguments"},

        {" (defvar l (lambda (x &optional y) (list x y)))", "l"},
        {"(l 1)", "(1 nil)"},

        {"(defun m (x &optional (1 1)) (list x y z))", "m"},
        {"(m 1)", "Eval error: m: optional default argument is not an atom 1"},
        {"(defun m (x &optional ()) (list x y z))", "m"},
        {"(m 1)", "Eval error: m: default argument not 2 member list nil"},
    };
    test_Evaluator(tests);
}

TEST(eval, rest) {
    std::vector<TestEval> tests = {
        {"(defun f (x &rest y) (list x y))", "f"},
        {"(f 1)", "(1 nil)"},
        {"(f 1 2)", "(1 (2))"},
        {"(f 1 2 3 4)", "(1 (2 3 4))"},

        {"(f)", "Eval error: unbound variable: x"},
    };
    test_Evaluator(tests);
}

TEST(eval, keyword) {
    std::vector<TestEval> tests = {
        {R"((defun particle(&key(charge 0) spin)
                (list charge spin)) )",
         "particle"},
        {"(particle :charge 1 :spin 'up)", "(1 up)"},
        {"(particle :charge -1 :spin 'up)", "(-1 up)"},
        {"(particle :charge -1 :spin 'down)", "(-1 down)"},
        {"(particle :spin 'up :charge 1)", "(1 up)"},
        {"(particle :charge 1)", "(1 nil)"},
        {"(particle :spin 'up)", "(0 up)"},
        {"(particle)", "(0 nil)"},

        {"(defun f (&key) 1)", "f"},
        {"(f)", "1"},

        {"(defun g (x &key) x)", "g"},
        {"(g 7)", "7"},
        {"(g)", "Eval error: unbound variable: x"},

        {"(defun h (x &key y) y)", "h"},
        {"(h 8)", "nil"},
        {"(h 8 :y 7)", "7"},
        {"(h)", "nil"}, // x not used
    };
    test_Evaluator(tests);
}

TEST(eval, function) {
    std::vector<TestEval> tests = {
        {"#'atom", "#'atom"},
        {"#'+", "#'+"},

        {"(function atom)", "#'atom"},
        {"(function +)", "#'+"},

        {"(function nil)", "Eval error: function function name needs to an atom"},
        {"(function atom atom)", "Eval error: function expecting an argument"},
    };
    test_Evaluator(tests);
}

TEST(eval, functionp) {
    std::vector<TestEval> tests = {
        {"(functionp #'+)", "t"},
        {"(functionp #'atom)", "t"},
        {"(functionp #'cadar)", "t"},
        {"(functionp #'caxar)", "nil"},

        {"(functionp t)", "nil"},
        {"(functionp 1)", "nil"},

        {"(defun f (x) x)", "f"},
        {"(functionp f)", "t"}, // type-1 Lisp, bound variables can be functions
        {"(functionp #'f)", "t"},
        {"(defmacro fm (x) x)", "fm"},
        {"(functionp #'fm)", "t"},
        {"(functionp #'length)", "t"},
        {"(functionp (lambda (x) (* x x)))", "t"},
        {"(functionp '(lambda (x) (* x x)))", "nil"},

        {"(functionp 's)", "nil"},
        {"(functionp nil)", "nil"},
        {"(functionp)", "Eval error: functionp expecting at least 1 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, fboundp2) {
    std::vector<TestEval> tests = {
        {"(fboundp 'atom)", "t"},
        {"(fboundp 'caar)", "t"},
        {"(fboundp '+)", "t"},
        {"(defun f(x) x)", "f"},
        {"(fboundp 'f)", "t"},
        {"(defmacro g(x) x)", "g"},
        {"(fboundp 'g)", "t"},
        {"(fboundp 'h)", "nil"},

        {"(fboundp)", "Eval error: fboundp expecting at least 1 arguments"},
        {"(fboundp 1)", "nil"},
    };
    test_Evaluator(tests);
}

TEST(eval, fboundp) {
    std::vector<TestEval> tests = {
        {"(defun f(x) x)", "f"},
        {"(fboundp 'f)", "t"},
        {"(fmakunbound 'f)", "f"},
        {"(fboundp 'f)", "nil"},
        {"(f 1)", "Eval error: Can't evaluate (f 1)"},
        {"(defmacro g(x) x)", "g"},
        {"(fboundp 'g)", "t"},
        {"(fmakunbound 'g)", "g"},
        {"(fboundp 'g)", "nil"},
        {"(g 1)", "Eval error: Can't evaluate (g 1)"},

        {"(fmakunbound)", "Eval error: fmakunbound expecting an argument"},
    };
    test_Evaluator(tests);
}

TEST(eval, funcall) {
    std::vector<TestEval> tests = {
        {"(funcall #'atom 1)", "t"},

        {"(funcall #'+ 1 2 3)", "6"},
        {"(funcall (lambda (x) (atom x)) 1)", "t"},
        {"(funcall (lambda (x) (atom x)) '(a b))", "nil"},

        {"(defun f(x) (zerop x))", "f"},
        {"(funcall #'f 1)", "nil"},
        {"(funcall #'f 0)", "t"},
        {"(funcall #'f -1)", "nil"},

        {"(defvar x #'atom )", "x"},
        {"(funcall x 1)", "t"},

        {"(funcall)", "Eval error: funcall expecting at least 2 arguments"},
        {"(funcall f)", "Eval error: funcall expecting at least 2 arguments"},
        {"(funcall 1 1)", "Eval error: funcall: Not function ref or lambda expression: 1"},
    };
    test_Evaluator(tests);
}

TEST(eval, apply) {
    std::vector<TestEval> tests = {
        {"(apply #'atom '(s))", "t"},

        {"(apply #'+ '(1 2 3))", "6"},
        {"(apply #'list '(a b))", "(a b)"},
        {"(apply #'and '(t t t))", "t"},
        {"(apply #'and '(t t t nil))", "nil"},
        {"(apply #'or '(nil nil nil 2))", "2"},
        {"(apply #'or '(nil 3 nil 2))", "3"},

        {"(apply (lambda (x) (atom x)) '(1))", "t"},

        {"(defun f(x) (zerop x))", "f"},
        {"(apply #'f '(1))", "nil"},
        {"(apply #'f '(0))", "t"},
        {"(apply #'f '(-1))", "nil"},

        // { "(apply #'+ 1 2 3 '(4 5 6))", "21" }, // not supported

        {"(apply)", "Eval error: apply expecting at least 2 arguments"},
        {"(apply f)", "Eval error: apply expecting at least 2 arguments"},
        {"(apply 1 1)", "Eval error: apply: Not function ref or lambda expression: 1"},
    };
    test_Evaluator(tests);
}

TEST(eval, identity) {
    std::vector<TestEval> tests = {
        {"(identity 1)", "1"},
        {"(funcall #'identity 1)", "1"},
        {"(funcall #'identity 'x)", "x"},

        {"(defvar c (constantly 17))", "c"},
        {"(funcall c 1)", "17"},
        {"(funcall c 's)", "17"},

        {"(defvar d (complement #'atom))", "d"},
        {"(funcall d 1)", "nil"},
        {"(funcall d '(1 2))", "t"},

        {"(funcall (complement #'zerop) 1)", "t"},
        {"(funcall (complement #'characterp) #\\A)", "nil"},
        {"(funcall (complement #'member) 'a '(a b c))", "nil"},
        {"(funcall (complement #'member) 'd '(a b c))", "t"},

    };
    test_Evaluator(tests);
}

TEST(eval, mapcar) {
    std::vector<TestEval> tests = {
        {"(mapcar #'+ '(1 2 3) '(4 5 6))", "(5 7 9)"},
        {"(defun id(x) x)", "id"},
        {"(mapcar #'id '(1 2 3) )", "(1 2 3)"},
        {"(mapcar #'car '((1 a) (2 b) (3 c)))", "(1 2 3)"},

        {"(mapcar (lambda (n) (^ n n)) '(1 2 3 4) )", "(1 4 27 256)"},
        {"(mapcar #'cadr '((a b) (d e) (g h)) )", "(b e h)"},
        {"(mapcar #'abs '(3 -4 2 -5 -6))", "(3 4 2 5 6)"},
        {"(mapcar #'cons '(a b c) '(1 2 3))", "((a . 1) (b . 2) (c . 3))"},

        {"(mapcar #'list '(1 2 3))", "((1) (2) (3))"},

        // Test mapcar called in function definition
        {"(defun f (a b) (mapcar a b))", "f"},
        {"(f #'zerop '(1 0 3))", "(nil t nil)"},

        {"(defun g (a b) (mapcar a b))", "g"},
        {"(g (lambda (n) (^ n n)) '(1 2 3))", "(1 4 27)"},

        {"(mapcar #'+ '(s s s) '(a a a))", "Eval error: + arguments needs to be a number"},

        {"(mapcar)", "Eval error: mapcar expecting at least 2 arguments"},
        {"(mapcar #'f)", "Eval error: mapcar expecting at least 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, maplist) {
    std::vector<TestEval> tests = {
        {"(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))", "((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))"},

        {"(defun f(x) (cons 'foo x))", "f"},
        {"(maplist #'f '(a b c d))", "((foo a b c d) (foo b c d) (foo c d) (foo d))"},

        {"(maplist (lambda (x) (cons 'foo x)) '(a b c d))",
         "((foo a b c d) (foo b c d) (foo c d) (foo d))"},

        {"(defun g(x) (if (member (car x) (cdr x)) 0 1))", "g"},
        {"(maplist #'g '(a b a c d b c))", "(0 0 1 0 1 1 1)"},
        {"n", "Eval error: unbound variable: n"},

        {"(maplist)", "Eval error: maplist expecting at least 2 arguments"},
        {"(maplist #'f)", "Eval error: maplist expecting at least 2 arguments"},
    };
    test_Evaluator(tests);
}

TEST(eval, dolist) {
    std::vector<TestEval> tests = {

        {"(dolist (n '(α β γ δ ε) r) (setq r n))", "ε"},
        {"(dolist (n '(alpha beta gamma) r) (setq r n))", "gamma"},
        {"(dolist (n '() r) (setq r n))", "nil"},
        {"n", "Eval error: unbound variable: n"},
        {"r", "Eval error: unbound variable: r"},

        {"(dolist)", "Eval error: dolist expecting at least 2 arguments"},
        {"(dolist ())", "Eval error: dolist expecting at least 2 arguments"},
        {"(dolist (n))", "Eval error: dolist expecting at least 2 arguments"},
        {"(dolist (n 'a))", "Eval error: dolist expecting at least 2 arguments"},

        {"(dolist () (atom 's))", "Eval error: dolist: not enough vars in parameter list"},
        {"(dolist (n) (atom 's))", "Eval error: dolist: not enough vars in parameter list"},
    };
    test_Evaluator(tests);
}

TEST(eval, dotimes) {
    std::vector<TestEval> tests = {
        {"(dotimes (n 10 r) (setq r n))", "9"},
        {"(dotimes (n 23 r) (setq r (* n n)))", "484"},
        {"n", "Eval error: unbound variable: n"},
        {"r", "Eval error: unbound variable: r"},

        {"(defvar xxx 1)", "xxx"},
        {"(dotimes (n 6) (setq xxx (* n n)))", "nil"},
        {"xxx", "25"},

        {R"( (defun dotimes-expt (m n)
                (let ((result 1))
                    (dotimes (count n result)
                        (setf result (* m result))))) )",
         "dotimes-expt"},
        {"(dotimes-expt 3 2)", "9"},
        {"(dotimes-expt 2 3)", "8"},
        {"(dotimes-expt 2 32)", "4294967296"},

        {"(dotimes)", "Eval error: dotimes expecting at least 2 arguments"},
        {"(dotimes ())", "Eval error: dotimes expecting at least 2 arguments"},
        {"(dotimes (n))", "Eval error: dotimes expecting at least 2 arguments"},
        {"(dotimes (n 'a))", "Eval error: dotimes expecting at least 2 arguments"},

        {"(dotimes () (atom 's))", "Eval error: dotimes: not enough vars in parameter list"},
        {"(dotimes (x) (atom 's))", "Eval error: dotimes: not enough vars in parameter list"},
    };
    test_Evaluator(tests);
}

TEST(eval, keywordp) {
    std::vector<TestEval> tests = {
        {"(keywordp 1)", "nil"},
        {"(keywordp :hello)", "t"},
    };
    test_Evaluator(tests);
}

TEST(eval, do) {
    std::vector<TestEval> tests = {
        {"(do ((x 1 (1+ x))) ((> x 10) r) (setf r (+ x x)))", "20"},
        {"(do ((x 1 (1+ x)) (y 9 (1- y))) ((> x 10) r) (setf r (+ x y)))", "10"},
        {R"( (do ((x 1)) ((> x 10) r) 
                  (setf r (+ x x)) 
                  (setf x (1+ x))) )",
         "20"},
        {R"( (defun do-expt (m n) 
                (do ((result 1 (* m result))
                    (exponent n (- exponent 1)))
                    ((zerop exponent) result))) )",
         "do-expt"},
        {"(do-expt 2 4)", "16"},
        {R"( (defun do-expt (m n)
                (do ((result 1)
                    (exponent n))
                  ((zerop exponent) result)
                    (setf result (* m result))
                    (setf exponent (- exponent 1)))) )",
         "do-expt"},
        {"(do-expt 2 4)", "16"},

        // error, value not set
        {R"( (defun do-expt (m n) 
                (do ((result m (* m result))
                    (exponent n (- exponent 1))
                    (counter (- exponent 1) (- exponent 1)))
                  ((zerop exponent) result)))  )",
         "do-expt"},
        {"(do-expt 2 4)", "Eval error: unbound variable: exponent"},

        // error, value not set
        {R"( (defun do-expt (m n) 
                (do* ((result m (* m result))
                     (exponent n (- exponent 1))
                    (counter (- exponent 1) (- exponent 1)))
                  ((zerop exponent) result)))  )",
         "do-expt"},
        {"(do-expt 2 4)", "32"}, // incorrect calculation, but uses do*

        // return nil
        {R"( (defun do-expt (m n) 
                (do ((result 1 (* m result))
                    (exponent n (- exponent 1)))
                    ((zerop exponent) ))) )",
         "do-expt"},
        {"(do-expt 2 4)", "nil"},

        {"(do)", "Eval error: do expecting at least 2 arguments"},
        {"(do())", "Eval error: do expecting at least 2 arguments"},
        // { "(do () ())", "Eval error: do: expecting a test nil" },
        {"(do(x)())", "Eval error: do: parameter is not a list x"},
        {"(do(())((null x)))", "Eval error: do: not enough vars in parameter list"},
        {"(do((x))((null x)))", "Eval error: do: not enough vars in parameter list"},
    };
    test_Evaluator(tests);
}

TEST(eval, hash_function) {
    std::vector<TestEval> tests = {
        {"#'(lambda (x) x)", "λ:lambda : (x)"},
        {"#'(lambda (x))", "λ:lambda : nil"},

        {"(mapcar #'(lambda (x) x) '(1 2 3) )", "(1 2 3)"},
        {"(mapcar #'(lambda (x) (1+ x)) '(1 2 3) )", "(2 3 4)"},
        {"(mapcar #'(lambda (n) (^ n n)) '(1 2 3 4) )", "(1 4 27 256)"},

        {"#'()", "Parse error: #' expecting lambda expression nil"},
        {"#'(lambda )", "Parse error: #' expecting lambda expression (lambda)"},
        {"#'(atom x)", "Parse error: #' expecting lambda expression (atom x)"},
    };
    test_Evaluator(tests);
}
