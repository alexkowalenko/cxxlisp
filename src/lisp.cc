//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "lisp.hh"

#include <iostream>
#include <sstream>

#include "evaluator.hh"
#include "exceptions.hh"
#include "linereaderRL.hh"
#include "linereaderReplxx.hh"
#include "linereaderStream.hh"
#include "parser.hh"
#include "primitive.hh"
#include "symboltable.hh"

namespace ax {

class NullBuffer : public std::streambuf {
public:
    int overflow(int c) { return c; }
};

Lisp::Lisp(Options& o)
    : opt(o)
    , symboltable(nullptr)
{
}

inline const string stdlib = R"stdlib( 
(defun cadr (x)
	(car (cdr x)))
(defun caar (x)
	(car (car x)))
(defun cddr (x)
	(cdr (cdr x)))
(defun cdar (x)
	(cdr (car x)))

(defun caadr (x)
	(car (car (cdr x))))
(defun caaar (x)
	(car (car (car x))))
(defun caddr (x)
	(car (cdr (cdr x))))
(defun cadar (x)
	(car (cdr (car x))))
	
(defun cdadr (x)
	(cdr (car (cdr x))))
(defun cdaar (x)
	(cdr (car (car x))))
(defun cdddr (x)
	(cdr (cdr (cdr x))))
(defun cddar (x)
	(cdr (cdr (car x))))

(defun caaadr (x)
	(car (car (car (cdr x)))))
(defun caaaar (x)
	(car (car (car (car x)))))
(defun caaddr (x)
	(car (car (cdr (cdr x)))))
(defun caadar (x)
	(car (car (cdr (car x)))))
	
(defun cadadr (x)
	(car (cdr (car (cdr x)))))
(defun cadaar (x)
	(car (cdr (car (car x)))))
(defun cadddr (x)
	(car (cdr (cdr (cdr x)))))
(defun caddar (x)
	(car (cdr (cdr (car x)))))

(defun cdaadr (x)
	(cdr (car (car (cdr x)))))
(defun cdaaar (x)
	(cdr (car (car (car x)))))
(defun cdaddr (x)
	(cdr (car (cdr (cdr x)))))
(defun cdadar (x)
	(cdr (car (cdr (car x)))))
	
(defun cddadr (x)
	(cdr (cdr (car (cdr x)))))
(defun cddaar (x)
	(cdr (cdr (car (car x)))))
(defun cddddr (x)
	(cdr (cdr (cdr (cdr x)))))
(defun cdddar (x)
	(cdr (cdr (cdr (car x)))))

(defun second (x) (cadr x))
(defun third (x) (caddr x))
(defun fourth (x) (cadddr x))

(defconstant pi 3.141592653589793)

(defun member (a lat)
	(cond
		((null lat) nil)
		((eq (car lat) a) lat)
        (t (member a (cdr lat)))))

;(defun 1+ (x) (+ 1 x))
;(defun 1- (x) (- x 1))

(defun gcd (x y)
  (defun gcd (x y)
    (cond ((= 0 x) y)
          ((= 0 y) x)
          ((< x y) (gcd x (rem y x)))
          (else    (gcd y (rem x y)))))
    (gcd (abs x) (abs y)))

(defun lcm (x y)
  (let ((cd (gcd x y)))
    (abs (* cd (div x cd) (div y cd)))))

(defun nth-tail (n a)
  (if (= 0 n)
      a
      (nth-tail (- n 1) (cdr a))))

(defun nth (n a) (car (nth-tail n a)))

(defun list-length (x) 
	(length x))

(defun fold (f b a)
  (defun fl (a r)
    (if (null a)
        r
        (fl (cdr a)
            (funcall f r (car a)))))
  (fl a b))

(defmacro when (cond result) 
	`(if ,cond ,result)) 

(defmacro unless (cond result) 
	`(if (not ,cond) ,result)) 

(defun identity (x) x)
(defmacro constantly (x) `(lambda (&optional s) ,x))
(defmacro complement (x) `(lambda (&rest arguments) (not (apply ,x arguments))))

(defun copy-seq (s)
    (subseq s 0))


(defun every (fn seq)
    (cond
        ((eq (length seq) 0) t)
        ((funcall fn (elt seq 0)) (every fn (subseq seq 1)))
		(t nil)))

(defun notevery (fn seq)
	(not (every fn seq)))
		
(defun some (fn seq)
    (cond
        ((eq (length seq) 0) nil)
        ((funcall fn (elt seq 0)) ) ;; return the value
		(t (some fn (subseq seq 1)))))
		
(defun notany (fn seq)
		(not (some fn seq)))

(defun find (x seq)
    (cond
         ((eq (length seq) 0) nil)
         ((equal x (elt seq 0)) (elt seq 0))
		 (t (find x (subseq seq 1)))))

(defun find-if (fn seq)
    (cond
         ((eq (length seq) 0) nil)
         ((funcall fn (elt seq 0)) (elt seq 0))
		 (t (find-if fn (subseq seq 1)))))
		 
(defun find-if-not (fn seq)
	(cond
	  ((eq (length seq) 0) nil)
	  ((not (funcall fn (elt seq 0))) (elt seq 0))
	  (t (find-if-not fn (subseq seq 1)))))

(defun position (x seq)
  (defun my-pos (x seq c)
    (cond
         ((eq (length seq) 0) nil)
         ((equal x (elt seq 0)) c)
		 (t (my-pos x (subseq seq 1) (+ c 1)))))
  (my-pos x seq 0))

(defun position-if-support (fn seq count)
	(cond
		((null seq) nil)
		((funcall fn (elt seq 0)) count)
		(t (position-if-support fn (subseq seq 1) (1+ count)))))

(defun position-if (fn seq)
	(position-if-support fn seq 0))

;(defun position-if-not (fn seq)
;	(position-if-support (complement fn) seq 0))

(defun fill (seq x)
    (let ((s seq))
      (dotimes (n (length s))
        (setq s (set-elt s n x)))
      s))

)stdlib";

void Lisp::init()
{
    if (!opt.silent) {
        cout << "Hello C++ Lisp 👾 !" << endl;
    }
    init_prims();

    // Init the standard library
    istringstream in(stdlib);
    NullBuffer null_buffer;
    std::ostream out(&null_buffer);
    opt.push_options();
    opt.readline = false;
    opt.debug_expr = false;
    // repl(in, out);
    opt.pop_options();
};

void Lisp::repl(istream& istr, ostream& ostr)
{
    unique_ptr<LineReader> rl;
    if (opt.readline) {
        // rl = make_unique<LineReaderReadLine>();
        rl = make_unique<LineReaderReplxx>();
    } else {
        rl = make_unique<LineReaderStream>(istr);
    }
    Lexer lex(*rl);
    Parser parser(lex);
    Evaluator evaluator(opt);

    while (true) {
        ParserResult res;
        try {
            res = parser.parse();
            if (res.eof) {
                break;
            }
            if (opt.parse_only) {
                ostr << to_string(res.val) << endl;
                continue;
            }

            auto ex = evaluator.eval(res.val, symboltable);
            ostr << to_string(ex) << endl;

        } catch (UnknownToken& e) {
            ostr << "Unknown token: " << e.tok << endl;
            continue;
        } catch (ParseException& e) {
            ostr << "Parse error: " << e.what() << endl;
            continue;
        } catch (EndBracketException& e) {
            ostr << "Parse error: Extra bracket found" << endl;
            continue;
        } catch (EvalException& e) {
            ostr << "Eval error: " << e.what() << endl;
        } catch (NumericException& e) {
            ostr << "Numeric exception: " << e.what() << endl;
        } catch (RuntimeException& e) {
            ostr << "Runtime exception: " << e.what() << endl;
        } catch (ExceptionQuit& e) {
            break;
        } catch (exception& e) {
            ostr << "Exception: " << e.what() << endl;
            continue;
        } catch (...) {
            ostr << "Unknown exception!" << endl;
            continue;
        }
        if (res.eof) {
            break;
        }
    }
}

void Lisp::terminate()
{
    if (!opt.silent) {
        cout << endl
             << "Bye 👾" << endl;
    }
};
}