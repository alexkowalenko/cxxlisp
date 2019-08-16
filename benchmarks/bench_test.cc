//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <sstream>
#include <string>

#include <benchmark/benchmark.h>
#include <boost/log/trivial.hpp>
#include <boost/log/utility/setup.hpp>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

using namespace ax;
using namespace std;
namespace logging = boost::log;

class NullBuffer : public std::streambuf {
public:
    int overflow(int c) { return c; }
};

template <class... ExtraArgs>
static void BM_Test(benchmark::State& state, ExtraArgs&&... extra_args)
{
    // Perform setup here

    Options options;
    options.silent = true;
    options.readline = false;
    options.debug_expr = false;

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::info);
    Lisp lisp(options);
    lisp.init();

    istringstream is(extra_args...);
    NullBuffer null_buffer;
    ostream null_stream(&null_buffer);

    for (auto _ : state) {
        // This code gets timed
        lisp.repl(is, null_stream);
    }
}

BENCHMARK_CAPTURE(BM_Test, atom, std::string("(atom 's)"));

BENCHMARK_CAPTURE(BM_Test, plus, std::string("(+ 1 1)"));

BENCHMARK_CAPTURE(BM_Test, defconstant, std::string("(defconstant a 10)"));

BENCHMARK_CAPTURE(BM_Test, fact_20, std::string("(defun fact (x) (if (<= x 0) 1 (* x (fact (- x 1))))) (fact 20)"));

BENCHMARK_CAPTURE(BM_Test, fib_28, std::string(R"x( 
    (defun fib (n)
	 		(if (<= n 1)
	 			1
	 			(+ (fib (- n 1)) (fib (- n 2)))))
	(fib 28) )x"));

BENCHMARK_CAPTURE(BM_Test, subst, std::string(R"x( 
    (defun subst (x y z)
	(cond 
		((atom z)
			(cond ((eq z y) x)
					(t z)))
	(t (cons (subst x y (car z))
			 (subst x y (cdr z))))))
	(subst 'm 'b '(a b (a b (a b c) c) d a b (a b (a b c) c) c)) )x"));

BENCHMARK_CAPTURE(BM_Test, tak, std::string(R"x( 
    (defun tak (x y z)
		(if (not (< y x))
			z
			(tak (tak (- x 1) y z)
				 (tak (- y 1) z x)
				 (tak (- z 1) x y))))
	(tak 18 12 6) )x"));

BENCHMARK_CAPTURE(BM_Test, change, std::string(R"x( 
    (defun firstdenomination (kindsofcoins)
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
		(cc amount 5))
	(countchange 106) )x"));

// Run the benchmark
BENCHMARK_MAIN();