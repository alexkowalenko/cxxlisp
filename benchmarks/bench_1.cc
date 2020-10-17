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
    int overflow(int) { return 0; }
};

static void BM_Atom(benchmark::State &state) {
    // Perform setup here

    Options options;
    options.silent = true;
    options.readline = false;
    options.debug_expr = false;

    logging::core::get()->set_filter(logging::trivial::severity >= logging::trivial::info);
    Lisp lisp(options);
    lisp.init();

    string input{"(atom 's)"};

    istringstream is(input);
    NullBuffer    null_buffer;
    ostream       null_stream(&null_buffer);

    for (auto _ : state) {
        // This code gets timed
        lisp.repl(is, null_stream);
    }
}

// Register the function as a benchmark
BENCHMARK(BM_Atom);

// Run the benchmark
BENCHMARK_MAIN();