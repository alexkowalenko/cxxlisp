//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <sstream>
#include <string>

#include <benchmark/benchmark.h>

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <spdlog/spdlog.h>

#include "exceptions.hh"
#include "linereaderStream.hh"
#include "lisp.hh"

using namespace ax;
using namespace std;

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

    spdlog::set_level(spdlog::level::info); // Set global log level to info 
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