//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef TEST_HH
#define TEST_HH

#include <string>
#include <vector>

struct TestEval {
    TestEval(const std::string &i, const std::string &o) : input(i), output(o){};
    std::string input;
    std::string output;
};

void test_Evaluator(const std::vector<TestEval> &tests);

#endif