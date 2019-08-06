//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#ifndef TEST_HH
#define TEST_HH

#include <string>
#include <vector>

using namespace std;

struct TestEval {
    string input;
    string output;
};

void test_Evaluator(const vector<TestEval>& tests);

#endif