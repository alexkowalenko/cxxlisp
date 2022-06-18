//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "test.hh"

#include <gtest/gtest.h>

TEST(eval, vector) {
    std::vector<TestEval> tests = {
        {"#()", "#()"},
        {"#(a )", "#(a)"},
        {"#(a 1 \"2\" (3 s f) #\\4 nil)", "#(a 1 \"2\" (3 s f) #\\4 nil)"},
        {"#(a 1 \"2\" (3 s f) #\\4 nil #(1 2 3))", "#(a 1 \"2\" (3 s f) #\\4 nil #(1 2 3))"},
        {"#(#(#(1)))", "#(#(#(1)))"},
        {"#(\"one two three\" \"ἑν δύο τρεῖς\" \"один два три\" \"一二三四五六七\" "
         "\"🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒\")",
         "#(\"one two three\" \"ἑν δύο τρεῖς\" \"один два три\" \"一二三四五六七\" "
         "\"🍏🍎🍐🍊🍋🍌🍉🍇🍓🍈🍒\")"},
    };
    test_Evaluator(tests);
}