//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <gtest/gtest.h>
#include <sstream>

#include "token.hh"

using namespace ax;

TEST(token, out) {
    // Test Atoms
    Token             a(TokenType::open);
    std::stringstream ss;
    ss << a;
    EXPECT_EQ(ss.str(), "(");

    a = Token(TokenType::atom, "hello");
    (ss = std::stringstream()) << a;
    EXPECT_EQ(ss.str(), "hello");
}