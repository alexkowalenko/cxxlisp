//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include <gtest/gtest.h>

#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "linereaderStream.hh"

struct TestLineReader {
    char want;
    bool eof;
};

void test_lineReader(const std::vector<TestLineReader> &tests, ax::LineReaderStream &r) {
    for (auto t : tests) {
        try {
            auto got = r.get_char();
            std::cout << "got: " << char(got) << " want: " << t.want << std::endl;
            EXPECT_EQ(got, t.want);
        } catch (ax::EOFException) {
            if (!t.eof) {
                std::cout << "No eof at end of file :" << t.want << std::endl;
                FAIL();
            }
        }
    }
}

TEST(linereader, 1) {
    std::vector<TestLineReader> tests = {
        {'a', false}, {'b', false}, {'c', false}, {10, false},
        {'d', false}, {'e', false}, {'f', false}, {10, true},
    };

    std::istringstream   is("abc\ndef");
    ax::LineReaderStream r(is);
    test_lineReader(tests, r);
}

TEST(linereader, 2) {
    std::vector<TestLineReader> tests = {
        {0, true},
    };

    std::istringstream   is("");
    ax::LineReaderStream r(is);
    test_lineReader(tests, r);
}

TEST(linereader, 3) {
    std::vector<TestLineReader> tests = {
        {'b', false},
    };

    std::istringstream   is("abc\ndef");
    ax::LineReaderStream r(is);

    auto c = r.get_char();
    EXPECT_EQ(c, 'a');
    test_lineReader(tests, r);
}

TEST(linereader, push_char) {
    std::vector<TestLineReader> tests = {
        {'a', false},
        {'b', false},
        {'c', false},
    };

    std::istringstream   is("abc\ndef");
    ax::LineReaderStream r(is);

    auto c = r.get_char();
    EXPECT_EQ(c, 'a');
    r.push_char(c);
    test_lineReader(tests, r);
}

TEST(linereader, peek_char) {
    std::vector<TestLineReader> tests = {
        {'a', false},
        {'b', false},
        {'c', false},
    };

    std::istringstream   is("abc\ndef");
    ax::LineReaderStream r(is);

    auto c = r.peek_char();
    EXPECT_EQ(c, 'a');
    test_lineReader(tests, r);
}
