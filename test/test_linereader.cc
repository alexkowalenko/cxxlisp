//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#define BOOST_TEST_MODULE test_linereader
#include <boost/test/unit_test.hpp>
#include <sstream>
#include <vector>

#include "exceptions.hh"
#include "linereaderStream.hh"

using namespace ax;
using namespace std;

struct TestLineReader {
    char want;
    bool eof;
};

void test_lineReader(const vector<TestLineReader>& tests, LineReaderStream& r)
{
    for (auto t : tests) {
        try {
            auto got = r.get_char();
            // cout << "got: " << char(got) << " want: " << t.want << endl;
            BOOST_REQUIRE_EQUAL(got, t.want);
        } catch (EOFException) {
            if (!t.eof) {
                BOOST_FAIL("No eof at end of file");
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(test_linereader_1)
{
    vector<TestLineReader> tests = {
        { 'a', false },
        { 'b', false },
        { 'c', false },
        { 'd', false },
        { 'e', false },
        { 'f', false },
        { 0, true },
    };

    istringstream is(string("abc\ndef"));
    LineReaderStream r = LineReaderStream(is);
    test_lineReader(tests, r);
}

BOOST_AUTO_TEST_CASE(test_linereader_2)
{
    vector<TestLineReader> tests = {
        { 0, true },
    };

    istringstream is(string(""));
    LineReaderStream r = LineReaderStream(is);
    test_lineReader(tests, r);
}

BOOST_AUTO_TEST_CASE(test_linereader_3)
{
    vector<TestLineReader> tests = {
        { 'b', false },
    };

    istringstream is(string("abc\ndef"));
    LineReaderStream r = LineReaderStream(is);

    auto c = r.get_char();
    BOOST_REQUIRE_EQUAL(c, 'a');
    test_lineReader(tests, r);
}

BOOST_AUTO_TEST_CASE(test_linereader_push_char)
{
    vector<TestLineReader> tests = {
        { 'a', false },
        { 'b', false },
        { 'c', false },
    };

    istringstream is(string("abc\ndef"));
    LineReaderStream r = LineReaderStream(is);

    auto c = r.get_char();
    BOOST_REQUIRE_EQUAL(c, 'a');
    r.push_char(c);
    test_lineReader(tests, r);
}

BOOST_AUTO_TEST_CASE(test_linereader_peek_char)
{
    vector<TestLineReader> tests = {
        { 'a', false },
        { 'b', false },
        { 'c', false },
    };

    istringstream is(string("abc\ndef"));
    LineReaderStream r = LineReaderStream(is);

    auto c = r.peek_char();
    BOOST_REQUIRE_EQUAL(c, 'a');
    test_lineReader(tests, r);
}
