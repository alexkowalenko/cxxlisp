//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereaderReplxx.hh"

#include "exceptions.hh"

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <iostream>

#include <boost/format.hpp>
#include <boost/log/trivial.hpp>

#include "utf8.h"

namespace ax {

namespace {
    char const* prompt = "++> ";

    const string history_file = ".cxxlisp.replxx";
    const int max_history = 1000;
};

LineReaderReplxx::LineReaderReplxx()
{
    BOOST_LOG_TRIVIAL(info) << "Using Replxx!";
    struct passwd* pw = getpwuid(getuid());
    my_history_file = string(pw->pw_dir);
    my_history_file += "/" + history_file;
    auto res = replxx.history_load(my_history_file);
    if (res != 0) {
        cerr << "Can't read the history file: " << my_history_file
             << ' ' << strerror(res) << endl;
    }
    replxx.set_max_history_size(max_history);

    ptr = buf.end();
}

LineReaderReplxx::~LineReaderReplxx()
{
    auto res = replxx.history_save(my_history_file);
    if (res != 0) {
        cerr << "Can't write the history file: " << my_history_file
             << ' ' << strerror(res) << endl;
    }
}

uint32_t LineReaderReplxx::get_char()
{
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char" << boost::format("buf: %1% ptr : %2%") % buf % ptr;
    if (ptr == buf.end()) {
        get_line();
    }
    uint32_t c = utf8::next(ptr, buf.end());
    //BOOST_LOG_TRIVIAL(trace) << " : char " << c;
    return c;
}

uint32_t LineReaderReplxx::peek_char()
{
    if (ptr == buf.end()) {
        get_line();
    }
    return utf8::peek_next(ptr, buf.end());
}

void LineReaderReplxx::push_char(uint32_t)
{
    return;
}

void LineReaderReplxx::get_line()
{
    auto cbuf = replxx.input(prompt);
    if (cbuf == nullptr) {
        throw EOFException();
    }
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << cbuf;

    if (utf8::find_invalid(buf) != string::npos) {
        BOOST_LOG_TRIVIAL(trace) << "invalid str " << cbuf;
    }
    buf = string(cbuf);
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << buf;
    replxx.history_add(cbuf);

    buf.append(1, '\n');
    ptr = buf.begin();
}
}
