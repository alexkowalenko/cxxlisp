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

#define SPDLOG_ACTIVE_LEVEL SPDLOG_LEVEL_DEBUG
#include <spdlog/spdlog.h>

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wold-style-cast"
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wcast-align"
#include "utf8.h"
#pragma clang diagnostic pop

namespace ax {

namespace {
constexpr char const *prompt{"++> "};

const std::string history_file = ".cxxlisp.replxx";
constexpr int     max_history = 1000;
}; // namespace

LineReaderReplxx::LineReaderReplxx() {
    // BOOST_LOG_TRIVIAL(info) << "Using Replxx!";
    struct passwd *pw = getpwuid(getuid());
    my_history_file = std::string(pw->pw_dir);
    my_history_file += "/" + history_file;
    replxx.history_load(my_history_file);
    replxx.set_max_history_size(max_history);

    ptr = buf.end();
}

LineReaderReplxx::~LineReaderReplxx() {
    replxx.history_save(my_history_file);
}

uint32_t LineReaderReplxx::get_char() {
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char" << boost::format("buf: %1% ptr : %2%") %
    // buf % ptr;
    if (ptr == buf.end()) {
        get_line();
    }
    uint32_t c = utf8::next(ptr, buf.end());
    // BOOST_LOG_TRIVIAL(trace) << " : char " << c;
    return c;
}

uint32_t LineReaderReplxx::peek_char() {
    if (ptr == buf.end()) {
        get_line();
    }
    return utf8::peek_next(ptr, buf.end());
}

void LineReaderReplxx::push_char(uint32_t) {
    return;
}

void LineReaderReplxx::get_line() {
    auto cbuf = replxx.input(prompt);
    if (cbuf == nullptr) {
        throw EOFException();
    }
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << cbuf;

    if (utf8::find_invalid(buf) != std::string::npos) {
        // SPDLOG_TRACE("invalid str {}", cbuf);
    }
    buf = std::string(cbuf);
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << buf;
    replxx.history_add(cbuf);

    buf.append(1, '\n');
    ptr = buf.begin();
}
} // namespace ax
