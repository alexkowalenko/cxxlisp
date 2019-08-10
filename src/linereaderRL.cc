//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "linereaderRL.hh"

#include "exceptions.hh"

#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

#include <iostream>

#include <boost/format.hpp>
#include <boost/log/trivial.hpp>
#include <readline/readline.h>

namespace ax {

namespace {
    char const* prompt = "++> ";

    const string history_file = ".cxxlisp";
    const int max_history = 1000;
};

LineReaderReadLine::LineReaderReadLine()
{
    using_history();

    struct passwd* pw = getpwuid(getuid());
    my_history_file = string(pw->pw_dir);
    my_history_file += "/" + history_file;
    auto res = read_history(my_history_file.c_str());
    if (res != 0) {
        cerr << "Can't read the history file: " << my_history_file
             << ' ' << strerror(res) << endl;
    }
    ptr = buf.end();
}

LineReaderReadLine::~LineReaderReadLine()
{
    auto res = write_history(my_history_file.c_str());
    if (res != 0) {
        cerr << "Can't write the history file: " << my_history_file
             << ' ' << strerror(res) << endl;
    }
    res = history_truncate_file(my_history_file.c_str(), max_history);
    if (res != 0) {
        cerr << "Can't truncate the history file: " << history_file << ' ' << strerror(res) << endl;
    }
}

uint32_t LineReaderReadLine::get_char()
{
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_char" << boost::format("buf: %1% ptr : %2%") % buf % ptr;
    if (ptr == buf.end()) {
        get_line();
    }
    // BOOST_LOG_TRIVIAL(trace) << "buf: " << buf;
    return *ptr++;
}

uint32_t LineReaderReadLine::peek_char()
{
    if (ptr == buf.end()) {
        get_line();
    }
    return *ptr;
}

void LineReaderReadLine::push_char(uint32_t)
{
    return;
}

void LineReaderReadLine::get_line()
{
    auto cbuf = readline(prompt);
    if (cbuf == nullptr) {
        throw EOFException();
    }
    add_history(cbuf);
    // BOOST_LOG_TRIVIAL(trace) << "LineReader::get_line: " << cbuf;
    buf = string(cbuf);
    buf.append(1, '\n');
    ptr = buf.begin();
    free(cbuf);
}
}
