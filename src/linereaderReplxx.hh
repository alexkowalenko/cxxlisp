//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "linereader.hh"

#include <replxx.hxx>
#include <string>

namespace ax {

using Replxx = replxx::Replxx;

class LineReaderReplxx : public LineReader {
  public:
    LineReaderReplxx();
    ~LineReaderReplxx();

    uint32_t get_char() override;
    uint32_t peek_char() override;
    void     push_char(uint32_t c) override;

  private:
    void get_line();

    Replxx      replxx;
    std::string my_history_file;
};

} // namespace ax
