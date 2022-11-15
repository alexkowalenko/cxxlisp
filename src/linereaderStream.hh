//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include "linereader.hh"

#include <iostream>

namespace ax {

class LineReaderStream : public LineReader {
  public:
    LineReaderStream(std::istream &is);

    uint32_t get_char() override;
    uint32_t peek_char() override;
    void     push_char(uint32_t c) override;

  private:
    void get_line();

    std::istream &is;
};

} // namespace ax
