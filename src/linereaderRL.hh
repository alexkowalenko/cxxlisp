//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <string>

#include "linereader.hh"

namespace ax {

class LineReaderReadLine : public LineReader {
  public:
    LineReaderReadLine();
    ~LineReaderReadLine();

    uint32_t get_char() override;
    uint32_t peek_char() override;
    void     push_char(uint32_t c) override;

  private:
    void get_line();

    std::string my_history_file;
};

} // namespace ax
