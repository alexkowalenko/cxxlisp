//
// AX compiler
//
// Copyright © 2020 Alex Kowalenko
//

#include <sstream>

#include "exceptions.hh"
#include "lexer.hh"
#include "linereaderStream.hh"
#include "token.hh"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    std::string          s{reinterpret_cast<const char *>(data), size};
    std::istringstream   is(s);
    ax::LineReaderStream r(is);
    ax::Lexer            lex(r);

    try {
        ax::Token token = lex.get_token();
        while (token.type != ax::TokenType::eof) {
            token = lex.get_token();
        }
    } catch (ax::UnknownToken &) {
    }

    return 0;
}