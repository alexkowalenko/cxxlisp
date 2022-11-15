//
// Cxx Scheme compiler
//
// Copyright © Alex Kowalenko 2020.
//

#include <sstream>

#include "exceptions.hh"
#include "lexer.hh"
#include "linereaderStream.hh"
#include "parser.hh"
#include "token.hh"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size) {
    std::string          s{reinterpret_cast<const char *>(data), size};
    std::istringstream   is(s);
    ax::LineReaderStream r(is);
    ax::Lexer            lex(r);
    ax::Parser           parser(lex);

    try {
        while (true) {
            auto [val, eof, _] = parser.parse();
            if (eof) {
                break;
            }
        }
    } catch (ax::UnknownToken &) {
    } catch (ax::EOFException &) {
    } catch (ax::ParseException &) {
    }
    return 0;
}