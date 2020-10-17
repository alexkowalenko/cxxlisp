//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "tracer.hh"

#include <iostream>

namespace ax {

Tracer::Tracer(const std::string &f, const std::string &args) : name(f) {
    std::cerr << std::string(level * 2, ' ') << "ENTERING: " << name << ": " << args << std::endl;
    level++;
}

Tracer::~Tracer() {
    level--;
    std::cerr << std::string(level * 2, ' ') << "EXITING: " << name << std::endl;
};

unsigned int Tracer::level = 0;

TracerGuard::~TracerGuard() {
    if (trace) {
        delete trace;
    }
}

void TracerGuard::add_trace(Tracer *t) {
    trace = t;
}

} // namespace ax
