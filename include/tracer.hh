//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <string>

namespace ax {

using namespace std;

class Tracer {
public:
    Tracer(const string& f, const string& args);
    ~Tracer();

    static unsigned int level;

private:
    const string& name;
};

class TracerGuard {
public:
    TracerGuard()
        : trace(nullptr){};
    ~TracerGuard();

    void add_trace(Tracer* t);

private:
    Tracer* trace;
};
}