//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#include "tracer.hh"

#include <iostream>

namespace ax {

using namespace std;

Tracer::Tracer(const string& f, const string& args)
    : name(f)
{
    cerr << string(level * 2, ' ') << "ENTERING: " << name << ": " << args << endl;
    level++;
}

Tracer::~Tracer()
{
    level--;
    cerr << string(level * 2, ' ') << "EXITING: " << name << endl;
};

unsigned int Tracer::level = 0;

TracerGuard::~TracerGuard()
{
    if (trace) {
        delete trace;
    }
}

void TracerGuard::add_trace(Tracer* t)
{
    trace = t;
}
}
