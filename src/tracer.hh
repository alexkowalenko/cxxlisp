//
// Common Lisp in C++17
//
// Copyright © Alex Kowalenko 2019.
//

#pragma once

#include <string>

namespace ax {

class Tracer {
  public:
    Tracer(const std::string &f, const std::string &args);
    ~Tracer();

    static unsigned int level;

  private:
    const std::string &name;
};

class TracerGuard {
  public:
    TracerGuard() : trace(nullptr){};
    ~TracerGuard();

    void add_trace(Tracer *t);

  private:
    Tracer *trace;
};

} // namespace ax