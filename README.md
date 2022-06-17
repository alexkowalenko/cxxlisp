# cxxlisp

Lisp in C++17

## Build

git submodule update --init --recursive

brew install boost
brew install boehmgc

mkdir build && cd build
cmake ..
make -j 4 install
ctest -j 4