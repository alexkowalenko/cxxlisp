# cxxlisp

Lisp, which was in C++17, but now trying C++23.

## Build

git submodule update --init --recursive

brew install boost
brew install boehmgc
pip3 colored

mkdir build && cd build
cmake ..
make -j 4 install
ctest -j 4