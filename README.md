# cxxlisp

Lisp, which was in C++17, but now trying C++23.

## Build

git submodule update --init --recursive

```sh
brew install boost
brew install boehmgc
pip3 colored
```

```sh
mkdir build && cd build
cmake ..
make -j install
ctest
```