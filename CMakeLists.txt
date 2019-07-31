cmake_minimum_required(VERSION 3.0)
project (demo LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-Wall -Wextra)

find_package(Boost 1.70 REQUIRED COMPONENTS program_options)
include_directories(${Boost_INCLUDE_DIRS})

include_directories(include)

file(GLOB source_files "src/*.cc")

add_executable(cxxlisp 
    cmd/cxxlisp.cc
    ${source_files})

target_link_libraries(cxxlisp 
    LINK_PUBLIC ${Boost_LIBRARIES} )