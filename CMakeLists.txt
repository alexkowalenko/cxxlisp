cmake_minimum_required(VERSION 3.0)
project (demo LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-Wall -Wextra)

find_package(Boost REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})


add_executable(cxxlisp 
    cmd/cxxlisp.cc)

target_link_libraries(cxxlisp 
    LINK_PUBLIC ${Boost_LIBRARIES} )