cmake_minimum_required(VERSION 3.12)

# Change compiler here
set(AX_BASE_CLANG /usr/local/Cellar/llvm/8.0.0_1/bin)
set(CMAKE_CXX_COMPILER ${AX_BASE_CLANG}/clang++)

project (cxxlisp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-Wall)

enable_testing()

#set(CMAKE_CXX_CLANG_TIDY "/Users/alex/Development/lisp-proj/cxxlisp/build/clang-tidy;-checks=*")

# Add custom Find cmake files
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Boost libraries
find_package(Boost 1.70 REQUIRED 
             COMPONENTS log program_options test_exec_monitor)
include_directories(${Boost_INCLUDE_DIRS})
link_libraries(${CMAKE_THREAD_LIBS_INIT} ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${Boost_LIBRARIES} ${Boost_LIBRARIES})
add_definitions(-DBOOST_LOG_DYN_LINK) # to link the log library in boost

# GNU Readline 
find_package(readline REQUIRED)
include_directories(${Readline_INCLUDE_DIR})
link_libraries(${Readline_LIBRARY})

include_directories(include)

# Third party libraries local to this project

add_subdirectory(third_party/replxx)
include_directories("${PROJECT_SOURCE_DIR}/third_party/replxx/include")

# CXX Lisp build

file(GLOB source_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} src/*.cc)
add_library(libcxxlisp STATIC ${source_files})

add_executable(cxxlisp cmd/cxxlisp.cc)
target_link_libraries(cxxlisp libcxxlisp replxx)

# Test programs build

file(GLOB test_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/*.cc)

foreach(testSrc ${test_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(testName ${testSrc} NAME_WE)

        #Add compile target
        add_executable(${testName} ${testSrc})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${testName} libcxxlisp replxx)

        #I like to move testing binaries into a testBin directory
        #set_target_properties(${testName} PROPERTIES 
        #    RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}/testBin)

        #Finally add it to test execution - 
        #Notice the WORKING_DIRECTORY and COMMAND
        add_test(NAME ${testName} 
                 COMMAND ${testName} )
endforeach(testSrc)