cmake_minimum_required(VERSION 3.12)

# Change compiler here
set(AX_BASE_CLANG /usr/local/opt/llvm/bin)
set(CMAKE_CXX_COMPILER ${AX_BASE_CLANG}/clang++)

project (cxxlisp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#add_compile_options(-Wall -Wextra -Wshadow -Wpedantic)
add_compile_options(-Wall -Wshadow)

enable_testing()


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

#Boehm Garbage collector
set(BOEHM_INCLUDE_DIRS /usr/local/opt/bdw-gc/include)
set(BOEHM_LIBRARY_DIRS /usr/local/opt/bdw-gc/lib)
set(BOEHM_LIBRARIES gc gccpp)
include_directories(${BOEHM_INCLUDE_DIRS})
link_directories(${BOEHM_LIBRARY_DIRS})


include_directories(include)

# Third party libraries local to this project

add_subdirectory(third_party/replxx)
include_directories("${PROJECT_SOURCE_DIR}/third_party/replxx/include")

# add_subdirectory(third_party/utfcpp)
include_directories("${PROJECT_SOURCE_DIR}/third_party/utfcpp/source")

set(ICU_INCLUDE_DIRS /usr/local/opt/icu4c/include)
set(ICU_LIBRARY_DIRS /usr/local/opt/icu4c/lib)
set(ICU_LIBRARIES icuuc)
include_directories(${ICU_INCLUDE_DIRS})
link_directories(${ICU_LIBRARY_DIRS})

set(GBEN_INCLUDE_DIRS /usr/local/opt/google-benchmark/include)
set(GBEN_LIBRARY_DIRS /usr/local/opt/google-benchmark/lib)
set(GBEN_LIBRARIES benchmark)
include_directories(${GBEN_INCLUDE_DIRS})
link_directories(${GBEN_LIBRARY_DIRS})

# CXX Lisp build

# options that work on macOs address, undefined
#add_compile_options (-fsanitize=undefined)
#set(CMAKE_EXE_LINKER_FLAGS -fsanitize=undefined)
#set(CMAKE_LINKER ${AX_BASE_CLANG}/clang++)

find_program(CLANG_TIDY_PROGRAM NAMES "clang-tidy" DOC ${AX_BASE_CLANG}/clang-tidy)
if (CLANG_TIDY_PROGRAM)
        set(CLANG_TIDY_OPTIONS "-warnings-as-errors=*")
        set(RUN_CLANG_TIDY "${CLANG_TIDY_PROGRAM}" "${CLANG_TIDY_OPTIONS}")
endif()

file(GLOB source_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} src/*.cc)
add_library(libcxxlisp STATIC ${source_files})

add_executable(cxxlisp cmd/cxxlisp.cc)
target_link_libraries(cxxlisp libcxxlisp replxx ${ICU_LIBRARIES} ${BOEHM_LIBRARIES})

if (CLANG_TIDY_PROGRAM)
        set_target_properties(cxxlisp PROPERTIES CXX_CLANG_TIDY "${RUN_CLANG_TIDY}")
endif()

# Test programs build

file(GLOB test_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/test_*.cc)

add_library(testlib test/utils.cc)

foreach(testSrc ${test_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(testName ${testSrc} NAME_WE)

        #Add compile target
        add_executable(${testName} ${testSrc})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${testName} libcxxlisp replxx testlib ${ICU_LIBRARIES} ${BOEHM_LIBRARIES})

        #I like to move testing binaries into a testBin directory
        #set_target_properties(${testName} PROPERTIES 
        #    RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}/testBin)

        #Finally add it to test execution - 
        #Notice the WORKING_DIRECTORY and COMMAND
        add_test(NAME ${testName} 
                 COMMAND ${testName} )
endforeach(testSrc)

# Benchmark programs

file(GLOB bench_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} benchmarks/bench_*.cc)

foreach(benSrc ${bench_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(benName ${benSrc} NAME_WE)

        #Add compile target
        add_executable(${benName} ${benSrc})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${benName} libcxxlisp replxx ${ICU_LIBRARIES} ${BOEHM_LIBRARIES} ${GBEN_LIBRARIES})

endforeach(benSrc)


