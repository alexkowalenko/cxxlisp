cmake_minimum_required(VERSION 3.0)
project (demo LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_compile_options(-Wall -Wextra)

enable_testing()

# Add custom Find cmake files
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Boost libraries
find_package(Boost 1.70 REQUIRED COMPONENTS program_options test_exec_monitor)
include_directories(${Boost_INCLUDE_DIRS})

# GNU Readline 
find_package(readline REQUIRED)
include_directories(${Readline_INCLUDE_DIR})

include_directories(include)

file(GLOB source_files RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} src/*.cc)

add_executable(cxxlisp 
    cmd/cxxlisp.cc
    ${source_files})

target_link_libraries(cxxlisp 
    LINK_PUBLIC ${Boost_LIBRARIES} ${Readline_LIBRARY})

file(GLOB test_src RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} test/*.cc)

foreach(testSrc ${test_src})
        #Extract the filename without an extension (NAME_WE)
        get_filename_component(testName ${testSrc} NAME_WE)

        #Add compile target
        add_executable(${testName} ${testSrc} ${source_files})

        #link to Boost libraries AND your targets and dependencies
        target_link_libraries(${testName} ${Boost_LIBRARIES} ${Readline_LIBRARY})

        #I like to move testing binaries into a testBin directory
        #set_target_properties(${testName} PROPERTIES 
        #    RUNTIME_OUTPUT_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}/testBin)

        #Finally add it to test execution - 
        #Notice the WORKING_DIRECTORY and COMMAND
        add_test(NAME ${testName} 
                 COMMAND ${testName} )
endforeach(testSrc)