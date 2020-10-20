# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE
      RelWithDebInfo
      CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui, ccmake
  set_property(
    CACHE CMAKE_BUILD_TYPE
    PROPERTY STRINGS
             "Debug"
             "Release"
             "MinSizeRel"
             "RelWithDebInfo")
endif()

# Generate compile_commands.json to make it easier to work with clang based tools
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(ENABLE_IPO "Enable Interprocedural Optimization, aka Link Time Optimization (LTO)" OFF)

if(ENABLE_IPO)
  include(CheckIPOSupported)
  check_ipo_supported(
    RESULT
    result
    OUTPUT
    output)
  if(result)
    set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
  else()
    message(SEND_ERROR "IPO is not supported: ${output}")
  endif()
endif()

# Add custom Find cmake files
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Boost libraries
find_package(Boost 1.70 REQUIRED 
             COMPONENTS log program_options unit_test_framework test_exec_monitor)
include_directories(${Boost_INCLUDE_DIRS})
link_libraries(${CMAKE_THREAD_LIBS_INIT} ${Boost_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${Boost_LIBRARIES} ${Boost_LIBRARIES})
add_definitions(-DBOOST_LOG_DYN_LINK) # to link the log library in boost

# GNU Readline 
find_package(Readline REQUIRED)
include_directories(${Readline_INCLUDE_DIR})
link_libraries(${Readline_LIBRARY})

#Boehm Garbage collector
set(BOEHM_INCLUDE_DIRS /usr/local/opt/bdw-gc/include)
set(BOEHM_LIBRARY_DIRS /usr/local/opt/bdw-gc/lib)
set(BOEHM_LIBRARIES gc gccpp)
include_directories(${BOEHM_INCLUDE_DIRS})
link_directories(${BOEHM_LIBRARY_DIRS})

#replxx
message(STATUS "Using replxx")
set(REPLXX_INCLUDE_DIRS "${PROJECT_SOURCE_DIR}/third_party/replxx/include")
set(REPLXX_LIBRARY_DIRS "${PROJECT_SOURCE_DIR}/third_party/replxx/lib")
set(REPLXX_LIBRARIES replxx)
message(STATUS "Using replxx ${REPLXX_LIBRARY_DIRS}")

set(REPLXX_BUILD_EXAMPLES OFF)

include_directories("${PROJECT_SOURCE_DIR}/third_party/utfcpp/source")

set(ICU_INCLUDE_DIRS /usr/local/opt/icu4c/include)
set(ICU_LIBRARY_DIRS /usr/local/opt/icu4c/lib)
set(ICU_LIBRARIES icuuc)
include_directories(${ICU_INCLUDE_DIRS})
link_directories(${ICU_LIBRARY_DIRS})
message(STATUS "Using International Components for Unicode: ${ICU_INCLUDE_DIRS}")

