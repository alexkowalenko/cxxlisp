
include_directories(${PROJECT_SOURCE_DIR}/src)

add_executable(cxxlisp cxxlisp.cc)
target_link_libraries(cxxlisp PRIVATE project_options project_warnings
                              cxxlispl replxx ${ICU_LIBRARIES})

install(TARGETS cxxlisp DESTINATION bin)