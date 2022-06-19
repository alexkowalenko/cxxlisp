
include_directories(../include)

add_executable(cxxlisp cxxlisp.cc)
target_link_libraries(cxxlisp PRIVATE project_options project_warnings
                              cxxlispl ${ICU_LIBRARIES} ${BOEHM_LIBRARIES} ${REPLXX_LIBRARIES})

install(TARGETS cxxlisp DESTINATION bin)