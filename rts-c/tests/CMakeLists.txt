cmake_minimum_required(VERSION 3.14)
set(CMAKE_CXX_STANDARD 20)
project(rts-c-tests
    LANGUAGES CXX
)

enable_testing()
find_package(GTest REQUIRED IMPORTED)

add_executable(rts-c-tests main.cpp
    array_tests.cpp
    maybe_tests.cpp
    map_tests.cpp
    bool_tests.cpp
    float_tests.cpp
    )

target_link_libraries(rts-c-tests GTest::GTest GTest::Main)
target_include_directories(rts-c-tests SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/..)
gtest_discover_tests(rts-c-tests)
