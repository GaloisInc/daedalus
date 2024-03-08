cmake_minimum_required(VERSION 3.14)
set(CMAKE_CXX_STANDARD 20)
project(rts-c-tests
    LANGUAGES CXX
)

enable_testing()
find_package(GTest REQUIRED)
find_package(PkgConfig)
pkg_check_modules(GMPXX REQUIRED IMPORTED_TARGET gmpxx)
find_package(Boost REQUIRED COMPONENTS context)

add_executable(rts-c-tests main.cpp
    array_tests.cpp
    bool_tests.cpp
    float_tests.cpp
    integer_tests.cpp
    map_tests.cpp
    maybe_tests.cpp
    number_tests.cpp
    size_tests.cpp
    unit_tests.cpp
    stream_tests.cpp
    )

target_include_directories(rts-c-tests SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(rts-c-tests ${Boost_LIBRARIES})


target_link_libraries(rts-c-tests GTest::GTest GTest::Main PkgConfig::GMPXX)

target_include_directories(rts-c-tests SYSTEM PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/..)
gtest_discover_tests(rts-c-tests)
