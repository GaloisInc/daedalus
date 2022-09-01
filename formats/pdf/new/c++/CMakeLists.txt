cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 20)

project(pdfcos
    VERSION 1
    DESCRIPTION "C++ PDF parser driver"
    LANGUAGES C CXX
)

find_package(PkgConfig REQUIRED)

add_subdirectory(filters)
add_subdirectory(opensslxx)
add_subdirectory(rts-c)
add_subdirectory(pdfcos)
add_subdirectory(pdf-driver)
add_subdirectory(tests)


