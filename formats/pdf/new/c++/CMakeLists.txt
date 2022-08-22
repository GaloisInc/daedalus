cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 20)

project(pdfcos
    VERSION 1
    DESCRIPTION "C++ PDF parser driver"
    LANGUAGES C CXX
)

find_package(PkgConfig REQUIRED)
pkg_check_modules(GMPXX REQUIRED IMPORTED_TARGET gmpxx)

find_package(ZLIB REQUIRED)

add_subdirectory(filters)
add_subdirectory(opensslxx)

add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.h
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/Daedalus.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfCos.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfDecl.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfXRef.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfValue.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/TextExtract.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/Catalog.ddl
  COMMAND cabal run exe:daedalus -v0 --
    compile-c++
    --inline-this=StandardEncodings.glyph
    --out-dir=${CMAKE_CURRENT_BINARY_DIR}
    --entry=PdfXRef.PdfEnd
    --entry=PdfXRef.CrossRef
    --entry=PdfDecl.TopDecl
    --entry=PdfDecl.ObjStream
    --entry=PdfDecl.ObjStreamEntry
    --entry=PdfCrypto.EncryptionDict
    --entry=TextExtract.TextInCatalog
    --entry=Catalog.PdfCatalog
    --entry=Validate.CheckRef
    --entry=StandardEncodings.StdEncodings
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfCos.ddl
  )

add_executable(parser-test
    src/args.cpp
    src/debug.cpp
    src/primitives.cpp
    src/main.cpp
    src/state.cpp
    src/encryption.cpp
    src/catalog.cpp
    src/glyphmap.c
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.cpp
)

target_include_directories(parser-test
    PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}/../../../../rts-c
)

target_link_libraries(parser-test PRIVATE PkgConfig::GMPXX ZLIB::ZLIB opensslxx pdffilters)

target_compile_options(parser-test PRIVATE -O3)
# target_compile_options(parser-test PRIVATE -fsanitize=address -g3)
# target_link_options(parser-test PRIVATE -fsanitize=address)

add_subdirectory(tests)
