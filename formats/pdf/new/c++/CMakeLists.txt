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
add_subdirectory(rts-c)
add_subdirectory(pdfcos)
add_subdirectory(pdf-validate)
add_subdirectory(tests)

add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.h
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/Daedalus.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfAll.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfDecl.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfXRef.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfValue.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/TextExtract.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/Catalog.ddl
  COMMAND cabal run exe:daedalus -v0 --
    compile-c++
    --add-include='<declare_state.hpp>'
    --user-state=ReferenceTable
    --inline-this=StandardEncodings.glyph
    --out-dir=${CMAKE_CURRENT_BINARY_DIR}
    --user-namespace=PdfCos
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
    ${CMAKE_CURRENT_SOURCE_DIR}/../spec/PdfAll.ddl
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
    src
    ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries(parser-test
  PRIVATE
    PkgConfig::GMPXX
    ZLIB::ZLIB
    opensslxx
    pdffilters
    ddl-rts
)

target_compile_options(parser-test PRIVATE -O3)
# target_compile_options(parser-test PRIVATE -fsanitize=address -g3)
# target_link_options(parser-test PRIVATE -fsanitize=address)

