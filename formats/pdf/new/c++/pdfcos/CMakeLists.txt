
find_package(PkgConfig REQUIRED)
pkg_check_modules(GMPXX REQUIRED IMPORTED_TARGET gmpxx)
find_package(ZLIB REQUIRED)

add_subdirectory(docs)

add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/include/pdfcos/types.h
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/Daedalus.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/PdfCos.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/PdfDecl.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/PdfXRef.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/PdfValue.ddl
  COMMAND cabal run exe:daedalus -v0 --
    compile-c++
    --add-include='<pdfcos/declare_state.hpp>'
    --user-state=ReferenceTable
    --out-dir=${CMAKE_CURRENT_BINARY_DIR}
    --out-dir-headers=include/pdfcos
    --file-root=types
    --user-namespace=PdfCos
    --entry=PdfXRef.PdfEnd
    --entry=PdfXRef.CrossRef
    --entry=PdfDecl.TopDecl
    --entry=PdfDecl.ObjStream
    --entry=PdfDecl.ObjStreamEntry
    --entry=PdfCrypto.EncryptionDict
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec/PdfCos.ddl
  )

add_library(pdfcos
  STATIC
    src/debug.cpp
    src/primitives.cpp
    src/state.cpp
    src/encryption.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/types.cpp
)


target_include_directories(pdfcos
  PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}/include
    include
)

target_include_directories(pdfcos
  PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}/include/pdfcos
)



target_link_libraries(pdfcos
  PRIVATE
    PkgConfig::GMPXX
    ZLIB::ZLIB
    opensslxx
    pdffilters
)
target_link_libraries(pdfcos
  PUBLIC
    ddl-rts
)

target_compile_options(pdfcos PRIVATE -O3)
