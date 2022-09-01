
add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/main_parser.h
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec/Daedalus.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec/PdfAll.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec/TextExtract.ddl
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec/Catalog.ddl
  COMMAND cabal run exe:daedalus -v0 --
    --path=${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-cos-spec
    --path=${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec
    compile-c++
    --add-include='<pdfcos.hpp>'
    --user-state=ReferenceTable
    --inline-this=StandardEncodings.glyph
    --out-dir=${CMAKE_CURRENT_BINARY_DIR}
    --user-namespace=PdfDriver
    --entry=TextExtract.TextInCatalog
    --entry=Catalog.PdfCatalog
    --entry=Validate.CheckRef
    --entry=StandardEncodings.StdEncodings
    --extern=PdfValue:PdfCos
    --extern=PdfDecl:PdfCos
    --extern=PdfXRef:PdfCos
    --extern=PdfCryptol:PdfCos
    ${CMAKE_CURRENT_SOURCE_DIR}/../../pdf-validate-spec/PdfAll.ddl
  )

add_executable(parser-test
    src/args.cpp
    src/debug.cpp
    src/primitives.cpp
    src/main.cpp
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
    ddl-rts
    pdfcos
)

target_compile_options(parser-test PRIVATE -O3)
# target_compile_options(parser-test PRIVATE -fsanitize=address -g3)
# target_link_options(parser-test PRIVATE -fsanitize=address)

