
add_custom_command(
  OUTPUT
    ${CMAKE_CURRENT_BINARY_DIR}/pdf-validate.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/pdf-validate.h
  DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/validate.ddl
  COMMAND cabal run exe:daedalus --
    --path=${CMAKE_CURRENT_SOURCE_DIR}/../../spec
    compile-c++
    ${CMAKE_CURRENT_SOURCE_DIR}/validate.ddl
    --file-root=pdf-validate
    --add-include='<pdfcos.hpp>'
    --user-state=ReferenceTable
    --out-dir=${CMAKE_CURRENT_BINARY_DIR}
    --user-namespace=PdfValidate
    --entry=Validate
    --extern=PdfValue:PdfCos
    --extern=PdfDecl:PdfCos
    --extern=PdfXRef:PdfCos
    --extern=PdfCrypto:PdfCos
    --extern=JpegBasics:PdfCos
  )

add_executable(pdf-validate
  main.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/pdf-validate.cpp
)

target_include_directories(pdf-validate
  PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries(pdf-validate PRIVATE pdfcos)
target_compile_options(pdf-validate PRIVATE -O3)
