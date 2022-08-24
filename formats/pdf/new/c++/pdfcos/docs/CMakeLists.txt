find_package(Doxygen REQUIRED doxygen)

set(DOXYGEN_GENERATE_HTML YES)

doxygen_add_docs(pdfcos-docs
  ${CMAKE_CURRENT_SOURCE_DIR}/../../../../../../rts-c/ddl
  ${CMAKE_CURRENT_SOURCE_DIR}/../src
  ${CMAKE_CURRENT_SOURCE_DIR}/../include
  ${CMAKE_CURRENT_SOURCE_DIR}/../include/pdfcos
  ${CMAKE_CURRENT_BINARY_DIR}/../include/pdfcos
  ALL
)
