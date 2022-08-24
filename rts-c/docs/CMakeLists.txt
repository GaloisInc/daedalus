find_package(Doxygen REQUIRED doxygen)

set(DOXYGEN_GENERATE_HTML YES)

doxygen_add_docs(rts-ddl-docs
  ${CMAKE_CURRENT_SOURCE_DIR}/../ddl
  ALL
)
