find_package(Doxygen)

if (DOXYGEN_FOUND)
  set(DOXYGEN_GENERATE_HTML YES)

  doxygen_add_docs(rts-ddl-docs
    ${CMAKE_CURRENT_SOURCE_DIR}/../ddl
  )
endif (DOXYGEN_FOUND)
