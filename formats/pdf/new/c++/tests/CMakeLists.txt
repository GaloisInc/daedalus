add_executable(bitstream-test bitstream_test.cpp)
target_link_libraries(bitstream-test PRIVATE pdffilters)


add_executable(pdfcos-test pdfcos-test.cpp)
target_link_libraries(pdfcos-test PRIVATE pdfcos)
