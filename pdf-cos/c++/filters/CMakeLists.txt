add_library(pdffilters STATIC
  asciihex.cpp
  ascii85.cpp
  bitstream.cpp
  lzw.cpp
)

target_include_directories(pdffilters PUBLIC include)
